//! Implements deep equality for types.
//!
//! # Algorithm

use std::{cell::RefCell, hash::Hash};

use rustc_hash::FxHashSet;

use crate::id::{RecordId, Tag, UnionId};

use super::{type_ctx::TypeCtxImmut, CtxBox, Record, RecordHandle, Type, TypeCtx, UnionHandle};

impl<'ctx, T: Tag> Type<'ctx, T> {
    fn deep_eq(&self, other: &Self) -> bool {
        let mut resolver = EqualityResolver::new();

        if resolver.are_not_equal(self, other) {
            return false;
        }

        true
    }
}

/// Determines if two types are definitely equal.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum MaybeEqual {
    /// [`MaybeEqual::No`] represents two types definitely not being equal.
    No = 0,
    /// [`MaybeEqual::Yes`] represents two types definitely being equal.
    Yes = 1,
    /// [`MaybeEqual::Maybe`] represents two types possibly being equal. To
    /// determine equality, it would require more in-depth comparisons.
    Maybe = 2,
}

impl From<bool> for MaybeEqual {
    fn from(value: bool) -> Self {
        match value {
            true => MaybeEqual::Yes,
            false => MaybeEqual::No,
        }
    }
}

/// Datastructure that maintains state during equality comparisons between
/// types.
pub struct EqualityResolver<'ctx1, 'ctx2, T: Tag> {
    record_constraints: Constraints<RecordHandle<'ctx1, T>, RecordHandle<'ctx2, T>>,
    union_constraints: Constraints<UnionHandle<'ctx1, T>, UnionHandle<'ctx2, T>>,
}

impl<'ctx1, 'ctx2, T: Tag> EqualityResolver<'ctx1, 'ctx2, T> {
    pub fn new() -> Self {
        Self {
            record_constraints: Default::default(),
            union_constraints: Default::default(),
        }
    }

    /// Solves all constraints until there are none less. If any types within
    /// the constraints are not equal, this will return `true`. If all types are
    /// not not-equal (meaning that they are either "probably equal" or "are
    /// equal"), this will return `false`.
    pub fn solve_constraints(&mut self) -> bool {
        while self.has_constraints_to_verify() {
            while let Some((a, b)) = self.record_constraints.next() {
                let a = a.borrow();
                let b = b.borrow();

                if a.len() != b.len() {
                    return false;
                }

                for (k, value_a) in a.iter() {
                    let value_b = match b.get(&match k {
                        Type::Any => Type::Any,
                        Type::Nothing => Type::Nothing,
                        Type::Bytes => Type::Bytes,
                        Type::Number => Type::Number,
                        Type::Boolean => Type::Boolean,
                        Type::Atom(x) => Type::Atom(*x),
                        Type::Int(x) => Type::Int(*x),
                        Type::Float(x) => Type::Float(*x),
                        Type::Bool(x) => Type::Bool(*x),
                        Type::FnPtr(x) => Type::FnPtr(*x),
                        Type::Byts(_) => todo!(),
                        Type::List(_) => todo!(),
                        Type::Record(_) => todo!(),
                        Type::Union(_) => todo!(),
                    }) {
                        Some(v) => v,
                        None => return false,
                    };

                    if self.are_not_equal(value_a, value_b) {
                        return false;
                    }
                }
            }

            while let Some((a, b)) = self.union_constraints.next() {
                todo!()
            }
        }

        false
    }

    pub fn has_constraints_to_verify(&self) -> bool {
        self.record_constraints.any() || self.union_constraints.any()
    }

    /// Tries to determine if two types are definitely not equal.
    ///
    /// If all types are not not equal, then it can be said that all types are
    /// equal.
    pub fn are_not_equal(&mut self, a: &Type<'ctx1, T>, b: &Type<'ctx2, T>) -> bool {
        use Type::*;

        let maybe_equal = match (a, b) {
            // records and unions need more comparisons
            (Record(r1), Record(r2)) => {
                self.record_constraints.assume_equal(*r1, *r2);
                MaybeEqual::Maybe
            }
            (Union(u1), Union(u2)) => {
                self.union_constraints.assume_equal(*u1, *u2);
                MaybeEqual::Maybe
            }
            (Any, Any) | (Bytes, Bytes) | (Number, Number) | (Boolean, Boolean) => MaybeEqual::Yes,
            (Atom(a), Atom(b)) => MaybeEqual::from(a == b),
            (Int(a), Int(b)) => MaybeEqual::from(a == b),
            (Float(a), Float(b)) => MaybeEqual::from(a == b),
            (Bool(a), Bool(b)) => MaybeEqual::from(a == b),
            (FnPtr(a), FnPtr(b)) => MaybeEqual::from(a == b),
            (Byts(a), Byts(b)) => {
                let a: &[u8] = a;
                let b: &[u8] = b;
                MaybeEqual::from(a == b)
            }
            _ => MaybeEqual::No,
        };

        MaybeEqual::No == maybe_equal
    }
}

/// Datastructure used to enforce constraints between IDs.
///
/// When checking for equality, it is impossible to know if two types are
/// exactly equal. However, we can assume that the types are equal, and then
/// perform deep equality on those types to ensure that they are equal.
///
/// This datastructure provides methods to assume two types are equal, as well
/// as the facilities to get the next pair of things that needs to be deeply
/// compared for equality.
///
/// Once all is said and done, if there were no definitely false comparisons, we
/// can safely assume that all things that were probably equal to one another,
/// were indeed equal to one another.
struct Constraints<T1, T2> {
    /// List of elements that are assumed to be equal.
    equals: FxHashSet<(T1, T2)>,
    /// List of elements to check for deep equality.
    ///
    /// The order in which deep equality is checked for doesn't matter, so we
    /// just push and pop from a Vec for maximum performance.
    check: Vec<(T1, T2)>,
}

impl<T1, T2> Constraints<T1, T2> {
    /// Creates a new [`Constraints`].
    ///
    /// # Examples
    ///
    /// ```
    /// let constraints = Constraints::new();
    /// ```
    pub fn new() -> Self {
        Self {
            equals: FxHashSet::default(),
            check: Vec::default(),
        }
    }

    pub fn any(&self) -> bool {
        !self.check.is_empty()
    }
}

impl<T1, T2> Default for Constraints<T1, T2> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T1: Copy + Hash + Eq, T2: Copy + Hash + Eq> Constraints<T1, T2> {
    /// Assumes two entries are equal, and inserts them into the list of
    /// constraints.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut constraints = Constraints::new();
    ///
    /// constraints.assume_equal(1, 1);
    /// ```
    pub fn assume_equal(&mut self, a: T1, b: T2) {
        // insert in both (a, b) and (b, a) so that comparisons for the reverse of the
        // two pair succeed.
        let did_insert = self.equals.insert((a, b));
        // let did_insert_2 = self.equals.insert((b, a));
        // debug_assert_eq!(did_insert, did_insert_2);

        if did_insert {
            self.check.push((a, b));
        }
    }

    /// Gets the next pair of elements to check for deep equality.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut constraints = Constraints::new();
    ///
    /// constraints.assume_equal(1, 1);
    /// assert!(constraints.next(), Some((1, 1)));
    /// ```
    pub fn next(&mut self) -> Option<(T1, T2)> {
        self.check.pop()
    }
}

// impl<T: Tag> Type<T> {
//     /// Tries to determine if two types are probably equal.
//     ///
//     /// This is implemented because it is possible to determine if two types
//     /// are definitively not equal. However, for other types, such as
//     /// [`Type::Record`]s and [`Type::Union`]s, more work needs to be done to
//     /// determine their equality. However, it is also obvious to see that
//     /// [`Type::Any`] is definitely not [`Type::Bytes`].
//     ///
//     /// # Examples
//     ///
//     /// ```
//     /// # use crate::id::{NoContext, RecordId};
//     /// # use MaybeEqual::*;
//     /// let any = Type::<NoContext>::Any;
//     /// let record1 = Type::<NoContext>::Record(RecordId::new_with_value(1));
//     /// let record2 = Type::<NoContext>::Record(RecordId::new_with_value(2));
//     ///
//     /// assert_eq!(any.maybe_equals(record1), No);
//     /// assert_eq!(record1.maybe_equals(record2), Maybe);
//     /// assert_eq!(any.maybe_equals(any), Yes);
//     /// ```
//     pub fn maybe_equals(&self, other: &Type<T>) -> MaybeEqual {
//         maybe_equals(self, other, |_, _| (), |_, _| ())
//     }
// }

#[cfg(test)]
mod tests {
    use crate::id::{Counter, NoContext};
    use crate::types::{Record, Type, TypeCtx};

    /// Compares if two simple records are equivalent:
    ///
    /// ```text
    /// %1 : { a : Int }
    /// %2 : { a : Int }
    /// ```
    #[test]
    pub fn simple_eq() {
        let mut branch_hit = false;
        let gen = Counter::new();
        let unique_id = Counter::new();
        let mut ctx = TypeCtx::<NoContext>::new();

        let reg1 = gen.next();
        let reg2 = gen.next();

        ctx.borrow_mut(|mut ctx| {
            let rec1 = ctx.make_type_record(Record::new(unique_id.next()));
            let rec2 = ctx.make_type_record(Record::new(unique_id.next()));
            // TODO: add `a : Int` as a fact

            // TODO: how to insert record
            // ctx.insert(reg1, Type::Record(rec1));
            // ctx.insert(reg2, Type::Record(rec2));

            let rec1 = *ctx.get(&reg1).unwrap();
            let rec2 = *ctx.get(&reg2).unwrap();

            branch_hit = true;
            assert_eq!(rec1, rec2);
        });

        assert!(branch_hit);
    }
}
