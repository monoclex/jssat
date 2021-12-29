//! Implements deep equality for types.
//!
//! # Algorithm

use std::{cell::RefCell, hash::Hash};

use rustc_hash::FxHashSet;

use crate::id::{RecordId, Tag, UnionId};

use super::{
    type_ctx::TypeCtxImmut, CtxBox, Record, RecordHandle, Type, TypeCtx, TypeRefEq, UnionHandle,
};

impl<'ctx, T: Tag> Type<'ctx, T> {
    pub fn deep_eq(&self, other: &Self) -> bool {
        let mut resolver = EqualityResolver::new();

        if resolver.are_not_equal(self, other) {
            return false;
        }

        if resolver.solve_constraints() {
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
    constraints: Constraints<TypeRefEq<'ctx1, T>, TypeRefEq<'ctx2, T>>,
    #[cfg(debug_assertions)]
    solve_constraints_called: bool,
    #[cfg(debug_assertions)]
    last_are_not_equal: bool,
}

impl<'ctx1, 'ctx2, T: Tag> EqualityResolver<'ctx1, 'ctx2, T> {
    pub fn new() -> Self {
        Self {
            constraints: Default::default(),
            #[cfg(debug_assertions)]
            solve_constraints_called: false,
            #[cfg(debug_assertions)]
            // default state is to complain
            // this will trigger complaining in drop impl
            last_are_not_equal: false,
        }
    }

    /// Solves all constraints until there are none less. If any types within
    /// the constraints are not equal, this will return `true`. If all types are
    /// not not-equal (meaning that they are either "probably equal" or "are
    /// equal"), this will return `false`.
    pub fn solve_constraints(&mut self) -> bool {
        #[cfg(debug_assertions)]
        {
            self.solve_constraints_called = true;
        }

        while self.has_constraints_to_verify() {
            while let Some((a, b)) = self.constraints.next() {
                let (a, b) = match (a.0, b.0) {
                    (Type::Record(a), Type::Record(b)) => (a, b),
                    (Type::List(a), Type::List(b)) => todo!(),
                    (Type::Union(a), Type::Union(b)) => todo!(),
                    _ => unreachable!(),
                };

                let a = a.borrow();
                let b = b.borrow();

                if self.records_not_equal(&a, &b) {
                    return false;
                }
            }
        }

        false
    }

    pub fn has_constraints_to_verify(&self) -> bool {
        self.constraints.any()
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
                self.constraints.assume_equal(TypeRefEq(*a), TypeRefEq(*b));
                MaybeEqual::Maybe
            }
            (Union(u1), Union(u2)) => {
                self.constraints.assume_equal(TypeRefEq(*a), TypeRefEq(*b));
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

        let result = MaybeEqual::No == maybe_equal;

        #[cfg(debug_assertions)]
        {
            self.last_are_not_equal = result;
        }

        result
    }

    /// Compares equality between two records. Returns `true` if the records
    /// are definitely not equal, otherwise `false` if they might be equal. Use
    /// the `solve_constraints` method to determine if they are completely
    /// equal.
    pub fn records_not_equal(&mut self, a: &Record<'ctx1, T>, b: &Record<'ctx2, T>) -> bool {
        macro_rules! ret {
            () => {{
                #[cfg(debug_assertions)]
                {
                    self.last_are_not_equal = true;
                }

                return true;
            }};
        }

        if a.unique_id() != b.unique_id() {
            ret!();
        }

        if a.len() != b.len() {
            ret!();
        }

        for (k, value_a) in a.iter() {
            enum EitherKey<'ctx1, 'ctx2, T: Tag> {
                Naive(Type<'ctx2, T>),
                Awful(Type<'ctx1, T>),
            };

            let b_key = match k {
                Type::Any => EitherKey::Naive(Type::Any),
                Type::Nothing => EitherKey::Naive(Type::Nothing),
                Type::Bytes => EitherKey::Naive(Type::Bytes),
                Type::Number => EitherKey::Naive(Type::Number),
                Type::Boolean => EitherKey::Naive(Type::Boolean),
                Type::Atom(x) => EitherKey::Naive(Type::Atom(*x)),
                Type::Int(x) => EitherKey::Naive(Type::Int(*x)),
                Type::Float(x) => EitherKey::Naive(Type::Float(*x)),
                Type::Bool(x) => EitherKey::Naive(Type::Bool(*x)),
                Type::FnPtr(x) => EitherKey::Naive(Type::FnPtr(*x)),
                Type::Byts(_) => EitherKey::Awful(*k),
                Type::List(_) => todo!(),
                Type::Record(_) => todo!(),
                Type::Union(_) => todo!(),
            };

            let value_b = match b_key {
                EitherKey::Naive(key) => match b.get(&key) {
                    Some(v) => *v,
                    None => ret!(),
                },
                EitherKey::Awful(typ) => {
                    // now we have to perform a linear search to find the type
                    // that matches this type as the key of the other object
                    //
                    // i fear that this is unstable
                    let mut matches = Vec::new();
                    for (key, value) in b.iter() {
                        // NOTE: this instantiates a new `EqualityResolver` for each key...
                        // yeeeah...
                        let mut eq = EqualityResolver::new();

                        if eq.are_not_equal(&typ, key) {
                            continue;
                        }

                        if eq.solve_constraints() {
                            continue;
                        }

                        matches.push(*value);
                        // TODO(future): if this algo is actually sane, `break`
                        // early
                    }

                    assert!(
                        matches.len() <= 1,
                        "this algorithm is unstable, we cannot do this. oh god"
                    );

                    match matches.get(0) {
                        Some(x) => *x,
                        None => ret!(),
                    }
                }
            };

            if self.are_not_equal(value_a, &value_b) {
                ret!();
            }
        }

        #[cfg(debug_assertions)]
        {
            self.last_are_not_equal = false;
        }

        false
    }
}

impl<'c1, 'c2, T: Tag> Drop for EqualityResolver<'c1, 'c2, T> {
    #[cfg(not(debug_assertions))]
    fn drop(&mut self) {}

    #[cfg(debug_assertions)]
    #[track_caller]
    fn drop(&mut self) {
        if !self.solve_constraints_called && self.has_constraints_to_verify() {
            // if this is `false`, that means we should've checked the constraints
            if !self.last_are_not_equal {
                panic!("EqualityResolver created without `solve_constriants` being called. possible bug.");
            }
        }
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
    use crate::types::{EqualityResolver, Record, Type, TypeCtx};

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
            let id = unique_id.next();

            let mut rec1 = Record::new(id);
            rec1.insert(ctx.make_type_byts(b"a"), Type::Number);

            let mut rec2 = Record::new(id);
            rec2.insert(ctx.make_type_byts(b"a"), Type::Number);

            let rec1 = ctx.make_type_record(rec1);
            let rec2 = ctx.make_type_record(rec2);

            ctx.insert(reg1, rec1);
            ctx.insert(reg2, rec2);

            let rec1 = ctx.get(&reg1).unwrap();
            let rec2 = ctx.get(&reg2).unwrap();

            branch_hit = true;
            assert_eq!(rec1, rec2);
        });

        assert!(branch_hit);
    }

    #[test]
    #[should_panic]
    pub fn catch_eqquality_resolver_not_calling_solve_constraints() {
        fn problematic(a: Type<NoContext>, b: Type<NoContext>) -> bool {
            let mut resolver = EqualityResolver::new();

            if resolver.are_not_equal(&a, &b) {
                return false;
            }

            true
        }

        let mut a = TypeCtx::<NoContext, ()>::new();

        a.borrow_mut(|mut a| {
            let mut rec = Record::new(Default::default());
            let rec_typ = a.make_type_record(rec);

            let mut rec = rec_typ.unwrap_record().borrow_mut();
            rec.insert(rec_typ, rec_typ);
            drop(rec);

            problematic(rec_typ, rec_typ);
        })
    }
}
