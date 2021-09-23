//! Implements equality for associated types.

use std::hash::Hash;

use rustc_hash::FxHashSet;

use crate::id::{RecordId, Tag, UnionId};

use super::{Type, TypeCtx};

/// Determines if two types are definitely equal.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum MaybeEqual {
    /// [`MaybeEqual::No`] represents two types definitely not being equal.
    No,
    /// [`MaybeEqual::Maybe`] represents two types possibly being equal. To
    /// determine equality, it would require more knowledge about the types.
    Maybe,
    /// [`MaybeEqual::Yes`] represents two types definitely being equal.
    Yes,
}

impl From<bool> for MaybeEqual {
    fn from(value: bool) -> Self {
        match value {
            true => MaybeEqual::Yes,
            false => MaybeEqual::No,
        }
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

impl<'ctx, T: Tag> PartialEq for Type<'ctx, T> {
    fn eq(&self, other: &Self) -> bool {
        // let mut resolver = EqualityResolver::new(self.1, other.1);

        // if resolver.are_not_equal(self, other) {
        //     return false;
        // }

        todo!()
    }
}

/// Datastructure that maintains state during equality comparisons between
/// types.
struct EqualityResolver<'ctx, T: Tag> {
    a: &'ctx TypeCtx<T>,
    b: &'ctx TypeCtx<T>,
    record_constraints: Constraints<RecordId<T>>,
    union_constraints: Constraints<UnionId<T>>,
}

impl<'ctx, T: Tag> EqualityResolver<'ctx, T> {
    pub fn new(a: &'ctx TypeCtx<T>, b: &'ctx TypeCtx<T>) -> Self {
        Self {
            a,
            b,
            record_constraints: Default::default(),
            union_constraints: Default::default(),
        }
    }

    /// Tries to determine if two types are definitely not equal.
    ///
    /// If all types are not not equal, then it can be said that all types are
    /// equal.
    pub fn are_not_equal(&mut self, a: &Type<'ctx, T>, b: &Type<'ctx, T>) -> bool {
        let record_constraints = &mut self.record_constraints;
        let union_constraints = &mut self.union_constraints;

        let maybe_eq = maybe_equals(
            a,
            b,
            |r1, r2| record_constraints.assume_equal(*r1, *r2),
            |u1, u2| union_constraints.assume_equal(*u1, *u2),
        );

        MaybeEqual::No == maybe_eq
    }
}

/*
i remember reading about ghostcell and it had the idea of using borrow checking to likemake sure "indices" came and were used from the same collection, could that at all be applied here?

in my case, i have the following (simplified) structure```rs
struct TypeCtx {
    types: HashMap<usize, Type>,
    records: HashMap<usize, Record>,
}

enum Type {
    Int,
    Record(usize),
}

struct AssocType(&Type, &TypeCtx);
struct AssocRecord(&Record, &TypeCtx);
```and i would like to construct the following code
*/

/// Implementation of `Type::maybe_equals` that is extremely generic.
fn maybe_equals<T: Tag, U: Tag, FR, FU>(
    a: &Type<T>,
    b: &Type<U>,
    records: FR,
    unions: FU,
) -> MaybeEqual
where
    FR: FnOnce(&RecordId<T>, &RecordId<U>),
    FU: FnOnce(&UnionId<T>, &UnionId<U>),
{
    use Type::*;

    match (a, b) {
        // records and unions need more comparisons
        (Record(r1), Record(r2)) => {
            // records(r1, r2);
            MaybeEqual::Maybe
        }
        (Union(u1), Union(u2)) => {
            // unions(u1, u2);
            MaybeEqual::Maybe
        }
        (Any, Any) | (Bytes, Bytes) | (Number, Number) | (Boolean, Boolean) => MaybeEqual::Yes,
        (Trivial(a), Trivial(b)) => (a == b).into(),
        (Int(a), Int(b)) => (a == b).into(),
        (Float(a), Float(b)) => (a == b).into(),
        (Bool(a), Bool(b)) => (a == b).into(),
        (FnPtr(a), FnPtr(b)) => (a == b).into(),
        (Byts(a), Byts(b)) => todo!(), // (resolve_spur(a) == resolve_spur(b)).into(),
        _ => MaybeEqual::No,
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
struct Constraints<T> {
    /// List of elements that are assumed to be equal.
    equals: FxHashSet<(T, T)>,
    /// List of elements to check for deep equality.
    ///
    /// The order in which deep equality is checked for doesn't matter, so we
    /// just push and pop from a Vec for maximum performance.
    check: Vec<(T, T)>,
}

impl<T> Constraints<T> {
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
}

impl<T> Default for Constraints<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Copy + Hash + Eq> Constraints<T> {
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
    pub fn assume_equal(&mut self, a: T, b: T) {
        // insert in both (a, b) and (b, a) so that comparisons for the reverse of the
        // two pair succeed.
        let did_insert = self.equals.insert((a, b));
        let did_insert_2 = self.equals.insert((b, a));
        debug_assert_eq!(did_insert, did_insert_2);

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
    pub fn next(&mut self) -> Option<(T, T)> {
        self.check.pop()
    }
}

// impl<'ctx, 'ctx2, T: Tag> PartialEq<AssociatedType<'ctx2, T>> for
// AssociatedType<'ctx, T> {     fn eq(&self, other: &AssociatedType<'ctx2, T>)
// -> bool {         self.0 == other.0 && self.1 == other.1
//     }
// }

// impl<'ctx, T: Tag> Eq for AssociatedType<'ctx, T> {}
