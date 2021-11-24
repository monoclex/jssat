//! Implements type relationships for use in records.
//!
//! Type relationships are necessary to know between types in order to determine
//! the possible values that one may pull out of a record.
//!
//! For example, consider the following record:
//!
//! ```text
//! { "a" |-> 1, String |-> "N/A" }
//! ```
//!
//! If an arbitrary `String` were to be used as a lookup in this record, the
//! possible resulting values are either `1` or `"N/A"` because it is possible
//! for a `String` to be `"a"`. Abstractly speaking, `String` is a supertype of
//! `"a"` which means it is possible for a lookup of type `String` to yield the
//! same results as a lookup of type `"a"` as well.

use derive_more::Display;

use super::{Type, TypeKind};
use crate::id::Tag;

/// Describes a relationship between two types.
#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, Hash)]
pub enum TypeRelation {
    /// [`TypeRelation::Subtype`] represents when two types have a subtypal
    /// relationship to one another.
    ///
    /// That is, if `A` is a subtype of `B`, then `A` can be used in all places
    /// `B` can.
    Subtype,
    /// [`TypeRelation::Supertype`] represents when two types have a supertypal
    /// relationship to one another.
    ///
    /// That is, if `A` is a supertype of `B`, then `B` can be used in all
    /// places `A` can.
    Supertype,
    /// [TypeRelation::Equal`] represents when two types are equivalent to one
    /// another.
    ///
    /// That is, if `A` is equal to `B`, then `A` can substitute `B` and `B` can
    /// substitute `A`.
    Equal,
}

impl TypeRelation {
    /// Reverses the direction of the type relation.
    ///
    /// # Examples
    ///
    /// ```
    /// # use TypeRelation::*;
    /// assert_eq!(None.reverse(), None);
    /// assert_eq!(Subtype.reverse(), Supertype);
    /// assert_eq!(Supertype.reverse(), Subtype);
    /// assert_eq!(Equal.reverse(), Equal);
    /// ```
    pub fn reverse(self) -> TypeRelation {
        use TypeRelation::*;

        match self {
            Subtype => Supertype,
            Supertype => Subtype,
            Equal => Equal,
        }
    }
}

impl<'ctx, T1: Tag> Type<'ctx, T1> {
    /// Determines if two types are of the same kind, meaning that they are of
    /// the same variant.
    ///
    /// # Examples
    ///
    /// ```
    /// # use crate::id::NoContext;
    /// let a: Type<NoContext> = Type::Any;
    /// let b: Type<NoContext> = Type::Any;
    /// let c: Type<NoContext> = Type::Bytes;
    ///
    /// assert!(a.is_same_kind(b));
    /// assert!(!a.is_same_kind(c));
    /// ```
    pub fn is_same_kind<U: Tag>(&self, other: &Type<U>) -> bool {
        // cannot use [`std::mem::discriminant`] because of different types:
        //     [`Type<T>`] vs [`Type<U>`]
        // std::mem::discriminant(self) == std::mem::discriminant(other)

        TypeKind::from(self) == TypeKind::from(other)
    }

    /// Determines if a type is substitutable by another type.
    ///
    /// # Examples
    ///
    /// ```
    /// # use crate::id::NoContext;
    /// let a: Type<NoContext> = Type::Number;
    /// let b: Type<NoContext> = Type::Int(42);
    /// let c: Type<NoContext> = Type::Bytes;
    ///
    /// assert!(a.is_substitutable_by(b));
    /// assert!(!b.is_substitutable_by(a));
    /// assert!(!a.is_substitutable_by(c));
    /// ```
    pub fn is_substitutable_by<U: Tag>(&self, other: &Type<U>) -> bool {
        use TypeRelation::*;

        matches!(self.relation_to(other), Some(Supertype | Equal))
    }

    /// Determines the relationship of two types based on a defined set of
    /// subtyping rules.
    ///
    /// The following rules are used to determine the relationship between two
    /// types:
    ///
    /// - `T` is always equal to `T`
    /// - `Any` is a supertype of `T`, such that `T` =/= `Any`
    /// - `Bytes` is a supertype of `Byts`
    /// - `Number` is a supertype of `Int`
    /// - `Number` is a supertype of `Float`
    /// - `Boolean` is a supertype of `Bool`
    /// - If `A` is a supertype of `B`, then `B` is a subtype of `A`
    ///
    /// Any relationship that does not fall into this set of rules produces
    /// [`None`].
    pub fn relation_to<U: Tag>(&self, other: &Type<U>) -> Option<TypeRelation> {
        use Type::*;
        use TypeRelation::*;

        // `T` is always equal to `T`
        if self.is_same_kind(other) {
            return Some(Equal);
        }

        // Perform the algorithm specified in the doc comment
        if let Some(relation) = algo(self, other) {
            return Some(relation);
        }

        // If `A` is a supertype of `B`, then `B` is a subtype of `A`
        // Thus, we can perform the algorithm specified in the doc comment in reverse.
        if let Some(relation) = algo(other, self) {
            return Some(relation.reverse());
        }

        return None;

        #[inline(always)]
        fn algo<T: Tag, U: Tag>(lhs: &Type<T>, rhs: &Type<U>) -> Option<TypeRelation> {
            // `Any` is a supertype of `T`, such that `T` =/= `Any`
            if let Any = lhs {
                // it is impossible for `rhs` to be `Any` because `is_same_kind` prevents that
                debug_assert!(!matches!(rhs, Any));

                return Some(Supertype);
            }

            // `Bytes` is a supertype of `Byts`
            if let Bytes = lhs {
                if let Byts(_) = rhs {
                    return Some(Supertype);
                }
            }

            // `Number` is a supertype of `Int`
            // `Number` is a supertype of `Float`
            if let Number = lhs {
                match rhs {
                    Int(_) | Float(_) => return Some(Supertype),
                    _ => {}
                }
            }

            // `Boolean` is a supertype of `Bool`
            if let Boolean = lhs {
                if let Bool(_) = rhs {
                    return Some(Supertype);
                }
            }

            None
        }
    }
}
