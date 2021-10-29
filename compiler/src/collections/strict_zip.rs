use std::iter::Zip;

pub trait StrictZip
where
    Self: ExactSizeIterator,
{
    /// The exact same as [std::iter::zip], except that it asserts that the two
    /// iterators are of the exact same length.
    #[allow(clippy::disallowed_method)]
    fn strict_zip<U>(self, other: U) -> Zip<Self, U::IntoIter>
    where
        Self: Sized,
        U: IntoIterator<IntoIter: ExactSizeIterator>,
    {
        let other = other.into_iter();
        assert_eq!(self.len(), other.len());
        self.zip(other)
    }
}

impl<I: ExactSizeIterator> StrictZip for I {}
