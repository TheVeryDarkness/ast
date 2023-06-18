//! The [`FromPest`] conversion framework to convert from pest trees into typed structure.

#[doc(hidden)]
pub extern crate log;
#[doc(hidden)]
pub extern crate pest;
extern crate void;

#[doc(inline)]
pub use void::Void;

use {
    pest::{
        iterators::{Pair, Pairs},
        RuleType,
    },
    std::marker::PhantomData,
};

/// An error that occurs during conversion.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ConversionError<FatalError, Rule: RuleType> {
    /// No match occurred: this node is not present here
    NoMatch(NoMatch<Rule>),
    /// No match occurred: the tree is empty
    Empty,
    /// Fatal error: this node is present but malformed
    Malformed(FatalError),
    /// Found unexpected tokens at the end
    Extraneous { current_node: &'static str },
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct NoMatch<Rule: RuleType> {
    expected_rule: Rule,
    found_rule: Rule,
}
impl<Rule: RuleType> Display for NoMatch<Rule> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "Expected {:?} but found {:?}. Rule did not match, failed to convert node.",
            self.expected_rule, self.found_rule
        ))
    }
}
impl<Rule: RuleType> NoMatch<Rule> {
    pub fn new(expected_rule: Rule, found_rule: Rule) -> NoMatch<Rule> {
        NoMatch {
            expected_rule,
            found_rule,
        }
    }
}

use std::fmt::{self, Display};

impl<FatalError, Rule: RuleType> fmt::Display for ConversionError<FatalError, Rule>
where
    FatalError: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ConversionError::NoMatch(no_match) => Display::fmt(no_match, f),
            ConversionError::Empty => write!(f, "No tokens to be converted."),
            ConversionError::Malformed(fatalerror) => write!(f, "Malformed node: {}", fatalerror),
            ConversionError::Extraneous { current_node, .. } => write!(
                f,
                "when converting {}, found extraneous tokens",
                current_node
            ),
        }
    }
}

use std::error;

impl<FatalError, Rule: RuleType> error::Error for ConversionError<FatalError, Rule>
where
    FatalError: error::Error + 'static,
{
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            ConversionError::NoMatch(_) => None,
            ConversionError::Empty => None,
            ConversionError::Extraneous { .. } => None,
            ConversionError::Malformed(ref fatalerror) => Some(fatalerror),
        }
    }
}

/// Potentially borrowing conversion from a pest parse tree.
pub trait FromPest<'pest>: Sized {
    /// The rule type for the parse tree this type corresponds to.
    type Rule: RuleType;
    /// A fatal error during conversion.
    type FatalError;
    /// Convert from a Pest parse tree.
    ///
    /// # Return type semantics
    ///
    /// - `Err(ConversionError::NoMatch)` => node not at head of the cursor, cursor unchanged
    /// - `Err(ConversionError::Malformed)` => fatal error; node at head of the cursor but malformed
    /// - `Ok` => success; the cursor has been updated past this node
    fn from_pest(
        pest: &mut Pairs<'pest, Self::Rule>,
    ) -> Result<Self, ConversionError<Self::FatalError, Self::Rule>>;
}

/// Convert a production without storing it.
impl<'pest, Rule: RuleType, T: FromPest<'pest, Rule = Rule>> FromPest<'pest> for PhantomData<T> {
    type Rule = Rule;
    type FatalError = T::FatalError;
    fn from_pest(
        pest: &mut Pairs<'pest, Rule>,
    ) -> Result<Self, ConversionError<T::FatalError, T::Rule>> {
        T::from_pest(pest).map(|_| PhantomData)
    }
}

/// For recursive grammars.
impl<'pest, Rule: RuleType, T: FromPest<'pest, Rule = Rule>> FromPest<'pest> for Box<T> {
    type Rule = Rule;
    type FatalError = T::FatalError;
    fn from_pest(
        pest: &mut Pairs<'pest, Rule>,
    ) -> Result<Self, ConversionError<T::FatalError, T::Rule>> {
        T::from_pest(pest).map(Box::new)
    }
}

/// Convert an optional production.
impl<'pest, Rule: RuleType, T: FromPest<'pest, Rule = Rule>> FromPest<'pest> for Option<T> {
    type Rule = Rule;
    type FatalError = T::FatalError;
    fn from_pest(
        pest: &mut Pairs<'pest, Rule>,
    ) -> Result<Self, ConversionError<T::FatalError, T::Rule>> {
        match T::from_pest(pest) {
            Err(ConversionError::NoMatch(_)) => Ok(None),
            Err(ConversionError::Empty) => Ok(None),
            result => result.map(Some),
        }
    }
}

/// Convert many productions. (If `<T as FromPest>` is non-advancing, this will be non-terminating.)
impl<'pest, Rule: RuleType, T: FromPest<'pest, Rule = Rule>> FromPest<'pest> for Vec<T> {
    type Rule = Rule;
    type FatalError = T::FatalError;
    fn from_pest(
        pest: &mut Pairs<'pest, Rule>,
    ) -> Result<Self, ConversionError<T::FatalError, T::Rule>> {
        let mut acc = vec![];
        loop {
            match T::from_pest(pest) {
                Ok(t) => acc.push(t),
                Err(ConversionError::NoMatch(_)) => break,
                Err(ConversionError::Empty) => break,
                Err(error) => return Err(error),
            }
        }
        Ok(acc)
    }
}

/// Consume a production without doing any processing.
impl<'pest, Rule: RuleType> FromPest<'pest> for Pair<'pest, Rule> {
    type Rule = Rule;
    type FatalError = Void;
    fn from_pest(pest: &mut Pairs<'pest, Rule>) -> Result<Self, ConversionError<Void, Rule>> {
        pest.next().ok_or(ConversionError::Empty)
    }
}

macro_rules! impl_for_tuple {
    () => {};
    ($ty1:ident $($ty:ident)*) => {
        impl<'pest, $ty1, $($ty,)* Rule: RuleType, FatalError> FromPest<'pest> for ($ty1, $($ty),*)
        where
            $ty1: FromPest<'pest, Rule=Rule, FatalError=FatalError>,
            $($ty: FromPest<'pest, Rule=Rule, FatalError=FatalError>,)*
        {
            type Rule = Rule;
            type FatalError = FatalError;
            fn from_pest(pest: &mut Pairs<'pest, Rule>)
                -> Result<Self, ConversionError<FatalError,Rule>>
            {
                let mut clone = pest.clone();
                let this = (
                    $ty1::from_pest(&mut clone)?,
                    $($ty::from_pest(&mut clone)?),*
                );
                *pest = clone;
                Ok(this)
            }
        }
        impl_for_tuple!($($ty)*);
    };
}

impl_for_tuple!(A B C D);
