use super::*;
use contextual_display::*;
use std::fmt::{Display, Formatter, Result};

impl Display for IR {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let mut needs_spacing = false;

        macro_rules! padline {
            () => {
                if needs_spacing {
                    write!(f, "\n")?
                }
                needs_spacing = true;
            };
        };

        let global_fmt = self.identifier_formatter();

        for global_variable in self.global_variables.iter() {
            padline!();
            write!(f, "global {};\n", global_fmt.wrap(&global_variable.id))?;
        }

        for external_function in self.external_functions.iter() {
            let local_fmt = external_function.identifier_formatter();

            padline!();
            write!(
                f,
                "extern fn {}({});\n",
                global_fmt.wrap(&external_function.id),
                local_fmt.wrap(&external_function.parameters)
            )?;
        }

        Ok(())
    }
}

/// Represents the scope an identifier is in.
#[derive(Clone, Copy)]
pub enum Scope {
    /// The global scope, `@`.
    Global,
    /// The local scope, `%`.
    Local,
}

pub struct IdentifierContext {
    scope: Scope,
    identifiers: Vec<Identifier>,
}

impl IdentifierContext {
    pub fn new<'i>(
        scope: Scope,
        identifiers: impl Iterator<Item = &'i Identifier>,
    ) -> IdentifierContext {
        let identifiers = identifiers.cloned().collect::<Vec<_>>();
        Self { scope, identifiers }
    }
}

impl IR {
    pub fn identifier_formatter(&self) -> DisplayContext<IdentifierContext> {
        DisplayContext::new(IdentifierContext::new(Scope::Global, self.identifiers()))
    }

    pub fn identifiers<'me>(&'me self) -> impl Iterator<Item = &'me Identifier> {
        self.global_variables
            .iter()
            .map(|g| &g.id)
            .chain(self.external_functions.iter().map(|f| &f.id))
            .chain(self.functions.iter().map(|f| &f.id))
    }
}

impl ExternalFunction {
    pub fn identifier_formatter(&self) -> DisplayContext<IdentifierContext> {
        DisplayContext::new(IdentifierContext::new(Scope::Local, self.identifiers()))
    }

    pub fn identifiers<'me>(&'me self) -> impl Iterator<Item = &'me Identifier> {
        self.parameters
            .iter()
            .map(|parameter| &parameter.register.id)
    }
}

impl Function {
    pub fn identifier_formatter(&self) -> DisplayContext<IdentifierContext> {
        DisplayContext::new(IdentifierContext::new(Scope::Local, self.identifiers()))
    }

    pub fn identifiers<'me>(&'me self) -> impl Iterator<Item = &'me Identifier> {
        self.arguments.iter().map(|r| &r.id)
    }
}

impl ContextualDisplay<IdentifierContext> for &Identifier {
    fn fmt(&self, f: &mut Formatter<'_>, ctx: &IdentifierContext) -> Result {
        format_identifier(f, ctx.scope, self, ctx.identifiers.iter())
    }
}

impl ContextualDisplay<IdentifierContext> for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>, ctx: &IdentifierContext) -> Result {
        format_identifier(f, ctx.scope, self, ctx.identifiers.iter())
    }
}

impl ContextualDisplay<IdentifierContext> for &Vec<TypedParameter> {
    fn fmt(&self, f: &mut Formatter<'_>, ctx: &IdentifierContext) -> Result {
        let mut needs_comma = false;

        macro_rules! putcomma {
            () => {
                if needs_comma {
                    write!(f, ", ")?
                }
                needs_comma = true;
            };
        };

        for typed_parameter in self.iter() {
            putcomma!();
            typed_parameter.fmt(f, ctx)?;
        }

        Ok(())
    }
}

impl ContextualDisplay<IdentifierContext> for TypedParameter {
    fn fmt(&self, f: &mut Formatter<'_>, ctx: &IdentifierContext) -> Result {
        self.register.fmt(f, ctx)?;
        write!(f, ": ")?;
        self.kind.fmt(f, ctx)?;
        Ok(())
    }
}

impl ContextualDisplay<IdentifierContext> for Register {
    fn fmt(&self, f: &mut Formatter<'_>, ctx: &IdentifierContext) -> Result {
        self.id.fmt(f, ctx)
    }
}

impl<C> ContextualDisplay<C> for Type {
    fn fmt(&self, f: &mut Formatter<'_>, ctx: &C) -> Result {
        write!(f, "Any")
    }
}

// TODO: examples
/// Writes the name part of an identifier.
///
/// If there are no identifiers with the same name, this will just write the name.
/// If the identifier does not have a name, this will just write the numerical ID.
/// If there is another identifier with the same name, this will append the numerical ID.
///
/// It is expected the caller
pub fn format_identifier<'identifier>(
    f: &mut Formatter<'_>,
    scope: Scope,
    identifier: &Identifier,
    mut identifiers: impl Iterator<Item = &'identifier Identifier>,
) -> Result {
    // 1. Prefix the identifier with the scope.
    match scope {
        Scope::Global => write!(f, "@")?,
        Scope::Local => write!(f, "%")?,
    };

    // 2. Write the name of the identifier, if any.
    if let Some(name) = &identifier.name {
        write!(f, "{}", name)?;
    }

    // 3. An identifier should write the ID (the `.0` of `%id.0`) portion if:
    let should_write_id = match &identifier.name {
        // a. There is no name
        None => true,
        // b. There is an identifier other than the target identifier with a similar name
        Some(name) => identifiers.any(|i| i.id != identifier.id && i.name.contains(name)),
    };

    if should_write_id {
        write!(f, ".{}", identifier.id)?;
    }

    Ok(())
}

mod contextual_display {
    use super::*;

    pub struct DisplayContext<C> {
        context: C,
    }

    impl<C> DisplayContext<C> {
        pub fn new(context: C) -> Self {
            Self { context }
        }

        pub fn wrap<'c, E: ContextualDisplay<C>>(
            &'c self,
            element: E,
        ) -> ContextuallyDisplayable<'c, C, E> {
            ContextuallyDisplayable {
                context: &self.context,
                element,
            }
        }
    }

    pub trait ContextualDisplay<C> {
        fn fmt(&self, f: &mut Formatter<'_>, ctx: &C) -> Result;
    }

    pub struct ContextuallyDisplayable<'context, C, E> {
        context: &'context C,
        element: E,
    }

    impl<'c, C, E> Display for ContextuallyDisplayable<'c, C, E>
    where
        E: ContextualDisplay<C>,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            self.element.fmt(f, self.context)
        }
    }
}
