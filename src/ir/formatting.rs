use super::*;
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

        for global_variable in self.global_variables.iter() {
            padline!();
            write!(f, "global {};\n", global_variable.id)?;
        }

        for external_function in self.external_functions.iter() {
            padline!();
            write!(
                f,
                "extern fn {}({});\n",
                external_function.id,
                CommaSep(&external_function.parameters),
            )?;
        }

        for function in self.functions.iter() {
            padline!();
            write!(
                f,
                "fn {}({}) {{\n",
                function.id,
                CommaSep(&function.arguments)
            )?;

            for block in function.body.blocks.iter() {
                write!(f, "{}:\n", block.id)?;

                for instruction in block.instructions.iter() {
                    write!(f, "  {}\n", instruction)?;
                }
            }

            write!(f, "}}\n")?;
        }

        Ok(())
    }
}

impl Display for TypedParameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}: {}", self.register.id, self.kind)
    }
}

impl Display for Parameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.register.id)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "Any")
    }
}

impl Display for Scope {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Scope::Global => write!(f, "@"),
            Scope::Local => write!(f, "%"),
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{}.{}",
            self.scope,
            self.name.as_deref().unwrap_or(""),
            self.id
        )

        // Written below is the proper algorithm for writing an identifier.
        // For simplicity, we are doing none of that.
        //
        // 1. Prefix the identifier with the scope.
        // 2. Write the name of the identifier, if any.
        // 3. An identifier should write the ID (the `.0` of `%id.0`) portion if:
        //   a. There is no name
        //   b. There is an identifier other than the target identifier with a similar name
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Instruction::Ret => write!(f, "ret"),
        }
    }
}

struct CommaSep<'t, T>(&'t Vec<T>);

impl<'t, T: Display> Display for CommaSep<'t, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let mut needs_comma = false;

        macro_rules! put_comma {
            () => {
                if needs_comma {
                    write!(f, ", ")?
                }
                needs_comma = true;
            };
        };

        for element in self.0.iter() {
            put_comma!();
            write!(f, "{}", element)?;
        }

        Ok(())
    }
}
