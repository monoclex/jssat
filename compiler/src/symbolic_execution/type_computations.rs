use crate::isa::BinaryOperator;
use thiserror::Error;

use super::types::{RegisterType, TypeBag};

// impl BinaryOperator {
//     pub fn make_executor<'t>(&self, types: &'t mut TypeBag) ->
// BinOpExecutor<'t> {         BinOpExecutor { types, op: *self }
//     }
// }

pub struct BinOpExecutor<'a> {
    types: &'a mut TypeBag,
    op: BinaryOperator,
}

#[derive(Error, Debug)]
pub enum BinOpExecErr {
    #[error("The binary operator {0} cannot be performed for the two types given ({1}, {2}).")]
    Impossible(BinaryOperator, RegisterType, RegisterType),
    #[error("The operator {0} is unimplemented for the given types ({1}, {2})")]
    Unimplemented(BinaryOperator, RegisterType, RegisterType),
}

pub enum BinaryOperatorExecutionError {
    Impossible,
    Unimplemented,
}

impl BinOpExecutor<'_> {
    pub fn execute(
        &mut self,
        lhs: RegisterType,
        rhs: RegisterType,
    ) -> Result<RegisterType, BinOpExecErr> {
        let result = match self.op {
            BinaryOperator::Add => self.add(lhs, rhs),
            BinaryOperator::And => self.and(lhs, rhs),
            BinaryOperator::Or => self.or(lhs, rhs),
            BinaryOperator::Equals => self.equals(lhs, rhs),
            BinaryOperator::LessThan => todo!(),
            // _ => Err(BinaryOperatorExecutionError::Unimplemented),
        };

        match result {
            Ok(t) => Ok(t),
            Err(BinaryOperatorExecutionError::Impossible) => {
                Err(BinOpExecErr::Impossible(self.op, lhs, rhs))
            }
            Err(BinaryOperatorExecutionError::Unimplemented) => {
                Err(BinOpExecErr::Unimplemented(self.op, lhs, rhs))
            }
        }
    }

    fn add(
        &mut self,
        lhs: RegisterType,
        rhs: RegisterType,
    ) -> Result<RegisterType, BinaryOperatorExecutionError> {
        use RegisterType::*;

        Ok(match (lhs, rhs) {
            (Bytes, Bytes) | (Byts(_), Bytes) | (Bytes, Byts(_)) => Bytes,
            (Byts(a), Byts(b)) => {
                let mut new = self.types.unintern_const(a).to_owned();
                new.extend(self.types.unintern_const(b));
                let id = self.types.intern_constant(&new);
                Byts(id)
            }
            _ => return Err(BinaryOperatorExecutionError::Unimplemented),
        })
    }

    fn and(
        &self,
        lhs: RegisterType,
        rhs: RegisterType,
    ) -> Result<RegisterType, BinaryOperatorExecutionError> {
        use RegisterType::*;

        Ok(match (lhs, rhs) {
            (Boolean, Boolean) | (Boolean, Bool(_)) | (Bool(_), Boolean) => Boolean,
            (Bool(a), Bool(b)) => Bool(a && b),
            _ => return Err(BinaryOperatorExecutionError::Unimplemented),
        })
    }

    fn or(
        &self,
        lhs: RegisterType,
        rhs: RegisterType,
    ) -> Result<RegisterType, BinaryOperatorExecutionError> {
        use RegisterType::*;

        Ok(match (lhs, rhs) {
            (Boolean, Boolean) | (Boolean, Bool(_)) | (Bool(_), Boolean) => Boolean,
            (Bool(a), Bool(b)) => Bool(a || b),
            _ => return Err(BinaryOperatorExecutionError::Unimplemented),
        })
    }

    fn equals(
        &self,
        lhs: RegisterType,
        rhs: RegisterType,
    ) -> Result<RegisterType, BinaryOperatorExecutionError> {
        use RegisterType::*;

        Ok(match (lhs, rhs) {
            (Number, Number) | (Int(_), Number) | (Number, Int(_)) => Boolean,
            (Int(a), Int(b)) => Bool(a == b),
            (Bytes, Bytes) | (Byts(_), Bytes) | (Bytes, Byts(_)) => Boolean,
            (Byts(a), Byts(b)) => {
                Bool(self.types.unintern_const(a) == self.types.unintern_const(b))
            }
            (Boolean, Boolean) | (Boolean, Bool(_)) | (Bool(_), Boolean) => Boolean,
            (Bool(a), Bool(b)) => Bool(a == b),
            (Atom(a), Atom(b)) => Bool(a == b),
            (Atom(_), Record(_)) | (Record(_), Atom(_)) => Bool(false),
            _ => return Err(BinaryOperatorExecutionError::Unimplemented),
        })
    }
}
