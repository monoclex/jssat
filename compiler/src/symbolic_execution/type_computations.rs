use crate::isa::BinaryOperator;
use thiserror::Error;

use super::types::{RegisterType, TypeBag};

impl BinaryOperator {
    pub fn make_executor<'t>(&self, types: &'t TypeBag) -> BinOpExecutor<'t> {
        BinOpExecutor { types, op: *self }
    }
}

pub struct BinOpExecutor<'a> {
    types: &'a TypeBag,
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
        &self,
        lhs: RegisterType,
        rhs: RegisterType,
    ) -> Result<RegisterType, BinOpExecErr> {
        let result = match self.op {
            BinaryOperator::Add => self.add(lhs, rhs),
            BinaryOperator::And => self.and(lhs, rhs),
            BinaryOperator::Or => self.or(lhs, rhs),
            BinaryOperator::Equals => self.equals(lhs, rhs),
            BinaryOperator::LessThan => todo!(),
            _ => Err(BinaryOperatorExecutionError::Unimplemented),
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
        &self,
        lhs: RegisterType,
        rhs: RegisterType,
    ) -> Result<RegisterType, BinaryOperatorExecutionError> {
        Ok(match (lhs, rhs) {
            (RegisterType::Bytes, RegisterType::Bytes)
            | (RegisterType::Byts(_), RegisterType::Bytes)
            | (RegisterType::Bytes, RegisterType::Byts(_)) => RegisterType::Bytes,
            (RegisterType::Byts(a), RegisterType::Byts(b)) => RegisterType::Byts(todo!()),
            _ => return Err(BinaryOperatorExecutionError::Unimplemented),
        })
    }

    fn and(
        &self,
        lhs: RegisterType,
        rhs: RegisterType,
    ) -> Result<RegisterType, BinaryOperatorExecutionError> {
        Ok(match (lhs, rhs) {
            (RegisterType::Boolean, RegisterType::Boolean)
            | (RegisterType::Boolean, RegisterType::Bool(_))
            | (RegisterType::Bool(_), RegisterType::Boolean) => RegisterType::Boolean,
            (RegisterType::Bool(a), RegisterType::Bool(b)) => RegisterType::Bool(a && b),
            _ => return Err(BinaryOperatorExecutionError::Unimplemented),
        })
    }

    fn or(
        &self,
        lhs: RegisterType,
        rhs: RegisterType,
    ) -> Result<RegisterType, BinaryOperatorExecutionError> {
        Ok(match (lhs, rhs) {
            (RegisterType::Boolean, RegisterType::Boolean)
            | (RegisterType::Boolean, RegisterType::Bool(_))
            | (RegisterType::Bool(_), RegisterType::Boolean) => RegisterType::Boolean,
            (RegisterType::Bool(a), RegisterType::Bool(b)) => RegisterType::Bool(a || b),
            _ => return Err(BinaryOperatorExecutionError::Unimplemented),
        })
    }

    fn equals(
        &self,
        lhs: RegisterType,
        rhs: RegisterType,
    ) -> Result<RegisterType, BinaryOperatorExecutionError> {
        Ok(match (lhs, rhs) {
            (RegisterType::Number, RegisterType::Number)
            | (RegisterType::Int(_), RegisterType::Number)
            | (RegisterType::Number, RegisterType::Int(_)) => RegisterType::Boolean,
            (RegisterType::Int(a), RegisterType::Int(b)) => RegisterType::Bool(a == b),
            (RegisterType::Bytes, RegisterType::Bytes)
            | (RegisterType::Byts(_), RegisterType::Bytes)
            | (RegisterType::Bytes, RegisterType::Byts(_)) => RegisterType::Boolean,
            (RegisterType::Byts(a), RegisterType::Byts(b)) => {
                RegisterType::Bool(self.types.unintern_const(a) == self.types.unintern_const(b))
            }
            (RegisterType::Boolean, RegisterType::Boolean)
            | (RegisterType::Boolean, RegisterType::Bool(_))
            | (RegisterType::Bool(_), RegisterType::Boolean) => RegisterType::Boolean,
            (RegisterType::Bool(a), RegisterType::Bool(b)) => RegisterType::Bool(a == b),
            (RegisterType::Trivial(a), RegisterType::Trivial(b)) => RegisterType::Bool(a == b),
            (RegisterType::Trivial(_), RegisterType::Record(_))
            | (RegisterType::Record(_), RegisterType::Trivial(_)) => RegisterType::Bool(false),
            _ => return Err(BinaryOperatorExecutionError::Unimplemented),
        })
    }
}
