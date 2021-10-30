use super::builder::{DynBlockBuilder, FnSignature, FunctionBuilder, ProgramBuilder, RegisterId};
use crate::{
    frontend::ir::Function,
    interpreter::{Interpreter, Value},
};
use derive_more::{Deref, DerefMut};
use rustc_hash::FxHashMap;

// pub use super::builder::*;

/// The emitter struct is a wrapper over the builder API to provide more natural
/// translation of code. Rather than expose the underlying block structure, the
/// emitter API exposes simple constructs such as while loops and if statements.
#[derive(Deref, DerefMut)]
pub struct Emitter<'builder, const P: usize> {
    program_builder: &'builder mut ProgramBuilder,
    function_builder: FunctionBuilder<P>,
    #[deref]
    #[deref_mut]
    block_builder: DynBlockBuilder,
}

pub enum ControlFlow {
    Fallthrough,
    Return(Option<RegisterId>),
}

impl<'b, const P: usize> Emitter<'b, P> {
    pub fn new(
        program_builder: &'b mut ProgramBuilder,
        mut function_builder: FunctionBuilder<P>,
    ) -> Self {
        let block_builder = function_builder.start_block_main().into_dynamic();

        Self {
            program_builder,
            function_builder,
            block_builder,
        }
    }

    pub fn finish(mut self, return_value: Option<RegisterId>) -> FnSignature<P> {
        self.function_builder
            .end_block_dyn(self.block_builder.ret(return_value));

        self.program_builder.end_function(self.function_builder)
    }

    pub fn if_then<'borrow, T>(
        &'borrow mut self,
        condition: impl FnOnce(&mut Self) -> RegisterId,
        then: T,
    ) -> EmitterIf<'borrow, 'b, T, P>
    where
        T: FnOnce(&mut Self) -> ControlFlow,
    {
        let condition = condition(self);

        EmitterIf {
            emitter: self,
            condition,
            then,
        }
    }
}

pub struct EmitterIf<'borrow, 'builder, T, const P: usize> {
    emitter: &'borrow mut Emitter<'builder, P>,
    condition: RegisterId,
    then: T,
}

impl<'bo, 'bu, T, const P: usize> EmitterIf<'bo, 'bu, T, P> {
    pub fn else_then<E>(self, else_then: E) -> EmitterIfElse<'bo, 'bu, T, E, P>
    where
        E: FnOnce(&mut Emitter<'bu, P>) -> ControlFlow,
    {
        EmitterIfElse {
            emitter_if: self,
            else_then,
        }
    }
}

impl<'bo, 'bu, T, const P: usize> Drop for EmitterIf<'bo, 'bu, T, P> {
    fn drop(&mut self) {
        println!("if dropping");
    }
}

pub struct EmitterIfElse<'borrow, 'builder, T, E, const P: usize> {
    emitter_if: EmitterIf<'borrow, 'builder, T, P>,
    else_then: E,
}

impl<'bo, 'bu, T, E, const P: usize> Drop for EmitterIfElse<'bo, 'bu, T, E, P> {
    fn drop(&mut self) {
        println!("elseif dropping");
    }
}

#[test]
pub fn emitter_emits_if_statements() {
    let mut builder = ProgramBuilder::new();
    builder.create_blank_entrypoint();
    let id = {
        let (check, [cond]) = builder.start_function();
        let mut e = Emitter::new(&mut builder, check);

        e.if_then(
            |_| cond,
            |e| {
                let in_if = e.program_builder.constant_str("in_if");
                ControlFlow::Return(Some(e.make_string(in_if)))
            },
        )
        .else_then(|e| todo!());

        let done = e.program_builder.constant_str("done");
        let done = e.make_string(done);
        e.finish(Some(done))
    };

    let ir = builder.finish();
    let lifted = crate::lifted::lift(ir);
    let ext_fns = Default::default();
    let interpreter = Interpreter::new(&lifted, &ext_fns);

    let result = interpreter
        .execute_fn_id(id.id.map_context(), vec![Value::Boolean(true)])
        .unwrap();
    let value = result.unwrap();
    let bytes = value.try_into_bytes().unwrap();
    assert_eq!(bytes, "in_if".as_bytes());

    let result = interpreter
        .execute_fn_id(id.id.map_context(), vec![Value::Boolean(false)])
        .unwrap();
    let value = result.unwrap();
    let bytes = value.try_into_bytes().unwrap();
    assert_eq!(bytes, "done".as_bytes());
}
