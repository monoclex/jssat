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

#[derive(Clone, Copy)]
pub enum ControlFlow {
    Fallthrough,
    // TODO: support `carry` for expression fun-ness
    Carry(RegisterId),
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
    ) -> EmitterIf<'borrow, 'b, P>
    where
        T: FnOnce(&mut Self) -> ControlFlow,
    {
        let condition = condition(self);

        EmitterIf::new(self, condition, then)
    }
}

pub struct EmitterIf<'borrow, 'builder, const P: usize> {
    emitter: &'borrow mut Emitter<'builder, P>,
    condition: RegisterId,
    control_flow: ControlFlow,
    true_clause: Option<DynBlockBuilder>,
    false_clause: Option<DynBlockBuilder>,
    fallthrough_clause: Option<crate::id::BlockId<crate::id::IrCtx>>,
    suppress_drop: bool,
}

impl<'bo, 'bu, const P: usize> EmitterIf<'bo, 'bu, P> {
    pub fn new<T>(emitter: &'bo mut Emitter<'bu, P>, condition: RegisterId, then: T) -> Self
    where
        T: FnOnce(&mut Emitter<'bu, P>) -> ControlFlow,
    {
        // create the clauses now so we have less ugly generics to deal with later
        // and also so we guarantee the order of functions being made (as emitterif drop
        // code runs before our drop code)
        let (true_clause, []) = emitter.function_builder.start_block();
        let mut true_clause = true_clause.into_dynamic();

        let (false_clause, []) = emitter.function_builder.start_block();
        let mut false_clause = false_clause.into_dynamic();

        // as a consequence of creating the true block here and wanting it logically
        // first in debug prints, we have to call the `then` condition now

        // TODO: the swaps are incorrect, but i'm waiting for unit tests to prove so
        std::mem::swap(&mut emitter.block_builder, &mut false_clause);
        let control_flow = then(emitter);
        std::mem::swap(&mut emitter.block_builder, &mut false_clause);

        Self {
            emitter,
            condition,
            control_flow,
            true_clause: Some(true_clause),
            false_clause: Some(false_clause),
            fallthrough_clause: None,
            suppress_drop: false,
        }
    }

    pub fn else_then<E>(self, else_then: E) -> EmitterIfElse<'bo, 'bu, E, P>
    where
        E: FnMut(&mut Emitter<'bu, P>) -> ControlFlow,
    {
        EmitterIfElse {
            emitter_if: Some(self),
            else_then,
        }
    }

    fn generate(&mut self) {
        // the `if` only cares about rerouting control flow to a "true" clause,
        // and a "false" clause depending upon the condition.

        // first, we must create the true and false clauses.
        //
        // we will create a *new* block to hold the contents of the true clause
        // (we have created this in the `new` method for reasons listed there)
        let mut true_clause = self.true_clause.take().expect("dont call drop twice");
        let true_clause_id = true_clause.id;

        // and consider the existing path to be the false clause (as that's
        // where we wish to continue execution
        let mut false_clause = self.false_clause.take().expect("dont call drop twice");
        let false_clause_id = false_clause.id;

        // now, we shall terminate the current path of execution, to go to either the
        // true or false clause
        std::mem::swap(&mut self.emitter.block_builder, &mut true_clause);
        let current_path = true_clause;
        let _true_clause = &mut self.emitter.block_builder;

        let termination = current_path.jmpif_dynargs(
            self.condition,
            true_clause_id,
            vec![],
            false_clause_id,
            vec![],
        );
        self.emitter.function_builder.end_block_dyn(termination);

        // now, we're in the true clause
        debug_assert_eq!(self.emitter.block_builder.id, true_clause_id);

        // we have already written the code for the true clause in the constructor of
        // `Emitter`, so we shall terminate the true clause
        std::mem::swap(&mut self.emitter.block_builder, &mut false_clause);
        let current_path = false_clause;
        let _false_clause = &mut self.emitter.block_builder;

        // used to configure where to fallthrough to, so that the else emitter can
        // fallthrough to after the end
        let fallthrough_id = self.fallthrough_clause.unwrap_or(false_clause_id);

        let termination = match self.control_flow {
            ControlFlow::Fallthrough => current_path.jmp_dynargs(fallthrough_id, vec![]),
            ControlFlow::Carry(value) => current_path.jmp_dynargs(fallthrough_id, vec![value]),
            ControlFlow::Return(value) => current_path.ret(value),
        };
        self.emitter.function_builder.end_block_dyn(termination);

        // now we have left the control flow state at `false_clause`, which is
        // where we wanted to end up
        debug_assert_eq!(self.emitter.block_builder.id, false_clause_id)
    }
}

impl<'bo, 'bu, const P: usize> Drop for EmitterIf<'bo, 'bu, P> {
    fn drop(&mut self) {
        if self.suppress_drop {
            return;
        }

        self.generate();
    }
}

pub struct EmitterIfElse<'borrow, 'builder, E, const P: usize>
where
    E: FnMut(&mut Emitter<'builder, P>) -> ControlFlow,
{
    emitter_if: Option<EmitterIf<'borrow, 'builder, P>>,
    else_then: E,
}

impl<'bo, 'bu, E, const P: usize> Drop for EmitterIfElse<'bo, 'bu, E, P>
where
    E: FnMut(&mut Emitter<'bu, P>) -> ControlFlow,
{
    fn drop(&mut self) {
        let mut emitter_if = self.emitter_if.take().expect("dont call drop twice");

        // we are going to use the `false_clause` to write the code that should happen
        // in the `else` branch, so we need to redirect the "fallthrough" branch to
        // after the else
        let emitter = &mut emitter_if.emitter;
        let (end_clause, []) = emitter.function_builder.start_block();
        let mut end_clause = end_clause.into_dynamic();
        let end_clause_id = end_clause.id;
        emitter_if.fallthrough_clause = Some(end_clause_id);

        emitter_if.generate();
        emitter_if.suppress_drop = true;

        let emitter = &mut emitter_if.emitter;

        // now write the else branch code
        let control_flow = (self.else_then)(emitter);

        // then terminate the else branch
        std::mem::swap(&mut emitter.block_builder, &mut end_clause);
        let _end_clause = &mut emitter.block_builder;
        let false_clause = end_clause;

        let finalized = match control_flow {
            ControlFlow::Fallthrough => false_clause.jmp_dynargs(end_clause_id, vec![]),
            ControlFlow::Carry(value) => false_clause.jmp_dynargs(end_clause_id, vec![value]),
            ControlFlow::Return(value) => false_clause.ret(value),
        };
        emitter.function_builder.end_block_dyn(finalized);

        // ensure we're set on the end clause now
        debug_assert_eq!(emitter.block_builder.id, end_clause_id);
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
        );

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

#[test]
pub fn emitter_emits_if_else_statements() {
    let mut builder = ProgramBuilder::new();
    builder.create_blank_entrypoint();
    let id = {
        let (check, [cond]) = builder.start_function();
        let mut e = Emitter::new(&mut builder, check);

        let in_if = e.program_builder.constant_str("in_if");
        let in_if = e.make_string(in_if);
        let in_else = e.program_builder.constant_str("in_else");
        let in_else = e.make_string(in_else);
        let done = e.program_builder.constant_str("done");
        let done = e.make_string(done);

        e.if_then(|_| cond, |_| ControlFlow::Return(Some(in_if)))
            .else_then(|_| ControlFlow::Return(Some(in_else)));

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
    assert_eq!(bytes, "in_else".as_bytes());
}

#[test]
pub fn emitter_emits_then_code_correctly() {
    let mut builder = ProgramBuilder::new();
    builder.create_blank_entrypoint();
    let id = {
        let (check, [cond]) = builder.start_function();
        let mut e = Emitter::new(&mut builder, check);

        let in_if = e.program_builder.constant_str("in_if");
        let in_if = e.make_string(in_if);
        let in_else = e.program_builder.constant_str("in_else");
        let in_else = e.make_string(in_else);

        e.if_then(
            |e| {
                e.comment("so");

                e.if_then(
                    |_| cond,
                    |e| {
                        e.comment("hi");
                        ControlFlow::Fallthrough
                    },
                );

                e.comment("bye");
                e.negate(cond)
            },
            |_| ControlFlow::Return(Some(in_if)),
        )
        .else_then(|_| ControlFlow::Return(Some(in_else)));

        e.finish(None)
    };

    let ir = builder.finish();
    println!("{}", crate::frontend::display_jssatir::display(&ir));
    let lifted = crate::lifted::lift(ir);
    let ext_fns = Default::default();
    let interpreter = Interpreter::new(&lifted, &ext_fns);

    let result = interpreter
        .execute_fn_id(id.id.map_context(), vec![Value::Boolean(true)])
        .unwrap();
    let value = result.unwrap();
    let bytes = value.try_into_bytes().unwrap();
    assert_eq!(bytes, "in_else".as_bytes());

    let result = interpreter
        .execute_fn_id(id.id.map_context(), vec![Value::Boolean(false)])
        .unwrap();
    let value = result.unwrap();
    let bytes = value.try_into_bytes().unwrap();
    assert_eq!(bytes, "in_if".as_bytes());
}
