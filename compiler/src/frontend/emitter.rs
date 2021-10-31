use super::builder::{DynBlockBuilder, FnSignature, FunctionBuilder, ProgramBuilder, RegisterId};
use derive_more::{Deref, DerefMut};

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
    Unreachable,
}

impl ControlFlow {
    fn is_fallthrough(&self) -> bool {
        matches!(self, ControlFlow::Fallthrough)
    }

    fn is_carry(&self) -> bool {
        matches!(self, ControlFlow::Carry(_))
    }

    fn is_return(&self) -> bool {
        matches!(self, ControlFlow::Return(_))
    }

    fn is_unreachable(&self) -> bool {
        matches!(self, ControlFlow::Unreachable)
    }
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

    pub fn load_constant(&mut self, payload: Vec<u8>) -> RegisterId {
        let constant = self.program_builder.constant(payload);
        self.make_string(constant)
    }

    pub fn load_str(&mut self, message: &str) -> RegisterId {
        let constant = self.program_builder.constant_str(message);
        self.make_string(constant)
    }

    pub fn load_str_utf16(&mut self, message: &str) -> RegisterId {
        let constant = self.program_builder.constant_str_utf16(message);
        self.make_string(constant)
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
    true_clause_id: crate::id::BlockId<crate::id::IrCtx>,
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
        let true_clause_id = true_clause.id;

        // as a consequence of creating the true block here and wanting it logically
        // first in debug prints, we have to generate the code for the `then` condition
        std::mem::swap(&mut emitter.block_builder, &mut true_clause);
        let control_flow = then(emitter);

        // at this point, we could potentially not be dealing with the original true
        // clause.
        std::mem::swap(&mut emitter.block_builder, &mut true_clause);

        let (false_clause, []) = emitter.function_builder.start_block();
        let false_clause = false_clause.into_dynamic();

        Self {
            emitter,
            condition,
            control_flow,
            true_clause: Some(true_clause),
            true_clause_id,
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
            suppress_drop: false,
        }
    }

    fn generate(&mut self) {
        // the `if` only cares about rerouting control flow to a "true" clause,
        // and a "false" clause depending upon the condition.

        // first, we must create the true and false clauses.
        //
        // we will create a *new* block to hold the contents of the true clause
        // (we have created this in the `new` method for reasons listed there)
        //
        // NOTE: `true_clause` may not be the ACTUAL true clause (see `new`).
        let mut true_clause = self.true_clause.take().expect("dont call drop twice");
        let true_clause_id = self.true_clause_id;

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
        let mut current_path = false_clause;
        let _false_clause = &mut self.emitter.block_builder;

        // used to configure where to fallthrough to, so that the else emitter can
        // fallthrough to after the end
        let fallthrough_id = self.fallthrough_clause.unwrap_or(false_clause_id);

        let termination = match self.control_flow {
            ControlFlow::Fallthrough => current_path.jmp_dynargs(fallthrough_id, vec![]),
            ControlFlow::Carry(value) => current_path.jmp_dynargs(fallthrough_id, vec![value]),
            ControlFlow::Return(value) => current_path.ret(value),
            ControlFlow::Unreachable => {
                current_path.unreachable();
                current_path.ret(None)
            }
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
    suppress_drop: bool,
}

impl<'bo, 'bu, E, const P: usize> EmitterIfElse<'bo, 'bu, E, P>
where
    E: FnMut(&mut Emitter<'bu, P>) -> ControlFlow,
{
    /// Generates the if and else code. This never needs to be called if you
    /// only want to generate the if and else code - that is done automatically
    /// on drop. This method is useful when you utilize [`ControlFlow::Carry`],
    /// in which after the if statement executes, the value is carried to the
    /// outside of the if statement.
    pub fn end(mut self) -> Option<RegisterId> {
        self.suppress_drop = true;
        self.generate()
    }

    fn generate(&mut self) -> Option<RegisterId> {
        let mut emitter_if = self.emitter_if.take().expect("dont call drop twice");

        // we are going to use the `false_clause` to write the code that should happen
        // in the `else` branch, so we need to redirect the "fallthrough" branch to
        // after the else
        let emitter = &mut emitter_if.emitter;
        let (end_clause, []) = emitter.function_builder.start_block();
        let mut end_clause = end_clause.into_dynamic();
        let end_clause_id = end_clause.id;
        emitter_if.fallthrough_clause = Some(end_clause_id);

        let path = emitter_if.emitter.block_builder.id;
        let true_clause_id = emitter_if.true_clause.as_ref().map(|f| f.id).unwrap();
        let false_clause_id = emitter_if.false_clause.as_ref().map(|f| f.id).unwrap();

        emitter_if.generate();
        emitter_if.suppress_drop = true;

        let emitter = &mut emitter_if.emitter;

        // now write the else branch code
        debug_assert_eq!(emitter.block_builder.id, false_clause_id);
        let control_flow = (self.else_then)(emitter);

        // now after we've ran the `else_then`, it could've modified the current path of
        // execution with if statement stuff. thus, we consider the current path the
        // false clause id
        let false_clause_id = emitter.block_builder.id;

        let carry_param = if control_flow.is_carry() || emitter_if.control_flow.is_carry() {
            Some(end_clause.add_parameter())
        } else {
            None
        };

        // then terminate the else branch
        std::mem::swap(&mut emitter.block_builder, &mut end_clause);
        let _end_clause = &mut emitter.block_builder;
        let mut false_clause = end_clause;

        debug_assert_eq!(false_clause_id, false_clause.id);

        let finalized = match control_flow {
            ControlFlow::Fallthrough => false_clause.jmp_dynargs(end_clause_id, vec![]),
            ControlFlow::Carry(value) => false_clause.jmp_dynargs(end_clause_id, vec![value]),
            ControlFlow::Return(value) => false_clause.ret(value),
            ControlFlow::Unreachable => {
                false_clause.unreachable();
                false_clause.ret(None)
            }
        };
        emitter.function_builder.end_block_dyn(finalized);

        // ensure we're set on the end clause now
        debug_assert_eq!(emitter.block_builder.id, end_clause_id);
        carry_param
    }
}

impl<'bo, 'bu, E, const P: usize> Drop for EmitterIfElse<'bo, 'bu, E, P>
where
    E: FnMut(&mut Emitter<'bu, P>) -> ControlFlow,
{
    fn drop(&mut self) {
        if self.suppress_drop {
            return;
        }

        self.generate();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::{InstErr, InstResult};
    use crate::interpreter::{Interpreter, Value};
    use Value::*;

    trait GetInner {
        fn get(self) -> Value;
    }

    impl GetInner for InstResult<Option<Value>> {
        #[track_caller]
        fn get(self) -> Value {
            self.expect("expected to execute function without errors")
                .expect("expected function to return value")
        }
    }

    fn create_interpreter<const P: usize>(
        program: fn(&mut Emitter<P>, [RegisterId; P]) -> Option<RegisterId>,
    ) -> impl Fn([Value; P]) -> InstResult<Option<Value>> {
        let mut builder = ProgramBuilder::new();
        builder.create_blank_entrypoint();

        let fn_signature = {
            let (func, params) = builder.start_function::<P>();
            let mut emitter = Emitter::new(&mut builder, func);

            let ret_val = program(&mut emitter, params);
            emitter.finish(ret_val)
        };

        let ir = builder.finish();
        println!("{}", crate::frontend::display_jssatir::display(&ir));
        let lifted = Box::leak(Box::new(crate::lifted::lift(ir)));
        // println!("{:#?}", lifted);

        let ext_fns = Box::leak(Box::new(Default::default()));
        let interpreter = Interpreter::new(lifted, ext_fns);

        // TODO: have lifted phase give information about ir -> fn id mappings
        move |args| interpreter.execute_fn_id(fn_signature.id.map_context(), args.to_vec())
    }

    #[track_caller]
    fn value_is_bytes(str: &str, value: Value) {
        let bytes = value.try_into_bytes().unwrap();
        assert_eq!(bytes, str.as_bytes());
    }

    #[test]
    pub fn if_then() {
        let run = create_interpreter(|e, [cond]| {
            e.if_then(|_| cond, |e| ControlFlow::Return(Some(e.load_str("in_if"))));
            Some(e.load_str("done"))
        });

        value_is_bytes("in_if", run([Boolean(true)]).get());
        value_is_bytes("done", run([Boolean(false)]).get());
    }

    #[test]
    pub fn if_then_else() {
        let run = create_interpreter(|e, [cond]| {
            e.if_then(|_| cond, |e| ControlFlow::Return(Some(e.load_str("in_if"))))
                .else_then(|e| ControlFlow::Return(Some(e.load_str("in_else"))));

            Some(e.load_str("done"))
        });

        value_is_bytes("in_if", run([Boolean(true)]).get());
        value_is_bytes("in_else", run([Boolean(false)]).get());
    }

    #[test]
    pub fn if_then_inner_if_then_outer() {
        let run = create_interpreter(|e, [cond1, cond2]| {
            e.if_then(
                |_| cond1,
                |e| {
                    e.if_then(|_| cond2, |e| ControlFlow::Return(Some(e.load_str("if2"))));

                    ControlFlow::Return(Some(e.load_str("if1")))
                },
            );

            Some(e.load_str("end"))
        });

        value_is_bytes("end", run([Boolean(false), Boolean(false)]).get());
        value_is_bytes("end", run([Boolean(false), Boolean(true)]).get());
        value_is_bytes("if1", run([Boolean(true), Boolean(false)]).get());
        value_is_bytes("if2", run([Boolean(true), Boolean(true)]).get());
    }

    #[test]
    pub fn if_then_inner_if_then_else_outer_else() {
        let run = create_interpreter(|e, [cond1, cond2]| {
            e.if_then(
                |_| cond1,
                |e| {
                    e.if_then(|_| cond2, |e| ControlFlow::Return(Some(e.load_str("ifif"))))
                        .else_then(|e| ControlFlow::Return(Some(e.load_str("ifelse"))));

                    ControlFlow::Return(Some(e.load_str("impossible_1")))
                },
            )
            .else_then(|e| {
                e.if_then(
                    |_| cond2,
                    |e| ControlFlow::Return(Some(e.load_str("elseif"))),
                )
                .else_then(|e| ControlFlow::Return(Some(e.load_str("elseelse"))));

                ControlFlow::Return(Some(e.load_str("impossible_2")))
            });

            Some(e.load_str("impossible_3"))
        });

        value_is_bytes("elseelse", run([Boolean(false), Boolean(false)]).get());
        value_is_bytes("elseif", run([Boolean(false), Boolean(true)]).get());
        value_is_bytes("ifelse", run([Boolean(true), Boolean(false)]).get());
        value_is_bytes("ifif", run([Boolean(true), Boolean(true)]).get());
    }

    #[test]
    pub fn if_then_else_end() {
        let run = create_interpreter(|e, [cond1]| {
            e.if_then(|_| cond1, |e| ControlFlow::Carry(e.load_str("if")))
                .else_then(|e| ControlFlow::Carry(e.load_str("else")))
                .end()
        });

        value_is_bytes("if", run([Boolean(true)]).get());
        value_is_bytes("else", run([Boolean(false)]).get());
    }

    #[test]
    #[should_panic]
    pub fn if_then_else_end_cannot_fallthrough_if_then() {
        let _run = create_interpreter(|e, [cond1]| {
            e.if_then(|_| cond1, |_| ControlFlow::Fallthrough)
                .else_then(|e| ControlFlow::Carry(e.load_str("else")))
                .end()
        });

        // value_is_bytes("else", run([Boolean(false)]).get());
        // assert!(matches!(
        //     run([Boolean(true)]),
        //     Err(InstErr::NotEnoughArgs(1, 0, _)),
        // ));
    }

    #[test]
    #[should_panic]
    pub fn if_then_else_end_cannot_fallthrough_else() {
        let _run = create_interpreter(|e, [cond1]| {
            e.if_then(|_| cond1, |e| ControlFlow::Carry(e.load_str("if")))
                .else_then(|_| ControlFlow::Fallthrough)
                .end()
        });

        // value_is_bytes("if", run([Boolean(true)]).get());
        // assert!(matches!(
        //     run([Boolean(false)]),
        //     Err(InstErr::NotEnoughArgs(1, 0, _)),
        // ));
    }

    #[test]
    pub fn if_then_incondition_if_then_else_end() {
        let run = create_interpreter(|e, [cond]| {
            e.if_then(
                |e| {
                    e.if_then(|_| cond, |e| ControlFlow::Carry(e.make_bool(true)))
                        .else_then(|e| ControlFlow::Carry(e.make_bool(false)))
                        .end()
                        .expect("carry should work")
                },
                |e| ControlFlow::Carry(e.load_str("if")),
            )
            .else_then(|e| ControlFlow::Carry(e.load_str("else")))
            .end()
        });

        value_is_bytes("if", run([Boolean(true)]).get());
        value_is_bytes("else", run([Boolean(false)]).get());
    }
}
