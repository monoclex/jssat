use rustc_hash::FxHashMap;

use jssat_ir::{
    id::Counter,
    lifted::{ExternalFunctionId, LiftedProgram},
    UnwrapNone,
};

use super::{ExtFnImpl, InstResult, Interpreter, Value};

pub struct InterpreterBuilder<'program> {
    program: &'program LiftedProgram,
    ext_fns: FxHashMap<ExternalFunctionId, ExtFnImpl>,
    counter: Counter<ExternalFunctionId>,
}

impl<'p> InterpreterBuilder<'p> {
    pub fn new(program: &'p LiftedProgram) -> Self {
        Self {
            program,
            ext_fns: Default::default(),
            counter: Default::default(),
        }
    }

    fn gen_ext_id(&self) -> ExternalFunctionId {
        let id = self.counter.next();

        match self.program.external_functions.contains_key(&id) {
            // lol im lazy, unlikely to be hit anyways
            true => self.gen_ext_id(),
            _ => id,
        }
    }

    pub fn with_ext_fn(
        &mut self,
        function: fn(Vec<Value>) -> InstResult<Option<Value>>,
        id: &mut Option<ExternalFunctionId>,
    ) -> &mut Self {
        let ext_fn_id = self.gen_ext_id();
        let ext_fn_impl = ExtFnImpl { function };

        self.ext_fns.insert(ext_fn_id, ext_fn_impl).expect_free();
        *id = Some(ext_fn_id);

        self
    }

    pub fn build(&self) -> Interpreter {
        Interpreter::new(self.program, &self.ext_fns)
    }
}
