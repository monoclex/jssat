mod frontend_pseudo;

// #[cfg(not(feature = "link-swc"))]
pub use frontend_pseudo::traverse;
use swc_ecmascript::parser::PResult;

use crate::{frontend::js::ast::parse_nodes::Dealer, isa::InternalSlot};

use self::{ast::parse_nodes::Visitor, ecmascript::ECMA262Methods};

use super::builder::{DynBlockBuilder, ProgramBuilder, RegisterId};

pub mod ast;
pub mod ecmascript;
pub mod hosts;

pub struct JavaScriptFrontend<'program> {
    program: &'program mut ProgramBuilder,
    pub ecma_methods: ECMA262Methods,
}

impl<'p> JavaScriptFrontend<'p> {
    /// Creates a new [`JavaScriptFrontend`], which creates ECMAScript methods
    /// within the [`ProgramBuilder`] specified.
    pub fn new(program: &'p mut ProgramBuilder) -> Self {
        let ecma_methods = ECMA262Methods::new(program);

        Self {
            program,
            ecma_methods,
        }
    }

    /// Parses some JavaScript source text, and then produces JSSAT IR which
    /// will perform the same computation that the JavaScript does. This expects
    /// `block` to be obtained from within the same ProgramBuilder specified in
    /// `new`.
    ///
    /// # Returns
    ///
    /// If parsing the JavaScript source_text is unsuccessful, this returns an
    /// [`Err`].
    ///
    /// If parsing the JavaScript source_text is successful, contained within
    /// the [`Ok`] variant is the [`RegisterId`] which holds the completion
    /// record returned from the ECMAScript method ScriptEvaluation.
    pub fn parse(
        &mut self,
        source_text: &str,
        block: &mut DynBlockBuilder,
        host_environment: &mut impl hosts::HostEnvironment,
    ) -> PResult<RegisterId> {
        let script = ast::parse_script(source_text)?;

        let dealer = Dealer::new(self.program);

        let entry_parse_node =
            ast::emit_nodes(self.program, block, &self.ecma_methods, &dealer, |v| {
                v.visit_script(&script)
            });

        let threaded_global = block.record_new();
        block.call(
            self.ecma_methods.InitializeJSSATThreadedGlobal,
            [threaded_global],
        );

        block.call(
            self.ecma_methods.InitializeHostDefinedRealm,
            [threaded_global],
        );

        // for some reason, ecmascript spec doesn't have initializehostdefinedrealm
        // return the execution context :/
        let exec_ctx = get_execution_context(block, threaded_global);

        // the last step in "InitializeHostDefinedRealm" states:
        // 11. Create any host-defined global object properties on globalObj.
        let realm = block.record_get_slot(exec_ctx, InternalSlot::Realm);
        let global_object = block.record_get_slot(realm, InternalSlot::GlobalObject);

        let hook = hosts::HostHookState {
            ecma_methods: &self.ecma_methods,
            program: self.program,
            threaded_global,
            block,
            realm,
            global_object,
            parse_nodes: &dealer,
        };

        host_environment.inject(hook);

        // we don't care about the parameters we pass null to for ParseScript
        let null = block.make_atom(self.ecma_methods.atoms.Null);

        let script_context = block.call_with_result(
            self.ecma_methods.ParseScript,
            [threaded_global, null, realm, null, entry_parse_node],
        );

        let result = block.call_with_result(
            self.ecma_methods.ScriptEvaluation,
            [threaded_global, script_context],
        );

        return Ok(result);

        fn get_execution_context(
            block: &mut DynBlockBuilder,
            threaded_global: RegisterId,
        ) -> RegisterId {
            let exec_ctx_stack =
                block.record_get_slot(threaded_global, InternalSlot::JSSATExecutionContextStack);
            let last = block.list_len(exec_ctx_stack);
            let one = block.make_number_decimal(1);
            let neg_one = block.negate(one);
            let last_minus_one = block.add(last, neg_one);
            block.list_get(exec_ctx_stack, last_minus_one)
        }
    }
}
