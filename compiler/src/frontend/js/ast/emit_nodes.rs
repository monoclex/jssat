use std::convert::TryInto;

use rustc_hash::FxHashMap;

use crate::{
    frontend::{
        builder::{DynBlockBuilder, FnSignature, ProgramBuilder, RegisterId},
        js::ecmascript::ECMA262Methods,
    },
    isa::Atom,
    UnwrapNone,
};

use super::parse_nodes::{self as js, Visitor};

// TODO(refactor): maybe separate this out somewhere else? it's simpler being
//   in one place though
fn emit_virt_overrides(
    parse_node: RegisterId,
    kind: js::ParseNodeKind,
    idx: usize,
    block: &mut DynBlockBuilder,
    slot: Atom,
    m: &ECMA262Methods,
) -> bool {
    use js::ParseNodeKind::*;

    #[rustfmt::skip]
    let function = match (kind, idx) {
        (IdentifierReference, _) => m.Evaluation_IdentifierReference,
        (CallExpression, _) => m.Evaluation_CallExpression,
        (Arguments, 0) => m.ArgumentListEvaluation,
        (ArgumentList, _) => m.ArgumentListEvaluation,
        (Literal, _) => m.Evaluation_Literal,
        (FunctionDeclaration, _) => m.FunctionDeclaration_Evaluation,
        (StatementList, 1) => m.StatementList_Evaluation,
        (FunctionStatementList, 0) => m.FunctionDeclaration_Evaluation,
        _ => return false,
    };

    let fn_ptr = block.make_fnptr(function.id);
    block.record_set_atom(parse_node, slot, fn_ptr);

    true
}

pub struct NodeEmitter<'scope> {
    block: &'scope mut DynBlockBuilder,
    program: &'scope mut ProgramBuilder,
    stack: Vec<ParseNode>,
    pub last_completed: Option<ParseNode>,
    simple_fns: FxHashMap<Atom, FnSignature<2>>,
    ecma_methods: &'scope ECMA262Methods,
    dealer: &'scope js::Dealer,
    slots: [Atom; 3],
    identifier_name_data: Atom,
    string_literal_data: Atom,
}

impl<'s> NodeEmitter<'s> {
    pub fn new(
        block: &'s mut DynBlockBuilder,
        program: &'s mut ProgramBuilder,
        ecma_methods: &'s ECMA262Methods,
        dealer: &'s js::Dealer,
    ) -> Self {
        let simple_fns = Self::generate_simple_fns(
            ecma_methods.atoms.JSSATParseNodeSlot1,
            ecma_methods.atoms.JSSATParseNodeEvaluate,
            program,
        );

        let slots = [
            ecma_methods.atoms.JSSATParseNodeSlot1,
            ecma_methods.atoms.JSSATParseNodeSlot2,
            ecma_methods.atoms.JSSATParseNodeSlot3,
        ];

        Self {
            block,
            program,
            stack: Vec::new(),
            last_completed: None,
            simple_fns,
            ecma_methods,
            dealer,
            slots,
            identifier_name_data: ecma_methods.atoms.JSSATParseNode_Identifier_StringValue,
            string_literal_data: ecma_methods.atoms.JSSATParseNode_StringLiteral_StringValue,
        }
    }

    /// Generates the "simple" ECMAScript functions - the functions that are
    /// defined by the following catch-most clause:
    ///
    /// > For example, assume that some algorithm has a step of the form:
    /// > “Return the result of evaluating Block” and that there is a
    /// > production:
    /// >
    /// > ```text
    /// > Block :
    /// >     { StatementList }
    /// > ```
    /// >
    /// > but the *Evaluation* operation does not associate an algorithm with
    /// > that production. In that case, the *Evaluation* operation
    /// > implicitly includes > an association of the form:
    /// >
    /// > > Runtime Semantics:
    /// > >
    /// > > ```text
    /// > > EvaluationBlock :
    /// > >     { StatementList }
    /// > > ```
    /// > > 1.  Return the result of evaluating StatementList.
    fn generate_simple_fns(
        node_slot: Atom,
        eval_slot: Atom,
        program: &mut ProgramBuilder,
    ) -> FxHashMap<Atom, FnSignature<2>> {
        let mut map = FxHashMap::default();

        let slot = eval_slot;
        let (mut f, [threaded_global, x]) = program.start_function();

        let mut e = f.start_block_main();
        let next = e.record_get_atom(x, node_slot);
        let fn_ptr = e.record_get_atom(next, slot);
        let result = e.call_virt_with_result(fn_ptr, [threaded_global, next]);
        f.end_block(e.ret(Some(result)));

        let signature = program.end_function(f);

        map.insert(slot, signature).expect_free();

        map
    }
}

pub struct ParseNode {
    pub parse_node: RegisterId,
    kind: js::ParseNodeKind,
    variant_idx: usize,
    parse_node_slot: usize,
}

impl ParseNode {
    fn new(emitter: &mut NodeEmitter, kind: js::ParseNodeKind, variant_idx: usize) -> Self {
        let block = &mut emitter.block;
        let parse_node = block.record_new();

        let node_kind = block.make_atom(emitter.dealer.translate(kind));
        let parse_node_kind_atom = emitter.ecma_methods.atoms.JSSATParseNodeKind;
        block.record_set_atom(parse_node, parse_node_kind_atom, node_kind);

        let variant_i64: i64 = variant_idx.try_into().unwrap();
        let variant_kind = block.make_number_decimal(variant_i64);
        let variant_atom = emitter.ecma_methods.atoms.JSSATParseNodeVariant;
        block.record_set_atom(parse_node, variant_atom, variant_kind);

        Self {
            parse_node,
            kind,
            variant_idx,
            parse_node_slot: 0,
        }
    }

    fn on_child_created(&mut self, block: &mut DynBlockBuilder, slots: &[Atom], child: &ParseNode) {
        let slot = self.parse_node_slot;
        self.parse_node_slot += 1;

        let slot = *slots.get(slot).expect("expected slot");

        block.record_set_atom(self.parse_node, slot, child.parse_node);
    }

    fn finish(
        self,
        block: &mut DynBlockBuilder,
        simple_fns: &FxHashMap<Atom, FnSignature<2>>,
        ecma_methods: &ECMA262Methods,
    ) -> Self {
        for (slot, fn_id) in simple_fns {
            let did_emit_overrides = emit_virt_overrides(
                self.parse_node,
                self.kind,
                self.variant_idx,
                block,
                *slot,
                ecma_methods,
            );

            // if we don't have a custom implementation of a runtime semantic,
            // emit the default function
            if !did_emit_overrides {
                let virt_fn = block.make_fnptr(fn_id.id);
                block.record_set_atom(self.parse_node, *slot, virt_fn);
            }
        }

        self
    }
}

impl<'b> Visitor for NodeEmitter<'b> {
    fn pre_visit(&mut self, kind: js::ParseNodeKind, variant_idx: usize) {
        println!("-> {:?}", kind);

        let node = ParseNode::new(self, kind, variant_idx);
        self.stack.push(node);
    }

    fn post_visit(&mut self) {
        let node = (self.stack.pop())
            .expect("post_visit should be called exactly same amount as pre_visit");

        println!("<- {:?}", node.kind);

        let node = node.finish(self.block, &self.simple_fns, self.ecma_methods);

        if let Some(parent) = self.stack.last_mut() {
            parent.on_child_created(self.block, &self.slots, &node);
        }

        self.last_completed = Some(node);
    }

    fn visit_identifier_name(&mut self, node: &js::IdentifierName) {
        let parse_node = self.stack.last_mut().expect("it");

        let string = node.0.clone();

        let constant = self.program.constant_str_utf16(string);
        let string = self.block.make_string(constant);

        self.block
            .record_set_atom(parse_node.parse_node, self.identifier_name_data, string);
    }

    fn visit_string_literal(&mut self, node: &js::StringLiteral) {
        let parse_node = self.stack.last_mut().expect("it");

        let string = node.0.clone();

        let constant = self.program.constant_str_utf16(string);
        let string = self.block.make_string(constant);

        self.block
            .record_set_atom(parse_node.parse_node, self.string_literal_data, string);
    }

    // rust doesn't have calling `super` so we have to sort of implement `visit_x`
    // instead of `visit_impl_x`

    fn visit_function_declaration(&mut self, node: &js::FunctionDeclaration) {
        self.pre_visit(js::ParseNodeKind::FunctionDeclaration, node.variant_idx());

        self.visit_impl_function_declaration(node);

        let parse_node = self.stack.last_mut().expect("it");

        // TODO: uhh actually get the source text of a function lol
        let source_text = format!("temporary jank function string: {:?}", node);
        let source_text = self.program.constant_str_utf16(source_text);
        let source_text = self.block.make_string(source_text);

        self.block.record_set_atom(
            parse_node.parse_node,
            self.ecma_methods.atoms.JSSATParseNodeSourceText,
            source_text,
        );

        self.post_visit();
    }

    // here we visit `cover`ed expressions and automatically parse them as alternate
    // options that way, we put into JSSATParseNodeSlot the alternatives
    // so when ecmascript instructions say "get the X covered by Y" we can load it

    // TODO: look into whether or not we can reuse the parse rather than parse
    //        duplicatedly i feel like we might run into weirdness if we parse as
    //        duplicately but w/e
    //
    //        by duplicately i mean like generating new code when re-parsing to
    //        satsify the "covered by" thing. e.g. in the below function, we clone
    //        member expression and arguments (that is fine) but then we visit it,
    //        potentially regenerating code when we should be able to re-use the
    //        already generated parse nodes

    fn visit_cover_call_expression_and_async_arrow_head(
        &mut self,
        node @ js::CoverCallExpressionAndAsyncArrowHead::Variant0(member_expression, arguments): &js::CoverCallExpressionAndAsyncArrowHead,
    ) {
        self.pre_visit(js::ParseNodeKind::CoverCallExpressionAndAsyncArrowHead, 0);

        // slot 1: member expressoin
        // slot 2: arguments
        self.visit_impl_cover_call_expression_and_async_arrow_head(node);

        // slot 3: CallMemberExpression
        let call_member_expr =
            js::CallMemberExpression::Variant0(member_expression.clone(), arguments.clone());

        self.visit_call_member_expression(&call_member_expr);

        self.post_visit();
    }
}
