use std::{array::IntoIter, convert::TryInto};

use rustc_hash::FxHashMap;

use crate::{
    frontend::{
        builder::{DynBlockBuilder, FnSignature, ProgramBuilder, RegisterId},
        js::ecmascript::ECMA262Methods,
    },
    isa::{InternalSlot, TrivialItem},
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
    slot: InternalSlot,
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
        _ => return false,
    };

    let fn_ptr = block.make_fnptr(function.id);
    block.record_set_slot(parse_node, slot, fn_ptr);

    true
}

pub struct NodeEmitter<'scope> {
    block: &'scope mut DynBlockBuilder,
    program: &'scope mut ProgramBuilder,
    stack: Vec<ParseNode>,
    pub last_completed: Option<ParseNode>,
    simple_fns: FxHashMap<InternalSlot, FnSignature<2>>,
    ecma_methods: &'scope ECMA262Methods,
    dealer: &'scope js::Dealer,
}

impl<'s> NodeEmitter<'s> {
    pub fn new(
        block: &'s mut DynBlockBuilder,
        program: &'s mut ProgramBuilder,
        ecma_methods: &'s ECMA262Methods,
        dealer: &'s js::Dealer,
    ) -> Self {
        let simple_fns = Self::generate_simple_fns(program);

        Self {
            block,
            program,
            stack: Vec::new(),
            last_completed: None,
            simple_fns,
            ecma_methods,
            dealer,
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
        program: &mut ProgramBuilder,
    ) -> FxHashMap<InternalSlot, FnSignature<2>> {
        let mut map = FxHashMap::default();

        for slot in IntoIter::new(RUNTIME_SEMANTICS) {
            let (mut f, [threaded_global, x]) = program.start_function();

            let mut e = f.start_block_main();
            let next = e.record_get_slot(x, InternalSlot::JSSATParseNodeSlot1);
            let fn_ptr = e.record_get_slot(next, slot);
            let result = e.call_virt_with_result(fn_ptr, [threaded_global, next]);
            f.end_block(e.ret(Some(result)));

            let signature = program.end_function(f);

            map.insert(slot, signature).expect_free();
        }

        map
    }
}

const PARSE_NODE_SLOTS: [InternalSlot; 6] = [
    InternalSlot::JSSATParseNodeSlot1,
    InternalSlot::JSSATParseNodeSlot2,
    InternalSlot::JSSATParseNodeSlot3,
    InternalSlot::JSSATParseNodeSlot4,
    InternalSlot::JSSATParseNodeSlot5,
    InternalSlot::JSSATParseNodeSlot6,
];

const RUNTIME_SEMANTICS: [InternalSlot; 1] = [InternalSlot::JSSATParseNodeEvaluate];

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

        let trivial_kind = block.make_atom(emitter.dealer.translate(kind));
        block.record_set_slot(parse_node, InternalSlot::JSSATParseNodeKind, trivial_kind);

        let variant_i64: i64 = variant_idx.try_into().unwrap();
        let variant_kind = block.make_number_decimal(variant_i64);
        block.record_set_slot(
            parse_node,
            InternalSlot::JSSATParseNodeVariant,
            variant_kind,
        );

        Self {
            parse_node,
            kind,
            variant_idx,
            parse_node_slot: 0,
        }
    }

    fn on_child_created(&mut self, block: &mut DynBlockBuilder, child: &ParseNode) {
        let slot = self.parse_node_slot;
        self.parse_node_slot += 1;

        let slot = *PARSE_NODE_SLOTS
            .get(slot)
            .expect("expected slot (increase -> 6+)");

        block.record_set_slot(self.parse_node, slot, child.parse_node);
    }

    fn finish(
        self,
        block: &mut DynBlockBuilder,
        simple_fns: &FxHashMap<InternalSlot, FnSignature<2>>,
        ecma_methods: &ECMA262Methods,
    ) -> Self {
        debug_assert!({
            IntoIter::new(RUNTIME_SEMANTICS).all(|slot| simple_fns.contains_key(&slot))
        });

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
                block.record_set_slot(self.parse_node, *slot, virt_fn);
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
            parent.on_child_created(self.block, &node);
        }

        self.last_completed = Some(node);
    }

    fn visit_identifier_name(&mut self, node: &js::IdentifierName) {
        let parse_node = self.stack.last_mut().expect("it");

        let string = node.0.clone();

        let constant = self.program.constant_str_utf16(string);
        let string = self.block.make_string(constant);

        self.block.record_set_slot(
            parse_node.parse_node,
            InternalSlot::JSSATParseNode_Identifier_StringValue,
            string,
        );
    }

    fn visit_string_literal(&mut self, node: &js::StringLiteral) {
        let parse_node = self.stack.last_mut().expect("it");

        let string = node.0.clone();

        let constant = self.program.constant_str_utf16(string);
        let string = self.block.make_string(constant);

        self.block.record_set_slot(
            parse_node.parse_node,
            InternalSlot::JSSATParseNode_StringLiteral_StringValue,
            string,
        );
    }

    // rust doesn't have calling `super` so we have to sort of implement `visit_x`
    // instead of `visit_impl_x`

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
