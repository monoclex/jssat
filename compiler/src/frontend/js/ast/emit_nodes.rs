use crate::frontend::builder::{DynBlockBuilder, RegisterId};

use super::parse_nodes::{self as js, Visitor};

pub struct NodeEmitter<'block> {
    block: &'block mut DynBlockBuilder,
    stack: Vec<ParseNode>,
    pub last_completed: Option<ParseNode>,
}

impl<'b> NodeEmitter<'b> {
    pub fn new(block: &'b mut DynBlockBuilder) -> Self {
        Self {
            block,
            stack: Vec::new(),
            last_completed: None,
        }
    }
}

pub struct ParseNode {
    pub parse_node: RegisterId,
    kind: js::ParseNodeKind,
    variant_idx: usize,
}

impl ParseNode {
    fn new(block: &mut DynBlockBuilder, kind: js::ParseNodeKind, variant_idx: usize) -> Self {
        let parse_node = block.record_new();

        Self {
            parse_node,
            kind,
            variant_idx,
        }
    }

    fn on_child_created(&mut self, block: &mut DynBlockBuilder, child: &ParseNode) {}

    fn finish(mut self, block: &mut DynBlockBuilder) -> Self {
        self
    }
}

impl<'b> Visitor for NodeEmitter<'b> {
    fn pre_visit(&mut self, kind: js::ParseNodeKind, variant_idx: usize) {
        println!("-> {:?}", kind);

        let node = ParseNode::new(self.block, kind, variant_idx);
        self.stack.push(node);
    }

    fn post_visit(&mut self) {
        let node = (self.stack.pop())
            .expect("post_visit should be called exactly same amount as pre_visit");

        println!("<- {:?}", node.kind);

        let node = node.finish(self.block);

        if let Some(parent) = self.stack.last_mut() {
            parent.on_child_created(self.block, &node);
        }

        self.last_completed = Some(node);
    }
}
