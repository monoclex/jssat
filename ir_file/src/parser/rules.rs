//! Contains rewrite rules for nodes.
//!
//! In an IR file, helpers are defined at the top which behave as node rewriters
//! - if the first rule is satisfied, it will be rewritten to the expression
//!   specified.
//!
//! For simplicity, rule rewriting only acts on single nodes.
//!
//! # Examples
//!
//! ```lisp
//! ; the following pattern
//!
//! (def
//!   hello-world
//!   goodbye-world)
//!
//! ; will apply itself to the following node tree
//!
//! (something something (more (hello-world) things))
//!
//! ; to generate
//!
//! (something something (more (goodbye-world) things))
//! ```
//!
//! Patterns also have parameters
//!
//! ```lisp
//! (def
//!   (repeat3 :x)
//!   (:x :x :x))
//!
//! (i like (repeat3 candy) !!!)
//!
//! (i like (candy candy candy) !!!)
//! ```

use std::ops::Deref;

use rustc_hash::FxHashMap;

use super::*;

pub fn apply_rule_recursively(rule: &Node, generate: &Node, node: Node) -> Node {
    match apply_rule(rule, generate, node) {
        Node::Parent(children, span) => Node::Parent(
            children
                .into_iter()
                .map(|child| apply_rule_recursively(rule, generate, child))
                .collect(),
            span,
        ),
        other => other,
    }
}

pub fn apply_rule(rule: &Node, generate: &Node, node: Node) -> Node {
    if !matches_rule(rule, &node) {
        return node;
    }

    let mut rule_values = FxHashMap::default();
    get_rule_params(rule, &node, &mut rule_values);
    apply_rule_inner(generate, &rule_values)
}

fn get_rule_params<'data>(
    rule: &Node,
    node: &'data Node,
    rule_values: &mut FxHashMap<String, &'data Node>,
) {
    match (rule, node) {
        (Node::Atom(name, _), node) => {
            rule_values.insert(name.clone(), node);
        }
        (Node::Parent(rule_children, _), Node::Parent(node_children, _)) => {
            for (rule, node) in rule_children.iter().zip(node_children) {
                get_rule_params(rule, node, rule_values);
            }
        }
        _ => {}
    }
}

fn apply_rule_inner(generate: &Node, params: &FxHashMap<String, &Node>) -> Node {
    // TODO(correctness): when producing a node from a parameter, the span
    //     information should try to be kept correct. rewriting the span information
    //     would help do so not sure if that's even possible or not tho
    match generate {
        Node::Atom(x, _) => params.get(x).unwrap().deref().clone(),
        Node::Parent(x, span) => {
            let mut children = Vec::new();

            for generate in x {
                children.push(apply_rule_inner(generate, params));
            }

            Node::Parent(children, *span)
        }
        other => other.clone(),
    }
}

fn matches_rule(rule: &Node, node: &Node) -> bool {
    match (rule, node) {
        (Node::Atom(_, _), _) => true,
        (Node::Word(a, _), Node::Word(b, _)) | (Node::String(a, _), Node::String(b, _)) => a == b,
        (Node::Number(a, _), Node::Number(b, _)) => a == b,
        (Node::Parent(a, _), Node::Parent(b, _)) if a.len() == b.len() => {
            a.iter().zip(b).all(|(node, rule)| matches_rule(node, rule))
        }
        _ => false,
    }
}

#[cfg(test)]
mod rule_tests {
    use super::*;
    use crate::parser::node::parse_to_nodes;

    fn doesnt_match(rule: &str, code: &str) {
        let mut rule_nodes = parse_to_nodes(rule);
        let rule = rule_nodes.remove(0);

        let node = parse_to_nodes(code).remove(0);
        assert!(!matches_rule(&node, &rule))
    }

    #[test]
    fn doesnt_match_on_same_type_different_data() {
        doesnt_match("word x", "also_word");
        doesnt_match("6.9 x", "4.2");
        doesnt_match(r#""string" x"#, r#""also string""#);
        doesnt_match("(child) x", "(more children)");
        doesnt_match("(child) x", "(same_child_count)");
    }

    fn matches_itself(rule_and_code: &str) {
        let mut rule_nodes = parse_to_nodes(&format!("{} {}", rule_and_code, rule_and_code));
        let rule = rule_nodes.remove(0);
        let generate = rule_nodes.remove(0);

        let node = parse_to_nodes(rule_and_code).remove(0);
        assert!(matches_rule(&node, &rule), "rule should match");
        let result = apply_rule(&rule, &generate, node.clone());

        assert_eq!(node, result);
    }

    #[test]
    fn matches_on_same_thing() {
        matches_itself("word");
        matches_itself("6.9");
        matches_itself(r#""string""#);
        matches_itself(":x");
        matches_itself("(abc)");
        matches_itself("(abc def)");
    }

    fn transform(rule: &str, code: &str) -> Node {
        let mut rule_nodes = parse_to_nodes(rule);
        let rule = rule_nodes.remove(0);
        let generate = rule_nodes.remove(0);

        let node = parse_to_nodes(code).remove(0);
        assert!(matches_rule(&rule, &node), "rule should match");
        apply_rule(&rule, &generate, node)
    }

    fn yields(node: &str) -> Node {
        parse_to_nodes(node).remove(0)
    }

    #[test]
    fn simple_transforms_work() {
        assert_eq!(transform("x y", "x"), yields("y"));
        assert_eq!(transform("6 9", "6"), yields("9"));
        assert_eq!(
            transform(r#""some string" "another string""#, r#""some string""#),
            yields(r#""another string""#)
        );
    }

    #[test]
    fn atom_rule() {
        assert_eq!(transform(":x y", "word-node"), yields("y"));
        assert_eq!(transform(":x y", "6.9"), yields("y"));
        assert_eq!(transform(":x y", r#""string node""#), yields("y"));
        assert_eq!(transform(":x y", ":atom-node"), yields("y"));
        assert_eq!(
            transform(":x y", "(parent node (with children))"),
            yields("y")
        );
    }

    #[test]
    fn identity_transform() {
        let id = |x| assert_eq!(transform(":x :x", x), yields(x));
        id("word");
        id(":atom");
        id("6.9");
        id(r#""string node""#);
        id("(parent node)");
        id("(parent node (with lots (and lots (and lots (of children)))))");
        id(r#"(parent :node 6.9 "with" (different :types) (of "children") ((4.20)))"#);
    }

    #[test]
    fn complex_transforms_work() {
        assert_eq!(transform("(f :x) (:x + 2)", "(f 2)"), yields("(2 + 2)"));
        assert_eq!(
            transform(
                "(elif :condition :then-condition :end-condition) (else (if :condition :then-condition :end-condition))",
                "(elif (condition) (then ()) (else ()))"
            ),
            yields("(else (if (condition) (then ()) (else ())))")
        );
    }

    fn rec_transform(rule: &str, code: &str) -> Node {
        let mut rule_nodes = parse_to_nodes(rule);
        let rule = rule_nodes.remove(0);
        let generate = rule_nodes.remove(0);

        let node = parse_to_nodes(code).remove(0);
        apply_rule_recursively(&rule, &generate, node)
    }

    #[test]
    fn recursive_rule_smokescreen_test() {
        assert_eq!(
            rec_transform(":x y", "x"),
            yields("y"),
            "sanity check passes"
        );
    }

    #[test]
    fn recursive_rule_applies_to_children_of_parent_node() {
        assert_eq!(
            rec_transform("x y", "(x x x)"),
            yields("(y y y)"),
            "applies to children inside parent node"
        );

        assert_eq!(
            rec_transform("x y", "(x (x x x) x)"),
            yields("(y (y y y) y)"),
            "applies to children nested inside parent node"
        );

        assert_eq!(
            rec_transform("x y", "(x (x (x x x) x) x)"),
            yields("(y (y (y y y) y) y)"),
            "applies to children deeply nested inside parent node"
        );
    }

    #[test]
    fn recursive_rule_applies_to_parent_nodes() {
        assert_eq!(rec_transform("(f x) (f y)", "(f x)"), yields("(f y)"));
        assert_eq!(
            rec_transform("(f x) (f y)", "(f (f x))"),
            yields("(f (f y))")
        );

        assert_eq!(
            rec_transform("(f x) (f y)", "(f (f (f x)))"),
            yields("(f (f (f y)))")
        );
    }

    #[test]
    fn recursive_rule_applies_to_parent_nodes_and_children() {
        assert_eq!(
            rec_transform("((x)) (x)", "((x))"),
            yields("(x)"),
            "smokescreen check"
        );
        assert_eq!(rec_transform("((x)) (x)", "((((x))))"), yields("(((x)))"));
        assert_eq!(rec_transform("((x)) (x)", "((((x))))"), yields("(((x)))"));
    }
}
