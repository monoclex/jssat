use std::fmt::Display;

use self::rules::apply_rule_recursively;

use super::*;

mod node;
mod rules;
use lexpr::datum::Span;
use node::*;

pub fn parse(code: &str) -> AST {
    let nodes = parse_to_nodes(code);
    let mut custom_rules = Vec::new();

    for node in nodes {
        let mut children = node.expect_parent();

        let header = children.remove(0);
        let child1 = children.remove(0);
        let child2 = children.remove(0);

        match header.expect_word().as_str() {
            "def" => {
                let (rule, generate) = (child1, child2);
                custom_rules.push((rule, generate));
            }
            "section" => {
                let (header, body) = (child1, child2);
                todo!("sections")
            }
            other => panic!("unrecognized header {}", other),
        }
    }

    todo!()
}

fn parse_with_rule_application(nodes: Vec<Node>) -> Vec<Node> {
    let mut new_nodes = Vec::new();
    let mut custom_rules = Vec::new();

    for node in nodes {
        if let Node::Parent(children, _) = &node {
            if let Some(Node::Word(header_word, _)) = children.get(0) {
                if header_word == "def" {
                    let (rule, generate) = (children[1].clone(), children[2].clone());
                    custom_rules.push((rule, generate));
                    continue;
                }
            }
        };

        let node = custom_rules.iter().fold(node, |node, (rule, generate)| {
            apply_rule_recursively(rule, generate, node)
        });
        new_nodes.push(node);
    }

    new_nodes
}

mod parse_with_rule_application_tests {
    use super::*;

    macro_rules! parse {
        ($code: expr, $yields: expr) => {
            assert_eq!(
                parse_with_rule_application(parse_to_nodes($code)),
                parse_to_nodes($yields)
            )
        };
    }

    #[test]
    fn identity() {
        let id = |x| parse!(x, x);

        id("f x");
        id("(whatever-idk)");
        id("(a (s) (d) (fff))");
    }

    #[test]
    fn obeys_rule_order() {
        parse!("(def x y) x", "y");
        parse!("x (def x y)", "x");
        parse!("x (def x y) x", "x y");
        parse!("x y (def y x) x y (def x y) x y", "x y x x y y");
    }

    #[test]
    fn applies_rules_to_node() {
        parse!(
            r#"
(def x y)

(x
    x
    (x)
    (x))
"#,
            "(y y (y) (y))"
        );
    }
}

#[test]
pub fn x() {
    parse(
        r#"

(def
    (:is-not-undefined x)
    (x = (trivial undefined)))
  
  (section
    (header 6.9 testing)
    (body ((assert (1 = 1)))))
  
  (section
    (header :10.1.6.3 ValidateAndApplyPropertyDescriptor (O, P, extensible, Desc, current))
    (body (;;; 1. Assert: If O is not undefined, then IsPropertyKey(P) is true
           (if (is-not-undefined O)
               (then ((assert ((IsPropertyKey P) = true) "If O is not undefined, then IsPropertyKey(P) is true"))))
           ;;; 2. If current is undefined, then
           (if (is-undefined current)
               (then (;;; a. If extensible is false, return false
                      (if (extensible = false)
                          (then ((return false))))
                      ;;; b. Assert: extensible is true.
                      (assert (extensible = true) "extensible is true")
                      ;;; c. If IsGenericDescriptor(Desc) is true or IsDataDescriptor(Desc) is true, then
                      (if (((IsGenericDescriptor Desc) = true) or ((IsDataDescriptor Desc) = true))
                          (then (;;; i. If O is not undefined, create an own data property named P of object O whose [[Value]],
                                 ;;;[[Writable]], [[Enumerable]], and [[Configurable]] attribute values are described by Desc. If the
                                 ;;;value of an attribute field of Desc is absent, the attribute of the newly created property is set to its
                                 ;;;default value.
                                 ("todo"))))))))))

"#,
    );

    panic!()
}
