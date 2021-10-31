use std::fmt::Display;

use lexpr::{
    datum::{Ref, Span},
    Number, Parser, Value,
};

pub fn parse_to_nodes(code: &str) -> Vec<Node> {
    let mut nodes = Vec::new();
    let mut parser = Parser::from_str(code);

    fn to_node(datum: Ref) -> Node {
        match datum.value() {
            Value::String(value) => {
                let string = (&**value).into();
                Node::String(string, datum.span())
            }
            Value::Symbol(value) => {
                let mut string = (&**value).to_string();

                match string.starts_with(':') {
                    true => {
                        string.remove(0);
                        Node::Atom(string, datum.span())
                    }
                    false => Node::Word(string, datum.span()),
                }
            }
            Value::Cons(_) => {
                let mut children = Vec::new();

                for datum in datum.list_iter().unwrap() {
                    children.push(to_node(datum));
                }

                Node::Parent(children, datum.span())
            }
            Value::Number(n) => Node::Number(n.clone(), datum.span()),
            Value::Null => Node::Parent(Vec::new(), datum.span()),
            Value::Nil
            | Value::Bytes(_)
            | Value::Bool(_)
            | Value::Char(_)
            | Value::Keyword(_)
            | Value::Vector(_) => panic!("cannot handle {:?}", datum.value()),
        }
    }

    for datum in parser.datum_iter().map(Result::unwrap) {
        nodes.push(to_node(datum.as_ref()));
    }

    nodes
}

/// A [`Node`] is the representation of S-expressions to what a JSSAT IR file
/// understands. The existence of this simplifies parsing and rules for a JSSAT
/// IR file, as [`lexpr`] provides far more utilities that we do not need.
#[derive(Debug, Clone)]
pub enum Node {
    /// ```text
    /// (hello)
    ///  ^^^^^ is a word
    /// ```
    Word(String, Span),
    /// Similar to a word, except the first character starts with a `:`. The `:`
    /// is removed from the value of the atom.
    ///
    /// ```text
    /// (:yep)
    ///  ^^^^ is an atom
    ///
    /// (:6.1.7.2)
    ///  ^^^^^^^^ is also an atom
    /// ```
    Atom(String, Span),
    /// ```text
    /// ("wowies")
    ///  ^^^^^^^^ is a string
    /// ```
    String(String, Span),
    /// ```text
    /// (69)
    ///  ^^ is a number
    ///
    /// (6.9)
    ///  ^^ is also a number
    /// ```
    ///
    /// For numbers with more than one decimal point, it is not parsed as
    /// expected. Rather, the value `(6.1.7.2)` is parsed as `(6.1 . 7.2)`,
    /// which is the Cons S-expression.
    Number(Number, Span),
    /// A collection of children nodes
    ///
    /// ```text
    /// (assert (1 = 1) "the world works")
    ///         ^^^^^^^ parent
    /// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ also a parent
    /// ```
    Parent(Vec<Node>, Span),
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Word(l0, _), Self::Word(r0, _)) => l0 == r0,
            (Self::Atom(l0, _), Self::Atom(r0, _)) => l0 == r0,
            (Self::String(l0, _), Self::String(r0, _)) => l0 == r0,
            (Self::Number(l0, _), Self::Number(r0, _)) => l0 == r0,
            (Self::Parent(l0, _), Self::Parent(r0, _)) => l0 == r0,
            _ => false,
        }
    }
}

impl Node {
    pub fn expect_word(self) -> String {
        match self {
            Node::Word(value, _) => value,
            other => panic!("expected word node on {}", DisplaySpan(other.span())),
        }
    }

    pub fn expect_string(self) -> String {
        match self {
            Node::String(value, _) => value,
            other => panic!("expected string node on {}", DisplaySpan(other.span())),
        }
    }

    pub fn expect_parent(self) -> Vec<Node> {
        match self {
            Node::Parent(value, _) => value,
            other => panic!("expected parent node on {}", DisplaySpan(other.span())),
        }
    }
}

impl Node {
    pub fn span(&self) -> Span {
        match self {
            Node::Word(_, span)
            | Node::Atom(_, span)
            | Node::String(_, span)
            | Node::Number(_, span)
            | Node::Parent(_, span) => *span,
        }
    }
}

struct DisplaySpan(Span);

impl Display for DisplaySpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let start = self.0.start();
        let end = self.0.end();

        write!(
            f,
            "line {} column {} to line {} column {}",
            start.line(),
            start.column(),
            end.line(),
            end.column()
        )
    }
}

#[cfg(test)]
mod node_tests {
    use super::{parse_to_nodes, Node::*};

    #[test]
    pub fn parses_word() {
        let nodes = parse_to_nodes("an-example-word");
        assert_eq!(nodes.len(), 1);
        assert!(matches!(&nodes[0], Word(a, _) if a == "an-example-word"));
    }

    #[test]
    pub fn parses_atom() {
        let nodes = parse_to_nodes(":example-atom :6.1.7.2");
        assert_eq!(nodes.len(), 2);
        assert!(matches!(&nodes[0], Atom(a, _) if a == "example-atom"));
        assert!(matches!(&nodes[1], Atom(a, _) if a == "6.1.7.2"));
    }

    #[test]
    pub fn parses_string() {
        let nodes = parse_to_nodes(r#""example of a string""#);
        assert_eq!(nodes.len(), 1);
        assert!(matches!(&nodes[0], String(a, _) if a == "example of a string"));
    }

    #[test]
    pub fn parses_parent() {
        let nodes = parse_to_nodes(r#"(a :b "c")"#);
        assert_eq!(nodes.len(), 1);
        assert!(matches!(&nodes[0], Parent(a, _) if a.len() == 3));
    }

    #[test]
    pub fn parses_empty_parent() {
        let nodes = parse_to_nodes("()");
        assert_eq!(nodes.len(), 1);
        assert!(matches!(&nodes[0], Parent(a, _) if a.len() == 0));
    }

    #[test]
    #[allow(clippy::float_cmp)]
    pub fn parses_number() {
        let nodes = parse_to_nodes(r#"69 6.9"#);
        assert_eq!(nodes.len(), 2);
        assert!(matches!(&nodes[0], Number(a, _) if a.as_i64().unwrap() == 69));
        assert!(matches!(&nodes[1], Number(a, _) if a.as_f64().unwrap() == 6.9f64));
    }
}
