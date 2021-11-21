use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Productions {
    pub ast: Vec<Production>,
    #[serde(rename = "oneOfAst")]
    pub one_of_ast: Vec<OneOfProduction>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Production {
    pub name: String,
    pub body: Vec<Body>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct OneOfProduction {
    pub name: String,
    pub terminals: Vec<String>,
}

impl Production {
    pub fn from_json(json: &str) -> Productions {
        serde_json::from_str(json).unwrap()
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Body {
    pub source: String,
    pub sequence: Vec<Rule>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Rule {
    #[serde(rename = "literal")]
    Literal(String),
    #[serde(rename = "symbol")]
    Symbol(String),
    #[serde(rename = "name")]
    Name(Name),
}

impl Rule {
    pub fn as_name(&self) -> Option<&Name> {
        if let Rule::Name(name) = self {
            Some(name)
        } else {
            None
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Name {
    pub name: String,
    pub optional: Option<bool>,
}
