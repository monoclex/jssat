use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct Production {
    name: String,
    parameters: Vec<String>,
    body: Vec<Body>,
}

impl Production {
    pub fn from_json(json: &str) -> Vec<Self> {
        serde_json::from_str(json).unwrap()
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Body(Vec<Rule>);

#[derive(Serialize, Deserialize, Debug)]
pub enum Rule {
    #[serde(rename = "literal")]
    Literal(String),
    #[serde(rename = "symbol")]
    Symbol(String),
    #[serde(rename = "name")]
    Name(Name),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Name {
    name: String,
    optional: Option<bool>,
    parameters: Option<Vec<Parameter>>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Parameter {
    parameter: String,
    kind: ParameterKind,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum ParameterKind {
    #[serde(rename = "?")]
    Question,
    #[serde(rename = "~")]
    Tilde,
    #[serde(rename = "+")]
    Plus,
}
