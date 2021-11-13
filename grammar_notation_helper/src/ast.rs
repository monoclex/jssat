use derive_more::{Deref, DerefMut};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Deref, DerefMut, Debug, Clone)]
pub struct Productions(#[deref] Vec<Production>);

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Production {
    name: String,
    parameters: Vec<String>,
    body: Vec<Body>,
}

impl Production {
    pub fn from_json(json: &str) -> Productions {
        serde_json::from_str(json).unwrap()
    }
}

#[derive(Serialize, Deserialize, Deref, DerefMut, Debug, Clone)]
pub struct Body(#[deref] Vec<Rule>);

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
    name: String,
    optional: Option<bool>,
    parameters: Option<Vec<Parameter>>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Parameter {
    parameter: String,
    kind: ParameterKind,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum ParameterKind {
    #[serde(rename = "?")]
    Question,
    #[serde(rename = "~")]
    Tilde,
    #[serde(rename = "+")]
    Plus,
}

impl Productions {
    pub fn simplify(&mut self) {
        self.remove_optionals();
    }

    fn remove_optionals(&mut self) {
        for production in self.iter_mut() {
            let bodies = (production.body.drain(..))
                .flat_map(|b| Rule::remove_optionals(&b))
                .collect();

            production.body = bodies;
        }
    }
}

impl Rule {
    pub fn remove_optionals(rules: &[Rule]) -> Vec<Body> {
        let rule = match rules.get(0) {
            Some(r) => r,
            None => return Vec::new(),
        };

        let mut end_rules = Rule::remove_optionals(&rules[1..]);

        if let Some(name) = rule.as_name() {
            // if we have a value for it it must be optional lol
            if name.optional.is_some() {
                let mut rules_without_this_rule = end_rules.clone();
                let mut rules_with_this_rule = end_rules;

                let mut name = (*name).clone();
                name.optional = None;

                for rule in &mut rules_with_this_rule {
                    rule.insert(0, Rule::Name(name.clone()))
                }

                end_rules = rules_with_this_rule;
                end_rules.append(&mut rules_without_this_rule);
            }
        }

        end_rules
    }
}
