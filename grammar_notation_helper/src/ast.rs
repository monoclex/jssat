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

pub fn remove_optionals(prods: &mut Productions) {
    for prod in &mut prods.ast {
        let sig = prod
            .body
            .iter()
            .map(|x| {
                x.sequence.iter().any(|y| {
                    if let Some(x) = y.as_name() {
                        x.optional.is_some()
                    } else {
                        false
                    }
                })
            })
            .any(|t| t);
        if sig {
            println!("debuggy:::");
            println!("{:#?}", prod);
        }

        let mut new_bodies = vec![];

        for body in &mut prod.body {
            let rules = cartesian_product(body.sequence.clone());
            for rule in rules {
                new_bodies.push(Body {
                    source: body.source.clone(),
                    sequence: rule,
                });
            }
        }

        prod.body.clear();
        prod.body.extend(new_bodies);

        if sig {
            println!("debuggy post:");
            println!("{:#?}", prod);
        }
    }
}

fn cartesian_product(mut rules: Vec<Rule>) -> Vec<Vec<Rule>> {
    if rules.is_empty() {
        return vec![vec![]];
    }

    if rules.len() == 1 {
        return produce_rules(rules.remove(0));
    }

    let next = rules.remove(0);

    let next_productions = produce_rules(next);
    let futures = cartesian_product(rules);

    let mut next = vec![];

    if futures.is_empty() {
        return next_productions;
    }

    for production in next_productions {
        for future in futures.clone() {
            let mut a = production.clone();
            a.extend(future);
            next.push(a);
        }
    }

    next
}

fn produce_rules(rule: Rule) -> Vec<Vec<Rule>> {
    if let Some(Name {
        name,
        optional: Some(true),
    }) = rule.as_name()
    {
        let rule = Rule::Name(Name {
            name: name.clone(),
            optional: None,
        });

        vec![vec![], vec![rule]]
    } else {
        vec![vec![rule]]
    }
}
