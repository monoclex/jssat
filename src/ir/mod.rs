use std::{ops::Deref, sync::Arc};

mod formatting;

pub fn new() -> IR {
    let agent = Identifier(Arc::new(IdentifierData {
        scope: Scope::Global,
        id: 0,
        name: Some("agent".to_string()),
    }));

    let ext = Identifier(Arc::new(IdentifierData {
        scope: Scope::Global,
        id: 1,
        name: Some("externalfunction".to_string()),
    }));

    let a = Identifier(Arc::new(IdentifierData {
        scope: Scope::Local,
        id: 0,
        name: Some("a".to_string()),
    }));

    let a2 = Identifier(Arc::new(IdentifierData {
        scope: Scope::Local,
        id: 1,
        name: Some("a".to_string()),
    }));

    let afn = Identifier(Arc::new(IdentifierData {
        scope: Scope::Global,
        id: 2,
        name: Some("afn".to_string()),
    }));

    let entry = Identifier(Arc::new(IdentifierData {
        scope: Scope::Local,
        id: 0,
        name: Some("entry".to_string()),
    }));

    let ir = IR {
        global_variables: vec![GlobalVariable { id: agent }],
        external_functions: vec![ExternalFunction {
            id: ext,
            parameters: vec![
                TypedParameter {
                    register: Register { id: a.clone() },
                    kind: Type::Any,
                },
                TypedParameter {
                    register: Register { id: a2.clone() },
                    kind: Type::Any,
                },
            ],
        }],
        functions: vec![Function {
            id: afn,
            arguments: vec![
                Parameter {
                    register: Register { id: a },
                },
                Parameter {
                    register: Register { id: a2 },
                },
            ],
            body: FunctionBody {
                blocks: vec![FunctionBlock {
                    id: entry,
                    instructions: vec![Instruction::Ret],
                }],
            },
        }],
    };

    ir
}

#[derive(Debug, Clone)]
pub struct Identifier(Arc<IdentifierData>);

impl Deref for Identifier {
    type Target = IdentifierData;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

/// Represents the scope an identifier is in.
#[derive(Debug, Clone, Copy)]
pub enum Scope {
    /// The global scope, `@`.
    Global,
    /// The local scope, `%`.
    Local,
}

#[derive(Debug)]
pub struct IdentifierData {
    // NOTE: scope doesn't *need* to be here, but it is here so we can format
    //       identifiers without need for state in display
    scope: Scope,
    id: usize,
    name: Option<String>,
}

#[derive(Debug)]
pub struct IR {
    pub global_variables: Vec<GlobalVariable>,
    pub external_functions: Vec<ExternalFunction>,
    pub functions: Vec<Function>,
    // TODO: `pub data: Vec<DataDeclaration>`
}

#[derive(Debug)]
pub struct GlobalVariable {
    id: Identifier,
}

#[derive(Debug)]
pub struct ExternalFunction {
    id: Identifier,
    parameters: Vec<TypedParameter>,
}

#[derive(Debug)]
pub struct TypedParameter {
    register: Register,
    kind: Type,
}

#[derive(Debug)]
pub struct Function {
    id: Identifier,
    arguments: Vec<Parameter>,
    body: FunctionBody,
}

#[derive(Debug)]
pub struct Parameter {
    register: Register,
}

#[derive(Debug)]
pub struct Register {
    id: Identifier,
}

#[derive(Debug)]
pub enum Type {
    Any,
}

#[derive(Debug)]
pub struct FunctionBody {
    blocks: Vec<FunctionBlock>,
}

#[derive(Debug)]
pub struct FunctionBlock {
    id: Identifier,
    instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    Ret,
}
