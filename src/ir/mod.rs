use std::{ops::Deref, sync::Arc};

mod formatting;

pub fn new() -> IR {
    let agent = Identifier(Arc::new(IdentifierData {
        id: 0,
        name: Some("agent".to_string()),
    }));

    let ext = Identifier(Arc::new(IdentifierData {
        id: 1,
        name: Some("externalfunction".to_string()),
    }));

    let a = Identifier(Arc::new(IdentifierData {
        id: 0,
        name: Some("a".to_string()),
    }));

    let a2 = Identifier(Arc::new(IdentifierData {
        id: 1,
        name: Some("a".to_string()),
    }));

    let ir = IR {
        global_variables: vec![GlobalVariable { id: agent }],
        external_functions: vec![ExternalFunction {
            id: ext,
            parameters: vec![
                TypedParameter {
                    register: Register { id: a },
                    kind: Type::Any,
                },
                TypedParameter {
                    register: Register { id: a2 },
                    kind: Type::Any,
                },
            ],
        }],
        functions: vec![],
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

#[derive(Debug)]
pub struct IdentifierData {
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
    arguments: Vec<Register>,
    body: FunctionBody,
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
    id: usize,
    name: Option<String>,
    instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    Ret,
}
