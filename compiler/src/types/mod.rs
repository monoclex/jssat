use std::{collections::HashMap, hash::Hash};

use crate::ir::{Function, RegisterId, TopLevelId, Type, IR};

pub fn annotate(ir: &IR) -> TypeAnnotations {
    // 1. Annotate types inside of functions
    let mut function_annotations = HashMap::new();

    for (&id, function) in ir.functions.iter() {
        let register_annotations = annotate_function(&function);

        function_annotations.insert(id, register_annotations);
    }

    // TODO: 2. Annotate global variables based on their usages

    // 3. Solve for types until they become exact
    // TODO: be more smart about this - here we can just abuse the facts that
    //   types are naive and don't utilize generics (yet).
    //
    //   because of this, we can get away with simple equality checks
    let mut id = 0;
    let mut type_mappings = HashMap::new();

    // ==> annotate external functions
    let mut full_ext_fn_anns = HashMap::new();

    for (key, f) in ir.external_functions.iter().map(|(&id, f)| (id, f)) {
        let mut full_annotations = Vec::new();

        for p in f.parameters.iter() {
            if let Some(t) =
                type_mappings
                    .iter()
                    .find_map(|(key, value)| if &p.kind == value { Some(key) } else { None })
            {
                full_annotations.push(*t);
            } else {
                // TODO: do we want to handle external functions after regular functions
                // to have less chacne of cloning?
                type_mappings.insert(TypeId(id), p.kind.clone());
                full_annotations.push(TypeId(id));
                id += 1;
            }
        }

        // TODO: don't be lazy about type interning
        type_mappings.insert(TypeId(id), f.return_type.clone());
        id += 1;

        full_ext_fn_anns.insert(
            key,
            ExternalFunctionAnnotations {
                parameter_annotations: full_annotations,
                return_type: TypeId(id - 1),
            },
        );
    }

    // ==> annotate functions
    let mut full_function_annotations = HashMap::new();

    for (key, (annotation, param_annotations, return_type)) in function_annotations.into_iter() {
        let mut full_annotations = HashMap::new();

        for (register, kind) in annotation.into_iter() {
            // TODO: clean up this bit (can definitely be made cleaner)
            if let Some(t) =
                type_mappings
                    .iter()
                    .find_map(|(key, value)| if &kind == value { Some(key) } else { None })
            {
                full_annotations.insert(register, *t);
            } else {
                type_mappings.insert(TypeId(id), kind);
                full_annotations.insert(register, TypeId(id));
                id += 1;
            }
        }

        let mut param_full_annotations = Vec::new();

        for param_type in param_annotations.into_iter() {
            // TODO: intern type
            type_mappings.insert(TypeId(id), param_type);
            param_full_annotations.push(TypeId(id));
            id += 1;
        }

        type_mappings.insert(TypeId(id), return_type);
        id += 1;

        full_function_annotations.insert(
            key,
            FunctionAnnotations {
                register_annotations: full_annotations,
                parameter_annotations: param_full_annotations,
                return_annotation: TypeId(id - 1),
            },
        );
    }

    TypeAnnotations {
        type_map: type_mappings,
        external_functions: full_ext_fn_anns,
        function_annotations: full_function_annotations,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

impl TypeId {
    pub fn value(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct TypeAnnotations {
    pub type_map: HashMap<TypeId, Type>,
    pub external_functions: HashMap<TopLevelId, ExternalFunctionAnnotations>,
    pub function_annotations: HashMap<TopLevelId, FunctionAnnotations>,
}

#[derive(Debug, Clone)]
pub struct FunctionAnnotations {
    pub register_annotations: HashMap<RegisterId, TypeId>,
    pub parameter_annotations: Vec<TypeId>,
    pub return_annotation: TypeId,
}

#[derive(Debug, Clone)]
pub struct ExternalFunctionAnnotations {
    pub parameter_annotations: Vec<TypeId>,
    pub return_type: TypeId,
}

// TODO: intern types here
pub fn annotate_function(function: &Function) -> (HashMap<RegisterId, Type>, Vec<Type>, Type) {
    let mut types = HashMap::new();
    let mut param_types = Vec::new();

    for parameter in function.parameters.iter() {
        types.insert(parameter.register, Type::Any);
        param_types.push(Type::Any);
    }

    for instruction in function
        .body
        .blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
    {
        if let Some(register) = instruction.assigned_to() {
            types.insert(register, Type::Any);
        }
    }

    (types, param_types, Type::Void)
}
