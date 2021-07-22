//! TODO: get rid of this file
//! for now it's a lot more work to make a new skeleton phase than to
//! just take the symb exec eng phase and glue it to the skeleton phase

use rustc_hash::FxHashMap;

use super::worker::WorkerResults;
use crate::frontend::assembler::*;
use crate::id::*;
use crate::lifted::LiftedProgram;
use crate::retag::CnstGenRetagger;
use crate::retag::CnstMapRetagger;
use crate::retag::ExtFnPassRetagger;
use crate::retag::ExtFnRetagger;
use crate::retag::FnPassRetagger;
use crate::retag::FnRetagger;

pub fn glue(
    entry: FunctionId<SymbolicCtx>,
    program: &LiftedProgram,
    results: FxHashMap<FunctionId<SymbolicCtx>, WorkerResults>,
) -> Program {
    let mut c_retagger = CnstMapRetagger::default();
    let mut constants = FxHashMap::default();

    let mut e_retagger = ExtFnPassRetagger::default();
    let mut external_functions = FxHashMap::default();
    for (&id, ext_fn) in program.external_functions.iter() {
        let ext_fn = ExternalFunction {
            name: ext_fn.name.clone(),
            parameters: ext_fn
                .parameters
                .iter()
                .map(|t| t.clone().into_value_type())
                .collect(),
            returns: ext_fn.returns.clone().into_return_type(),
        };
        external_functions.insert(e_retagger.retag_new(id), ext_fn);
    }

    let mut fn_retagger = FnPassRetagger::default();
    let mut functions = FxHashMap::default();

    for (id, mut results) in results {
        let id = fn_retagger.retag_new(id);

        let instructions = (results.assembler_piece.blocks)
            .iter_mut()
            .flat_map(|(_, b)| b.instructions.iter_mut());

        for (_, blk) in results.assembler_piece.blocks.iter_mut() {
            for inst in blk.instructions.iter_mut() {
                if let Instruction::MakeString(r, c) = inst {
                    if let crate::frontend::type_annotater::ValueType::ExactString(payload) =
                        blk.register_types.get(*r)
                    {
                        *c = intern_constant(&mut c_retagger, &mut constants, payload);
                    }
                }
            }
        }

        functions.insert(id, results.assembler_piece);
    }

    Program {
        entrypoint: fn_retagger.retag_old(entry),
        constants,
        external_functions,
        functions,
    }
}

fn intern_constant(
    c_retagger: &mut CnstMapRetagger<NoContext, AssemblerCtx>,
    constants: &mut FxHashMap<ConstantId<AssemblerCtx>, Vec<u8>>,
    payload: &Vec<u8>,
) -> ConstantId<AssemblerCtx> {
    for (k, v) in constants.iter() {
        if v == payload {
            return *k;
        }
    }

    let k = c_retagger.gen();
    constants.insert(k, payload.clone());
    k
}
