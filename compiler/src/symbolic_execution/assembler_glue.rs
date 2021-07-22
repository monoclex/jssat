//! TODO: get rid of this file
//! for now it's a lot more work to make a new skeleton phase than to
//! just take the symb exec eng phase and glue it to the skeleton phase

use rustc_hash::FxHashMap;

use super::worker::WorkerResults;
use crate::frontend::assembler::*;
use crate::id::*;
use crate::lifted::LiftedProgram;
use crate::retag::FnPassRetagger;
use crate::retag::FnRetagger;

pub fn glue(
    entry: FunctionId<SymbolicCtx>,
    program: &LiftedProgram,
    results: FxHashMap<FunctionId<SymbolicCtx>, WorkerResults>,
) -> Program {
    let mut constants = FxHashMap::default();

    let mut external_functions = FxHashMap::default();

    let mut fn_retagger = FnPassRetagger::default();
    let mut functions = FxHashMap::default();

    for (id, results) in results {
        //
    }

    Program {
        entrypoint: fn_retagger.retag_old(entry),
        constants,
        external_functions,
        functions,
    }
}
