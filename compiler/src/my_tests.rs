//! Because this is all in one binary, i can't use a `tests` folder

#[cfg(test)]
use crate::{
    frontend::{builder::ProgramBuilder, conv_only_bb, ir::ControlFlowInstruction},
    isa::{BlockJump, Jump},
};

/// Confirm that registers are passed one block away properly
#[test]
pub fn can_lift_registers_in_near_blocks() {
    let mut program = ProgramBuilder::new();

    let mut main = program.start_function_main();
    let main_id = main.id;

    let mut block1 = main.start_block_main();
    let (block2, []) = main.start_block();
    let block1_id = block1.id;

    let undefined = block1.make_undefined();
    main.end_block(block1.jmp(block2.signature(), []));
    main.end_block(block2.ret(Some(undefined)));

    program.end_function(main);

    let ir = program.finish();
    println!("{}", crate::frontend::display_jssatir::display(&ir));
    let pure_blocks = conv_only_bb::translate(&ir);
    println!("{}", crate::frontend::display_bb::display(&pure_blocks));

    let entrypoint = pure_blocks.get_block_id_by_host(main_id, block1_id);

    // the main block shall have no parameters
    let entry_block = pure_blocks.get_block(entrypoint);
    assert_eq!(entry_block.parameters.len(), 0);

    // the main block shall jump to the next block with the parameter
    if let ControlFlowInstruction::Jmp(Jump(BlockJump(_, args))) = &entry_block.end {
        // we shall jump with one argument: the undefined value
        assert_eq!(args.len(), 1);
    } else {
        unreachable!("the end control flow of main should be a jump");
    }
}

/// There may be a bug in `conv_only_bb` where if a block is far enough away,
/// the constraints for what registers to pass to it don't get passed along.
/// Thus, there ends up being a parameter to the main block/register that is unknown.
#[test]
pub fn can_lift_registers_in_far_blocks() {
    let mut program = ProgramBuilder::new();

    let mut main = program.start_function_main();
    let main_id = main.id;

    let mut block1 = main.start_block_main();
    let (block2, []) = main.start_block();
    let (block3, []) = main.start_block();
    let (block4, []) = main.start_block();
    let (block5, []) = main.start_block();
    let block1_id = block1.id;

    let undefined = block1.make_undefined();
    main.end_block(block1.jmp(block2.signature(), []));
    main.end_block(block2.jmp(block3.signature(), []));
    main.end_block(block3.jmp(block4.signature(), []));
    main.end_block(block4.jmp(block5.signature(), []));
    main.end_block(block5.ret(Some(undefined)));

    program.end_function(main);

    let ir = program.finish();
    println!("{}", crate::frontend::display_jssatir::display(&ir));
    let pure_blocks = conv_only_bb::translate(&ir);
    println!("{}", crate::frontend::display_bb::display(&pure_blocks));

    let entrypoint = pure_blocks.get_block_id_by_host(main_id, block1_id);

    // the main block shall have no parameters
    let entry_block = pure_blocks.get_block(entrypoint);
    assert_eq!(entry_block.parameters.len(), 0);

    // the main block shall jump to the next block with the parameter
    let to = if let ControlFlowInstruction::Jmp(Jump(BlockJump(to, args))) = &entry_block.end {
        // we shall jump with one argument: the undefined value
        assert_eq!(args.len(), 1);
        *to
    } else {
        panic!("it shall jump to")
    };

    println!("entry jumped to {:?}", to);

    // the second block shall have one parameter
    let block = pure_blocks.get_block(to);
    println!("jumped to {:?}", block);
    assert_eq!(block.parameters.len(), 1);

    // that parameter shall be used to jump to another block
    let param = block.parameters[0];
    println!("got: {:?}", &block.end);
    if let ControlFlowInstruction::Jmp(Jump(BlockJump(to, args))) = &block.end {
        assert_eq!(args.len(), 1);
        assert_eq!(param, args[0]);
        *to
    } else {
        panic!("block must end in jump")
    };
}
