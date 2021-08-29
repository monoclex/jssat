//! Because this is all in one binary, i can't use a `tests` folder

#[cfg(test)]
use crate::{
    frontend::builder::ProgramBuilder,
    isa::{BlockJump, Jump},
    lifted::EndInstruction,
};

/// Confirm that registers are passed one block away properly
#[test]
pub fn can_lift_registers_in_near_blocks() {
    let mut program = ProgramBuilder::new();

    let mut main = program.start_function_main();

    let mut block1 = main.start_block_main();
    let (block2, []) = main.start_block();

    let undefined = block1.make_undefined();
    main.end_block(block1.jmp(block2.signature(), []));
    main.end_block(block2.ret(Some(undefined)));

    program.end_function(main);

    let ir = program.finish();
    println!("{}", crate::frontend::display_jssatir::display(&ir));
    let lifted = crate::lifted::lift(ir);
    println!("{:#?}", lifted);

    let entrypoint = lifted.functions.get(&lifted.entrypoint).unwrap();

    // the main block shall have no parameters
    assert_eq!(entrypoint.parameters.len(), 0);

    // the main block shall jump to the next block with the parameter
    if let EndInstruction::Jump(Jump(BlockJump(_, args))) = &entrypoint.end {
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

    let mut block1 = main.start_block_main();
    let (block2, []) = main.start_block();
    let (block3, []) = main.start_block();
    let (block4, []) = main.start_block();
    let (block5, []) = main.start_block();

    let undefined = block1.make_undefined();
    main.end_block(block1.jmp(block2.signature(), []));
    main.end_block(block2.jmp(block3.signature(), []));
    main.end_block(block3.jmp(block4.signature(), []));
    main.end_block(block4.jmp(block5.signature(), []));
    main.end_block(block5.ret(Some(undefined)));

    program.end_function(main);

    let ir = program.finish();
    println!("{}", crate::frontend::display_jssatir::display(&ir));
    let lifted = crate::lifted::lift(ir);
    println!("{:#?}", lifted);

    let entrypoint = lifted.functions.get(&lifted.entrypoint).unwrap();

    // the main block shall have no parameters
    assert_eq!(entrypoint.parameters.len(), 0);

    // the main block shall jump to the next block with the parameter
    let to = if let EndInstruction::Jump(Jump(BlockJump(to, args))) = &entrypoint.end {
        // we shall jump with one argument: the undefined value
        assert_eq!(args.len(), 1);
        *to
    } else {
        panic!("it shall jump to")
    };

    println!("entry jumped to {:?}", to);

    // the second block shall have one parameter
    let block = lifted.functions.get(&to).unwrap();
    // println!("jumped to {:?}", block);
    assert_eq!(block.parameters.len(), 1);

    // that parameter shall be used to jump to another block
    let param = block.parameters[0];
    println!("got: {:?}", &block.end);
    if let EndInstruction::Jump(Jump(BlockJump(_, args))) = &block.end {
        assert_eq!(args.len(), 1);
        assert_eq!(param, args[0]);
    } else {
        panic!("block must end in jump")
    }
}
