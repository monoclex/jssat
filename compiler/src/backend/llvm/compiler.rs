use std::{collections::HashMap, u64};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{CodeModel, FileType, RelocMode, Target, TargetMachine},
    values::{BasicValueEnum, FunctionValue, GlobalValue},
    AddressSpace, OptimizationLevel,
};

use super::super::BuildArtifact;
use super::super::{runtime_glue::RuntimeGlue, skeleton::ir::*};

use crate::id::*;

#[derive(Debug)]
pub struct BackendCompiler<'compilation, 'module> {
    pub ir: &'compilation IR,
    pub type_info: &'compilation TypeManager,
    pub context: &'compilation Context,
    pub builder: &'compilation Builder<'compilation>,
    pub module: &'module Module<'compilation>,
    pub glue: &'module RuntimeGlue<'compilation, 'module>,
    pub globals: &'module HashMap<TopLevelId, GlobalValue<'module>>,
    pub functions: &'module HashMap<TopLevelId, FunctionValue<'module>>,
}

impl BackendCompiler<'_, '_> {
    pub fn compile(&self) -> BuildArtifact {
        for (id, function) in self.ir.functions.iter() {
            let llvm_function = *self.functions.get(id).unwrap();

            let is_main = matches!(
                function.kind(*id, self.ir.entry_function),
                FunctionKind::Entrypoint
            );
            println!("compiling... -> id {:?}", id);
            println!("compiling... -> llvm_function {:?}", llvm_function);
            println!("compiling... -> is_main {:?}", is_main);
            println!("compiling... -> ir {:?}", self.ir);
            self.compile_function(function, llvm_function, is_main);
        }

        self.compile_llvm()
    }

    fn compile_function(&self, function: &Function, llvm_function: FunctionValue, is_main: bool) {
        let body = match &function.body {
            Some(b) => b,
            None => return,
        };

        let mut runtime_inserted = false;

        let runtime_register = match is_main {
            true => {
                // TODO: insert main function runtime generation code (properly)
                let rt_block = self.context.append_basic_block(llvm_function, "rt_setup");
                self.builder.position_at_end(rt_block);
                let result = self
                    .builder
                    .build_call(self.glue.fn_jssatrt_runtime_new, &[], "");
                runtime_inserted = true;
                result.try_as_basic_value().unwrap_left()
            }
            false => {
                // the runtime parameter is *usually* the first param
                // TODO: do more complex analysis to always get the right register

                llvm_function.get_first_param().unwrap()
            }
        };

        // [entry block -> the rest of the blocks]
        // this is so the order we append basic blocks in has the first block being the entry block
        let blocks =
            std::iter::once((&body.entry_block, body.body.get(&body.entry_block).unwrap()))
                .chain(body.body.iter().filter(|(k, _)| body.entry_block != **k));

        let mut inst_values = HashMap::new();

        // load params into registers
        for (param, register) in llvm_function
            .get_params()
            .iter()
            .zip(body.parameter_registers.iter())
        {
            inst_values.insert(*register, *param);
        }

        for (_, block) in blocks {
            // TODO: use debug information to generate a name
            let basic_block = self.context.append_basic_block(llvm_function, "");

            if runtime_inserted {
                self.builder.build_unconditional_branch(basic_block);
                runtime_inserted = false;
            }

            self.builder.position_at_end(basic_block);

            for instruction in block.instructions.iter() {
                match instruction {
                    Instruction::LoadGlobal(_, _) => todo!("LoadGlobal"),
                    Instruction::SaveGlobal(_, _) => todo!("SaveGlobal"),
                    Instruction::RecordGet(_, _, _) => todo!("RecordGet"),
                    Instruction::RecordSet(_, _, _) => todo!("RecordSet"),
                    Instruction::RefIsEmpty(_, _) => todo!("RefIsEmpty"),
                    Instruction::RefDeref(_, _) => todo!("RefDeref"),
                    Instruction::MakePrimitive { .. } => todo!("MakePrimitive"),
                    Instruction::GcTracingUnmarkRoot(_) => todo!("GcTracingUnmarkRoot"),
                    Instruction::Call(a, b, c) => {
                        let call_func = match b {
                            Callable::GlobalFunction(f) => self.functions.get(f).unwrap(),
                            Callable::LocalFunction(_) => todo!("LocalFunction"),
                        };

                        let mut args = c
                            .iter()
                            .map(|v| match v {
                                Value::Runtime => runtime_register,
                                Value::Register(register) => *inst_values.get(register).unwrap(),
                                Value::Constant(constant) => {
                                    // TODO: make sure to perform proper type conversions based on the actual types
                                    // for now we just cast to any
                                    let global = *self.globals.get(constant).unwrap();
                                    let constant = self.ir.constants.get(constant).unwrap();

                                    // BEGIN CONST_TYPE_CONV
                                    // trying to turn a constant into a `Type::Any`
                                    // `in_bounds_gep` works on `ArrayValue`s too
                                    println!("converting: {:?}", global);
                                    let p = global.as_pointer_value();
                                    println!("dealing with: {:?}", p);

                                    // TxODO: what am i doing
                                    let gep = unsafe {
                                        p.const_in_bounds_gep(&[
                                            self.context.i64_type().const_int(0, false),
                                            // context.i64_type().const_int(0, false),
                                        ])
                                    };
                                    println!("gep -> {:?}", gep);
                                    let gep = self.builder.build_bitcast(
                                        gep,
                                        self.context.i8_type().ptr_type(AddressSpace::Generic),
                                        "",
                                    );
                                    println!("--> gep after {:?}", gep);

                                    let args = &[
                                        runtime_register,
                                        gep.into(),
                                        // TxODO: use platform dependent size
                                        self.context
                                            .i64_type()
                                            .const_int(constant.payload.len() as u64, false)
                                            .into(),
                                    ];
                                    println!(
                                        "-> cast\n\tcall {:?}\n\tfn {:?}",
                                        args, self.glue.fn_jssatrt_constant_new
                                    );
                                    let value = self.builder.build_call(
                                        self.glue.fn_jssatrt_constant_new,
                                        args,
                                        "",
                                    );

                                    value.try_as_basic_value().unwrap_left()
                                    // END CONST_TYPE_CONV
                                }
                                Value::Number(number) => BasicValueEnum::FloatValue(
                                    self.context.f64_type().const_float(*number),
                                ),
                            })
                            .collect::<Vec<_>>();

                        // TODO: there might be something more sophisticated we can do
                        // this will insert a runtime parameter to the beginning until we match the fn sig length
                        // this is cuz when we call JSSAT IR functions, we don't hint at the RT param
                        // in the values when we call it, but we implicitly need it there
                        let caller_len = call_func.get_params().len();
                        while args.len() != caller_len {
                            args.insert(0, runtime_register);
                        }

                        println!("=== building call for -> {:?}", function);
                        println!("types -> {:?}", self.type_info);
                        println!("inst_values -> {:?}", inst_values);
                        println!("values -> {:?}", c);
                        println!("call_func -> {:?}", call_func);
                        println!("args -> {:?}", args);
                        println!("llvm_fn_params -> {:?}", llvm_function.get_params());
                        println!("param_regs -> {:?}", body.parameter_registers);
                        println!(
                            "Args.size() {} == FTy->getNumParams() {}: {}",
                            args.len(),
                            call_func.get_params().len(),
                            args.len() == call_func.get_params().len()
                        );
                        let result = self.builder.build_call(*call_func, args.as_slice(), "");

                        if let Some(r) = a {
                            inst_values.insert(*r, result.try_as_basic_value().unwrap_left());
                        }
                    }
                }
            }

            match &block.end_flow {
                InstructionFlow::Phi(_, _) => todo!("Phi"),
                InstructionFlow::Jmp(_) => todo!("Jmp"),
                InstructionFlow::JmpIf(_, _) => todo!("JmpIf"),
                InstructionFlow::Ret(None) => self.builder.build_return(None),
                InstructionFlow::Ret(_) => todo!("Ret"),
            };
        }
    }

    fn compile_llvm(&self) -> BuildArtifact {
        #[cfg(debug_assertions)]
        {
            let text_buff = self.module.print_to_string().to_string();
            println!(
                "EARLY LLVM IR:\n=== LLVM START\n{}\n=== LLVM END",
                text_buff
            );
        }

        Target::initialize_all(&Default::default());
        let target_triple = TargetMachine::get_default_triple();

        let target = Target::from_triple(&target_triple).unwrap();

        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                OptimizationLevel::Aggressive,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .expect("couldn't make target machine");

        let text_buff = self.module.print_to_string().to_string();

        let obj_buff = target_machine
            .write_to_memory_buffer(&self.module, FileType::Object)
            .expect("couldn't compile to assembly");

        BuildArtifact {
            llvm_ir: text_buff.to_string(),
            obj: obj_buff.as_slice().to_vec(),
        }
    }
}
