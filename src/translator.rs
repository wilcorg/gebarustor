use crate::ast::*;
use crate::instruction::*;
use crate::memory::{DeclType, Memory, Type};
use crate::procedure_builder::*;
use crate::utils::create_asm_value;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Translator {
    pseudo_: Vec<Instruction>,
    procedures_: HashMap<String, ProcedureBuilder>,
    memory_: Memory,
    syntax_tree_: Program,
}

impl Translator {
    pub fn new(ast: Program) -> Result<Translator, CodeError> {
        // parsowanie prodedur
        let mut pseudo = vec![];
        let mut procedures: HashMap<String, ProcedureBuilder> = HashMap::new();

        if let Some(procedures_ast) = ast.0.clone() {
            for procedure in procedures_ast {
                if procedures
                    .insert(
                        procedure.0 .0 .0.clone(), // procedure name
                        ProcedureBuilder::new(procedure.clone()),
                    )
                    .is_some()
                {
                    Err(CodeError::DuplicateProcedureDeclaration(
                        procedure.0 .0 .0.clone(), // procedure name
                        procedure.0 .0 .1.clone(), // number ??? todo
                    ))?
                }
            }
        }

        let mut memory: Memory = Memory::new(&mut pseudo);
        if let Some(vars) = ast.1 .0.clone() {
            for var in vars {
                match var {
                    VarDeclVariant::Val(pid) => {
                        pseudo.extend(
                            memory
                                .declare_variable(pid.0.clone(), DeclType::Variable)
                                .unwrap_or(vec![]),
                        );
                    }
                    VarDeclVariant::NumIndexed(pid, from_ind, to_ind) => {
                        // pseudo.extend(
                        memory
                            .declare_variable(pid.0.clone(), DeclType::Array(from_ind, to_ind))?;
                        // );
                    }
                }
            }
        }

        Ok(Translator {
            pseudo_: pseudo,
            procedures_: procedures,
            memory_: memory,
            syntax_tree_: ast,
        })
    }

    pub fn translate(&mut self) -> Result<(), CodeError> {
        self.translate_main()?;
        self.pseudo_.push(Instruction::Halt);
        Ok(())
    }

    fn translate_main(&mut self) -> Result<(), CodeError> {
        let commands = self.syntax_tree_.1 .1.clone();
        let mut translated_commands: Vec<Vec<Instruction>> = vec![];
        for command in commands {
            translated_commands.push(self.translate_command(command)?)
        }
        for command in translated_commands {
            self.pseudo_.extend(command);
        }
        Ok(())
    }

    fn translate_condition(&mut self, condition: Condition) -> Result<Vec<Instruction>, CodeError> {
        let mut cond_pseudo = vec![];
        match condition {
            Condition::EQ(val_1, val_2) => {
                cond_pseudo.extend(self.memory_.load_value(val_1)?);
                cond_pseudo.push(Instruction::Store(10));
                cond_pseudo.extend(self.memory_.load_value(val_2)?);
                cond_pseudo.push(Instruction::Sub(10));
                cond_pseudo.push(Instruction::Jzero(3));
                cond_pseudo.push(Instruction::Load(50));
                cond_pseudo.push(Instruction::Jump(2));
                cond_pseudo.push(Instruction::Load(51));
            }
            Condition::NEQ(val_1, val_2) => {
                cond_pseudo.extend(self.memory_.load_value(val_1)?);
                cond_pseudo.push(Instruction::Store(10));
                cond_pseudo.extend(self.memory_.load_value(val_2)?);
                cond_pseudo.push(Instruction::Sub(10));
                cond_pseudo.push(Instruction::Jzero(3));
                cond_pseudo.push(Instruction::Load(51));
                cond_pseudo.push(Instruction::Jump(2));
                cond_pseudo.push(Instruction::Load(50));
            }
            Condition::GT(val_1, val_2) => {
                cond_pseudo.extend(self.memory_.load_value(val_1)?);
                cond_pseudo.push(Instruction::Store(10));
                cond_pseudo.extend(self.memory_.load_value(val_2)?);
                cond_pseudo.push(Instruction::Sub(10));
                cond_pseudo.push(Instruction::Jneg(3));
                cond_pseudo.push(Instruction::Load(50));
                cond_pseudo.push(Instruction::Jump(2));
                cond_pseudo.push(Instruction::Load(51));
            }
            Condition::LT(val_1, val_2) => {
                cond_pseudo.extend(self.memory_.load_value(val_1)?);
                cond_pseudo.push(Instruction::Store(10));
                cond_pseudo.extend(self.memory_.load_value(val_2)?);
                cond_pseudo.push(Instruction::Sub(10));
                cond_pseudo.push(Instruction::Jpos(3));
                cond_pseudo.push(Instruction::Load(50));
                cond_pseudo.push(Instruction::Jump(2));
                cond_pseudo.push(Instruction::Load(51));
            }
            Condition::GEQ(val_1, val_2) => {
                cond_pseudo.extend(self.memory_.load_value(val_1)?);
                cond_pseudo.push(Instruction::Store(10));
                cond_pseudo.extend(self.memory_.load_value(val_2)?);
                cond_pseudo.push(Instruction::Sub(10));
                cond_pseudo.push(Instruction::Jpos(3));
                cond_pseudo.push(Instruction::Load(51));
                cond_pseudo.push(Instruction::Jump(2));
                cond_pseudo.push(Instruction::Load(50));
            }
            Condition::LEQ(val_1, val_2) => {
                cond_pseudo.extend(self.memory_.load_value(val_1)?);
                cond_pseudo.push(Instruction::Store(10));
                cond_pseudo.extend(self.memory_.load_value(val_2)?);
                cond_pseudo.push(Instruction::Sub(10));
                cond_pseudo.push(Instruction::Jneg(3));
                cond_pseudo.push(Instruction::Load(51));
                cond_pseudo.push(Instruction::Jump(2));
                cond_pseudo.push(Instruction::Load(50));
            }
        }
        Ok(cond_pseudo)
    }

    fn translate_command(&mut self, command: Command) -> Result<Vec<Instruction>, CodeError> {
        let mut pseudo: Vec<Instruction> = vec![];
        match command {
            Command::ProcCall((procedure_id, parameters)) => {
                // 1. DONE
                // 2. recursive calls check
                let ids: Vec<String> = parameters.iter().map(|arg| arg.0.clone()).collect();
                for id in ids {
                    if (&id).contains(&format!("@{}", procedure_id.0)) {
                        return Err(CodeError::RecursiveProcedureCall(
                            procedure_id.0,
                            procedure_id.1,
                        ));
                    }
                }

                // 3. find procedure definition
                let builder = self
                    .procedures_
                    .clone()
                    .get(&procedure_id.0)
                    .ok_or(CodeError::UndeclaredProcedure(
                        procedure_id.0.clone(),
                        procedure_id.1,
                    ))?
                    .clone();

                // 4. check parameter count
                if builder.declared_arguments.len() != parameters.len() {
                    return Err(CodeError::WrongNumberOfArguments(
                        procedure_id.0.clone(),
                        procedure_id.1,
                    ));
                }

                // 5. allocate memory slots for LOCAL variables
                if let Some(declarations) = &builder.declarations {
                    for declaration in declarations {
                        match declaration {
                            VarDeclVariant::Val(pid) => {
                                let scoped_pid = format!("{}@{}", pid.0, procedure_id.0);
                                pseudo.extend(
                                    self.memory_
                                        .declare_variable(scoped_pid, DeclType::Variable)
                                        .unwrap_or(vec![]),
                                );
                            }
                            VarDeclVariant::NumIndexed(pid, from_ind, to_ind) => {
                                let scoped_pid = format!("{}@{}", pid.0, procedure_id.0);
                                pseudo.extend(
                                    self.memory_
                                        .declare_variable(
                                            scoped_pid,
                                            DeclType::Array(*from_ind, *to_ind),
                                        )
                                        .unwrap_or(vec![]),
                                );
                            }
                        }
                    }
                }

                // 6. match each actual parameter with formal parameter declared by the procedure
                for (parameter, declared_parameter) in
                    parameters.iter().zip(builder.declared_arguments)
                {
                    self.memory_.identifiers_.insert(parameter.0.clone()); // auto-declare variables

                    if let Some(declarations) = &builder.declarations {
                        for declaration in declarations {
                            let pid = match declaration {
                                VarDeclVariant::Val(pid) => pid,
                                VarDeclVariant::NumIndexed(pid, _, _) => pid,
                            };
                            let declared_pid = match declared_parameter.clone() {
                                ArgumentsDeclarationVariant::Val(pid) => pid,
                                ArgumentsDeclarationVariant::Array(pid) => pid,
                            };

                            if pid.0 == declared_pid.0 {
                                return Err(CodeError::DuplicateVariableDeclaration(
                                    pid.0.clone(),
                                    pid.1,
                                ));
                            }
                        }
                    }

                    let parameter_type = self.memory_.get_variable(&parameter.0)?;
                    // let parameter_addr = self.memory_.addr_from_url(parameter.0.clone())?;

                    match declared_parameter {
                        ArgumentsDeclarationVariant::Val(pid) => {
                            let scoped_pid = format!("{}@{}", pid.0, procedure_id.0);

                            match parameter_type {
                                Type::Variable(addr) => {
                                    pseudo.extend(
                                        self.memory_
                                            .declare_variable(
                                                scoped_pid.clone(),
                                                DeclType::Pointer(*addr),
                                            )
                                            .unwrap_or(vec![]),
                                    );
                                    self.memory_.identifiers_.insert(scoped_pid.clone());
                                }
                                Type::Array(_, _, _) => {
                                    Err(CodeError::WrongArgumentType(pid.0.clone(), pid.1))?
                                }
                                Type::Iterator(_) => {
                                    todo!("undefined behaviour")
                                }
                                Type::Pointer(_, to) => {
                                    pseudo.extend(
                                        self.memory_
                                            .declare_variable(
                                                scoped_pid.clone(),
                                                DeclType::Pointer(*to),
                                            )
                                            .unwrap_or(vec![]),
                                    );
                                    self.memory_.identifiers_.insert(scoped_pid.clone());
                                }
                            }
                        }
                        ArgumentsDeclarationVariant::Array(pid) => {
                            let scoped_pid = format!("{}@{}", pid.0, procedure_id.0);

                            match parameter_type {
                                Type::Array(addr, _, _) => {
                                    pseudo.extend(
                                        self.memory_
                                            .declare_variable(
                                                scoped_pid.clone(),
                                                DeclType::Pointer(*addr),
                                            )
                                            .unwrap_or(vec![]),
                                    );
                                    self.memory_.identifiers_.insert(scoped_pid.clone());
                                }
                                _ => Err(CodeError::WrongArgumentType(pid.0.clone(), pid.1))?,
                            }
                        }
                    }
                }

                for command in &builder.commands {
                    pseudo.extend(self.translate_command(command.clone())?);
                }

                Ok(pseudo)
            }
            Command::Assign(identifier, expression) => {
                let pid = unwrap_identifier(identifier.clone());
                self.memory_.identifiers_.insert(pid.0.clone()); // sus

                match self.memory_.get_variable(&pid.0)? {
                    Type::Iterator(_) => {
                        Err(CodeError::IteratorUsedAsVariable(pid.0.clone(), pid.1))?
                    }
                    _ => {}
                }

                pseudo.extend(self.memory_.load_identifier_addr(identifier)?);
                pseudo.push(Instruction::Store(9));
                pseudo.extend(self.translate_expression(expression)?);
                pseudo.push(Instruction::StoreI(9));
                Ok(pseudo)
            }
            Command::If(condition, then_commands, else_commands) => {
                let mut then_pseudo = vec![];
                let mut else_pseudo = vec![];

                for command in then_commands {
                    then_pseudo.extend(self.translate_command(command)?);
                }
                if let Some(else_commands) = else_commands.clone() {
                    for command in else_commands {
                        else_pseudo.extend(self.translate_command(command)?);
                    }
                }
                let then_psuedo_size: u64 = then_pseudo.iter().map(|i| i.len()).sum();
                let else_psuedo_size: u64 = else_pseudo.iter().map(|i| i.len()).sum();

                pseudo.extend(self.translate_condition(condition)?);

                if !else_pseudo.is_empty() {
                    pseudo.push(Instruction::Jzero((then_psuedo_size + 2) as i64));
                    pseudo.extend(then_pseudo);

                    pseudo.push(Instruction::Jump((else_psuedo_size + 1) as i64));
                    pseudo.extend(else_pseudo);
                } else {
                    pseudo.push(Instruction::Jzero((then_psuedo_size + 1) as i64));
                    pseudo.extend(then_pseudo);
                }

                Ok(pseudo)
            }
            Command::Repeat(commands, condition) => {
                let mut loop_pseudo = vec![];

                for command in commands {
                    loop_pseudo.extend(self.translate_command(command)?);
                }
                let loop_pseudo_size: u64 = loop_pseudo.iter().map(|i| i.len()).sum();

                let mut condition_pseudo = vec![];
                condition_pseudo.extend(self.translate_condition(condition.clone())?);
                let condition_pseudo_size: u64 = condition_pseudo.iter().map(|i| i.len()).sum();

                pseudo.extend(loop_pseudo);
                pseudo.extend(condition_pseudo);
                pseudo.push(Instruction::Jzero(
                    -((loop_pseudo_size + condition_pseudo_size) as i64),
                ));

                Ok(pseudo)
            }
            Command::While(condition, commands) => {
                let mut loop_pseudo = vec![];

                for command in commands {
                    loop_pseudo.extend(self.translate_command(command)?);
                }
                let loop_pseudo_size: u64 = loop_pseudo.iter().map(|i| i.len()).sum();

                let mut condition_pseudo = vec![];
                condition_pseudo.extend(self.translate_condition(condition.clone())?);
                let condition_pseudo_size: u64 = condition_pseudo.iter().map(|i| i.len()).sum();

                pseudo.extend(condition_pseudo);
                pseudo.push(Instruction::Jzero((loop_pseudo_size + 2) as i64));
                pseudo.extend(loop_pseudo);
                pseudo.push(Instruction::Jump(
                    -((loop_pseudo_size + condition_pseudo_size + 1) as i64),
                ));

                Ok(pseudo)
            }
            Command::For(iterator_pid, from, direction, to, commands) => {
                // if let Some(_) = self.memory_.table_.get(&iterator_pid.0) {
                //     // TODO maybe better exception name?
                //     Err(CodeError::IteratorUsedAsVariable(
                //         iterator_pid.0.clone(),
                //         iterator_pid.1,
                //     ))?
                // }
                // pseudo.extend(
                self.memory_
                    .declare_variable(iterator_pid.0.clone(), DeclType::Iterator)?;
                // .unwrap_or(vec![]),
                // );
                self.memory_.identifiers_.insert(iterator_pid.0.clone());

                let iterator_addr = self.memory_.addr_from_url(iterator_pid.0.clone())?;
                pseudo.extend(self.memory_.load_value(from)?);
                pseudo.push(Instruction::Store(iterator_addr));

                let condition = match direction {
                    LoopDirection::Up => {
                        Condition::LEQ(Value::Id(Identifier::Pid(iterator_pid.clone())), to)
                    }
                    LoopDirection::Down => {
                        Condition::GEQ(Value::Id(Identifier::Pid(iterator_pid.clone())), to)
                    }
                };

                let mut loop_pseudo = vec![];

                for command in commands {
                    loop_pseudo.extend(self.translate_command(command)?);
                }
                let loop_pseudo_size: u64 = loop_pseudo.iter().map(|i| i.len()).sum();

                let mut condition_pseudo = vec![];
                condition_pseudo.extend(self.translate_condition(condition.clone())?);
                let condition_pseudo_size: u64 = condition_pseudo.iter().map(|i| i.len()).sum();

                pseudo.extend(condition_pseudo);
                pseudo.push(Instruction::Jzero((loop_pseudo_size + 5) as i64));
                pseudo.extend(loop_pseudo);
                pseudo.push(Instruction::Load(iterator_addr as u64));
                match direction {
                    LoopDirection::Up => pseudo.push(Instruction::Add(51)),
                    LoopDirection::Down => pseudo.push(Instruction::Sub(51)),
                }
                pseudo.push(Instruction::Store(iterator_addr));
                pseudo.push(Instruction::Jump(
                    -((loop_pseudo_size + condition_pseudo_size + 4) as i64),
                ));

                Ok(pseudo)
            }
            Command::Read(identifier) => {
                let pid = unwrap_identifier(identifier.clone());
                self.memory_.identifiers_.insert(pid.0.clone());
                pseudo.extend(self.memory_.load_identifier_addr(identifier)?);
                pseudo.push(Instruction::Store(1));
                pseudo.push(Instruction::Get(0));
                pseudo.push(Instruction::StoreI(1));
                Ok(pseudo)
            }
            Command::Write(value) => {
                pseudo.extend(self.memory_.load_value(value)?);
                pseudo.push(Instruction::Put(0));
                Ok(pseudo)
            }
        }
    }

    fn translate_expression(
        &mut self,
        expression: Expression,
    ) -> Result<Vec<Instruction>, CodeError> {
        let mut pseudo: Vec<Instruction> = vec![];
        match expression {
            Expression::Val(value) => {
                pseudo.extend(self.memory_.load_value(value)?);
                Ok(pseudo)
            }
            Expression::ADD(value1, value2) => {
                pseudo.extend(self.memory_.load_value(value2)?);
                pseudo.push(Instruction::Store(30));
                pseudo.extend(self.memory_.load_value(value1)?);
                pseudo.push(Instruction::Add(30));
                Ok(pseudo)
            }
            Expression::SUB(value1, value2) => {
                pseudo.extend(self.memory_.load_value(value2)?);
                pseudo.push(Instruction::Store(30));
                pseudo.extend(self.memory_.load_value(value1)?);
                pseudo.push(Instruction::Sub(30));
                Ok(pseudo)
            }
            Expression::MUL(value1, value2) => {
                // # r30:  multiplicand
                // # r31:  multiplier
                // # r32:  result
                // # r37:  negative flag
                //
                pseudo.extend(self.memory_.load_value(value1)?);
                pseudo.push(Instruction::Store(30)); // multiplicand to r30
                pseudo.extend(self.memory_.load_value(value2)?);
                pseudo.push(Instruction::Mul(30));
                Ok(pseudo)
            }
            Expression::DIV(value1, value2) => {
                //                 # r30:  dividend
                // # r31:  divisor
                // # r32:  quotient - Q
                // # r33:  remainder - R
                // # r34:  temp divisor - D
                // # r35:  1
                // # r36:  multiple - M
                // # r37:  negative flag

                pseudo.extend(self.memory_.load_value(value1)?);
                pseudo.push(Instruction::Store(30)); // multiplicand to r30
                pseudo.extend(self.memory_.load_value(value2)?);
                pseudo.push(Instruction::Div(30));
                Ok(pseudo)
            }
            Expression::MOD(value1, value2) => {
                // # r30:  dividend
                // # r31:  divisor
                // # r32:  quotient - Q
                // # r33:  remainder - R
                // # r34:  temp divisor - D
                // # r35:  1
                // # r36:  multiple - M
                // # r37:  negative flag

                pseudo.extend(self.memory_.load_value(value1)?);
                pseudo.push(Instruction::Store(30)); // multiplicand to r30
                pseudo.extend(self.memory_.load_value(value2)?);
                pseudo.push(Instruction::Mod(30));
                Ok(pseudo)
            }
        }
    }

    pub fn final_translation(&self) -> String {
        let mut assembly: Vec<String> = Vec::new();
        for instruction in &self.pseudo_ {
            match instruction {
                Instruction::Get(i) => assembly.push(format!("GET {i}\n")),
                Instruction::Put(i) => assembly.push(format!("PUT {i}\n")),
                Instruction::Load(i) => assembly.push(format!("LOAD {i}\n")),
                Instruction::Store(i) => assembly.push(format!("STORE {i}\n")),
                Instruction::LoadI(i) => assembly.push(format!("LOADI {i}\n")),
                Instruction::StoreI(i) => assembly.push(format!("STOREI {i}\n")),
                Instruction::Add(i) => assembly.push(format!("ADD {i}\n")),
                Instruction::Sub(i) => assembly.push(format!("SUB {i}\n")),
                Instruction::AddI(i) => assembly.push(format!("ADDI {i}\n")),
                Instruction::SubI(i) => assembly.push(format!("SUBI {i}\n")),
                Instruction::Set(i) => assembly.push(format!("SET {i}\n")),
                Instruction::Half => assembly.push("HALF\n".to_string()),
                Instruction::Jump(addr) => assembly.push(format!("JUMP {}\n", addr)),
                Instruction::Jpos(addr) => assembly.push(format!("JPOS {}\n", addr)),
                Instruction::Jzero(addr) => assembly.push(format!("JZERO {}\n", addr)),
                Instruction::Jneg(addr) => assembly.push(format!("JNEG {}\n", addr)),
                Instruction::Rtrn(_) => todo!(),
                Instruction::Halt => assembly.push("HALT\n".to_string()),
                Instruction::Mul(addr) => {
                    assembly.push(format!("STORE {}\n", 31)); // multiplier to r31
                    assembly.push(format!("LOAD {}\n", 50)); // multiplier to r31

                    assembly.push(format!("STORE {}\n", 32));
                    assembly.push(format!("LOAD {}\n", 51)); // negative flag to r37
                    assembly.push(format!("STORE {}\n", 37));

                    // # multiplicand flag setup
                    assembly.push(format!("LOAD {}\n", addr));
                    assembly.push(format!("JPOS {}\n", 10));
                    assembly.push(format!("JZERO {}\n", 9));
                    assembly.push(format!("LOAD {}\n", 37));
                    assembly.push(format!("SUB {}\n", 37));
                    assembly.push(format!("SUB {}\n", 37));
                    assembly.push(format!("STORE {}\n", 37));
                    // make multiplicand pos
                    assembly.push(format!("LOAD {}\n", addr));
                    assembly.push(format!("SUB {}\n", addr));
                    assembly.push(format!("SUB {}\n", addr));
                    assembly.push(format!("STORE {}\n", addr));

                    // # multiplier flag setup
                    assembly.push(format!("LOAD {}\n", 31));
                    assembly.push(format!("JPOS {}\n", 10));
                    assembly.push(format!("JZERO {}\n", 9));
                    assembly.push(format!("LOAD {}\n", 37));
                    assembly.push(format!("SUB {}\n", 37));
                    assembly.push(format!("SUB {}\n", 37));
                    assembly.push(format!("STORE {}\n", 37));
                    // make multiplier pos
                    assembly.push(format!("LOAD {}\n", 31));
                    assembly.push(format!("SUB {}\n", 31));
                    assembly.push(format!("SUB {}\n", 31));
                    assembly.push(format!("STORE {}\n", 31));

                    // if multiplicand < multiplier: SWAP them
                    assembly.push(format!("LOAD {}\n", addr));
                    assembly.push(format!("SUB {}\n", 31));
                    assembly.push(format!("JPOS {}\n", 10));
                    // a = a + b
                    // b = a - b
                    // a = a - b
                    assembly.push(format!("LOAD {}\n", addr));
                    assembly.push(format!("ADD {}\n", 31));
                    assembly.push(format!("STORE {}\n", addr));
                    assembly.push(format!("LOAD {}\n", addr));
                    assembly.push(format!("SUB {}\n", 31));
                    assembly.push(format!("STORE {}\n", 31));
                    assembly.push(format!("LOAD {}\n", addr));
                    assembly.push(format!("SUB {}\n", 31));
                    assembly.push(format!("STORE {}\n", addr));

                    // BEGIN WHILE
                    // while multiplier > 0:
                    assembly.push(format!("LOAD {}\n", 31));
                    assembly.push(format!("JZERO {}\n", 17));
                    assembly.push(format!("JNEG {}\n", 16));

                    // BEGIN IF
                    // if multiplier % 2 == 1:  # CHECK If multiplier is odd
                    // if multiplier / 2 * 2 == multiplier:
                    // multiplier is divisible BY 2
                    // else:
                    // multiplier is not divisible BY 2
                    assembly.push(format!("LOAD {}\n", 31));
                    assembly.push("HALF\n".to_string());
                    assembly.push(format!("ADD {}\n", 0));
                    assembly.push(format!("SUB {}\n", 31));
                    assembly.push(format!("JZERO {}\n", 4)); // jump if even

                    // result = result + multiplicand
                    assembly.push(format!("LOAD {}\n", 32));
                    assembly.push(format!("ADD {}\n", addr));
                    assembly.push(format!("STORE {}\n", 32));
                    // END IF

                    // multiplicand = multiplicand * 2
                    assembly.push(format!("LOAD {}\n", addr));
                    assembly.push(format!("ADD {}\n", addr));
                    assembly.push(format!("STORE {}\n", addr));
                    // multiplier = multiplier / 2      # Halve the multiplier
                    assembly.push(format!("LOAD {}\n", 31));
                    assembly.push("HALF\n".to_string());
                    assembly.push(format!("STORE {}\n", 31));

                    assembly.push(format!("JUMP {}\n", -17));
                    // END WHILE

                    // if flag < 0; change result SIGN
                    assembly.push(format!("LOAD {}\n", 37));
                    assembly.push(format!("JPOS {}\n", 5));
                    assembly.push(format!("LOAD {}\n", 32));
                    assembly.push(format!("SUB {}\n", 32));
                    assembly.push(format!("SUB {}\n", 32));
                    assembly.push(format!("STORE {}\n", 32));

                    // load result
                    assembly.push(format!("LOAD {}\n", 32));
                }
                Instruction::Div(addr) => {
                    // Initialize registers
                    assembly.push(format!("STORE {}\n", 31));
                    assembly.push(format!("LOAD {}\n", 51));
                    assembly.push(format!("STORE {}\n", 35));
                    assembly.push(format!("LOAD {}\n", 51));
                    assembly.push(format!("STORE {}\n", 37));

                    // # Dividend flag setup
                    assembly.push(format!("LOAD {}\n", addr));
                    assembly.push(format!("JPOS {}\n", 10));
                    assembly.push(format!("JZERO {}\n", 9));
                    assembly.push(format!("LOAD {}\n", 37));
                    assembly.push(format!("SUB {}\n", 37));
                    assembly.push(format!("SUB {}\n", 37));
                    assembly.push(format!("STORE {}\n", 37));

                    assembly.push(format!("LOAD {}\n", addr));
                    assembly.push(format!("SUB {}\n", addr));
                    assembly.push(format!("SUB {}\n", addr));
                    assembly.push(format!("STORE {}\n", addr));

                    // # Divisor flag setup
                    assembly.push(format!("LOAD {}\n", 31));
                    assembly.push(format!("JPOS {}\n", 10));
                    assembly.push(format!("JZERO {}\n", 9));
                    assembly.push(format!("LOAD {}\n", 37));
                    assembly.push(format!("SUB {}\n", 37));
                    assembly.push(format!("SUB {}\n", 37));
                    assembly.push(format!("STORE {}\n", 37));

                    assembly.push(format!("LOAD {}\n", 31));
                    assembly.push(format!("SUB {}\n", 31));
                    assembly.push(format!("SUB {}\n", 31));
                    assembly.push(format!("STORE {}\n", 31));

                    // # Setup Q and R
                    assembly.push(format!("LOAD {}\n", 50));
                    assembly.push(format!("STORE {}\n", 32));
                    assembly.push(format!("LOAD {}\n", addr));
                    assembly.push(format!("STORE {}\n", 33));

                    // Check for division by zero
                    assembly.push(format!("LOAD {}\n", 31));
                    assembly.push(format!("JZERO {}\n", 32));

                    // BEGIN WHILE_2: while divisor <= remainder
                    assembly.push(format!("LOAD {}\n", 33));
                    assembly.push(format!("SUB {}\n", 31));
                    assembly.push(format!("JNEG {}\n", 23));

                    // Before loop1: Initialize temp_divisor and multiple
                    assembly.push(format!("LOAD {}\n", 31));
                    assembly.push(format!("STORE {}\n", 34));
                    assembly.push(format!("LOAD {}\n", 35));
                    assembly.push(format!("STORE {}\n", 36));

                    // BEGIN WHILE_1: while (temp_divisor * 2) <= R
                    assembly.push(format!("LOAD {}\n", 33));
                    assembly.push(format!("SUB {}\n", 34));
                    assembly.push(format!("SUB {}\n", 34));
                    assembly.push(format!("JNEG {}\n", 8));

                    // Temp_divisor = temp_divisor * 2
                    assembly.push(format!("LOAD {}\n", 34));
                    assembly.push(format!("ADD {}\n", 34));
                    assembly.push(format!("STORE {}\n", 34));

                    // Multiple = multiple * 2
                    assembly.push(format!("LOAD {}\n", 36));
                    assembly.push(format!("ADD {}\n", 36));
                    assembly.push(format!("STORE {}\n", 36));

                    // Jump back to BEGIN WHILE_1
                    assembly.push(format!("JUMP {}\n", -10));

                    // After loop1: Update R and Q
                    // R = R - temp_divisor
                    assembly.push(format!("LOAD {}\n", 33));
                    assembly.push(format!("SUB {}\n", 34));
                    assembly.push(format!("STORE {}\n", 33));

                    assembly.push(format!("LOAD {}\n", 32));
                    assembly.push(format!("ADD {}\n", 36));
                    assembly.push(format!("STORE {}\n", 32));

                    // Jump back to BEGIN WHILE_2
                    assembly.push(format!("JUMP {}\n", -24));

                    // END WHILE_2

                    // If flag < 0; change result SIGN
                    assembly.push(format!("LOAD {}\n", 37));
                    assembly.push(format!("JPOS {}\n", 5));
                    assembly.push(format!("LOAD {}\n", 32));
                    assembly.push(format!("SUB {}\n", 32));
                    assembly.push(format!("SUB {}\n", 32));
                    assembly.push(format!("STORE {}\n", 32));

                    // Return quotient
                    assembly.push(format!("LOAD {}\n", 32));
                }
                Instruction::Mod(addr) => {
                    assembly.push(format!("STORE {}\n", 31));
                    assembly.push(format!("LOAD {}\n", 51));
                    assembly.push(format!("STORE {}\n", 35));
                    assembly.push(format!("LOAD {}\n", 51));
                    assembly.push(format!("STORE {}\n", 37));
                    assembly.push(format!("LOAD {}\n", addr));
                    assembly.push(format!("JPOS {}\n", 6));
                    assembly.push(format!("JZERO {}\n", 5));
                    assembly.push(format!("STORE {}\n", 37));
                    assembly.push(format!("LOAD {}\n", addr));
                    assembly.push(format!("SUB {}\n", addr));
                    assembly.push(format!("SUB {}\n", addr));
                    assembly.push(format!("STORE {}\n", addr));
                    assembly.push(format!("LOAD {}\n", 31));
                    assembly.push(format!("JPOS {}\n", 10));
                    assembly.push(format!("JZERO {}\n", 9));
                    assembly.push(format!("LOAD {}\n", 37));
                    assembly.push(format!("SUB {}\n", 37));
                    assembly.push(format!("SUB {}\n", 37));
                    assembly.push(format!("STORE {}\n", 37));
                    assembly.push(format!("LOAD {}\n", 31));
                    assembly.push(format!("SUB {}\n", 31));
                    assembly.push(format!("SUB {}\n", 31));
                    assembly.push(format!("STORE {}\n", 31));
                    assembly.push(format!("LOAD {}\n", 50));
                    assembly.push(format!("STORE {}\n", 32));
                    assembly.push(format!("LOAD {}\n", addr));
                    assembly.push(format!("STORE {}\n", 33));
                    assembly.push(format!("LOAD {}\n", 31));
                    assembly.push(format!("JZERO {}\n", 32));
                    assembly.push(format!("LOAD {}\n", 33));
                    assembly.push(format!("SUB {}\n", 31));
                    assembly.push(format!("JNEG {}\n", 23));
                    assembly.push(format!("LOAD {}\n", 31));
                    assembly.push(format!("STORE {}\n", 34));
                    assembly.push(format!("LOAD {}\n", 35));
                    assembly.push(format!("STORE {}\n", 36));
                    assembly.push(format!("LOAD {}\n", 33));
                    assembly.push(format!("SUB {}\n", 34));
                    assembly.push(format!("SUB {}\n", 34));
                    assembly.push(format!("JNEG {}\n", 8));
                    assembly.push(format!("LOAD {}\n", 34));
                    assembly.push(format!("ADD {}\n", 34));
                    assembly.push(format!("STORE {}\n", 34));
                    assembly.push(format!("LOAD {}\n", 36));
                    assembly.push(format!("ADD {}\n", 36));
                    assembly.push(format!("STORE {}\n", 36));
                    assembly.push(format!("JUMP {}\n", -10));
                    assembly.push(format!("LOAD {}\n", 33));
                    assembly.push(format!("SUB {}\n", 34));
                    assembly.push(format!("STORE {}\n", 33));
                    assembly.push(format!("LOAD {}\n", 32));
                    assembly.push(format!("ADD {}\n", 36));
                    assembly.push(format!("STORE {}\n", 32));
                    assembly.push(format!("JUMP {}\n", -24));
                    assembly.push(format!("LOAD {}\n", 37));
                    assembly.push(format!("JPOS {}\n", 5));
                    assembly.push(format!("LOAD {}\n", 33));
                    assembly.push(format!("SUB {}\n", 33));
                    assembly.push(format!("SUB {}\n", 33));
                    assembly.push(format!("STORE {}\n", 33));
                    assembly.push(format!("LOAD {}\n", 33));
                }
            }
        }
        let mut assembled = "".to_string();
        for i in assembly {
            assembled += &i;
        }
        assembled
    }
}
