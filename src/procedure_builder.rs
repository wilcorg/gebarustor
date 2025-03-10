use crate::ast::*;

#[derive(Debug, Clone)]
pub struct ProcedureBuilder {
    pub name: String,
    pub declared_arguments: Vec<ArgumentsDeclarationVariant>,
    pub declarations: Option<Declarations>,
    pub commands: Commands,
}

macro_rules! rename_commands {
    ($self:ident, $commands:expr) => {
        $commands
            .iter()
            .cloned()
            .map(|com| $self.rename_command(com))
            .collect()
    };
}

impl ProcedureBuilder {
    pub fn new(procedure: Procedure) -> ProcedureBuilder {
        let mut procedure_builder = ProcedureBuilder {
            name: procedure.0 .0 .0,
            declared_arguments: procedure.0 .1,
            declarations: procedure.1,
            commands: procedure.2,
        };
        procedure_builder.rename_commands();
        procedure_builder
    }

    pub fn rename_commands(&mut self) {
        // TODO for loop there
        let new_commands: Vec<Command> = rename_commands!(self, self.commands);
        self.commands = new_commands;
    }

    pub fn rename_command(&self, command: Command) -> Command {
        match command {
            Command::Assign(id, expression) => {
                let new_id = self.rename_indentifier(id);
                let new_expression = match expression {
                    Expression::Val(value) => {
                        let new_value = self.rename_value(value);
                        Expression::Val(new_value)
                    }
                    Expression::ADD(value0, value1) => {
                        let new_value0 = self.rename_value(value0);
                        let new_value1 = self.rename_value(value1);
                        Expression::ADD(new_value0, new_value1)
                    }
                    Expression::SUB(value0, value1) => {
                        let new_value0 = self.rename_value(value0);
                        let new_value1 = self.rename_value(value1);
                        Expression::SUB(new_value0, new_value1)
                    }
                    Expression::MUL(value0, value1) => {
                        let new_value0 = self.rename_value(value0);
                        let new_value1 = self.rename_value(value1);
                        Expression::MUL(new_value0, new_value1)
                    }
                    Expression::DIV(value0, value1) => {
                        let new_value0 = self.rename_value(value0);
                        let new_value1 = self.rename_value(value1);
                        Expression::DIV(new_value0, new_value1)
                    }
                    Expression::MOD(value0, value1) => {
                        let new_value0 = self.rename_value(value0);
                        let new_value1 = self.rename_value(value1);
                        Expression::MOD(new_value0, new_value1)
                    }
                };
                Command::Assign(new_id, new_expression)
            }
            Command::If(condition, commands, else_commands) => {
                let new_condition = self.rename_condition(condition);
                let new_commands: Vec<Command> = rename_commands!(self, commands);
                let new_else_condition: Option<Vec<Command>> = else_commands.map(|else_commands| {
                    else_commands
                        .iter()
                        .cloned()
                        .map(|com| self.rename_command(com))
                        .collect()
                });
                Command::If(new_condition, new_commands, new_else_condition)
            }
            Command::While(condition, commands) => {
                let new_condition = self.rename_condition(condition);
                let new_commands: Vec<Command> = rename_commands!(self, commands);
                Command::While(new_condition, new_commands)
            }
            Command::Repeat(commands, condition) => {
                let new_condition = self.rename_condition(condition);
                let new_commands: Vec<Command> = rename_commands!(self, commands);
                Command::Repeat(new_commands, new_condition)
            }
            Command::For(pidentifier, from, direction, to, commands) => {
                // TODO add checks
                let new_pidentifier = self.rename_pidentifier(pidentifier);
                let new_from = self.rename_value(from);
                let new_to = self.rename_value(to);
                let new_commands: Vec<Command> = rename_commands!(self, commands);
                Command::For(new_pidentifier, new_from, direction, new_to, new_commands)
            }
            Command::ProcCall((name, arguments)) => {
                let new_arguments: Vec<(String, usize)> = arguments
                    .iter()
                    .map(|arg| (format!("{}@{}", arg.0, self.name), arg.1))
                    .collect();
                Command::ProcCall((name, new_arguments))
            }
            Command::Read(identifier) => {
                let new_identifier = self.rename_indentifier(identifier);
                Command::Read(new_identifier)
            }
            Command::Write(value) => {
                let new_value = self.rename_value(value);
                Command::Write(new_value)
            }
        }
    }

    pub fn rename_condition(&self, condition: Condition) -> Condition {
        match condition {
            Condition::EQ(value0, value1) => {
                let new_value0 = self.rename_value(value0);
                let new_value1 = self.rename_value(value1);
                Condition::EQ(new_value0, new_value1)
            }
            Condition::NEQ(value0, value1) => {
                let new_value0 = self.rename_value(value0);
                let new_value1 = self.rename_value(value1);
                Condition::NEQ(new_value0, new_value1)
            }
            Condition::GT(value0, value1) => {
                let new_value0 = self.rename_value(value0);
                let new_value1 = self.rename_value(value1);
                Condition::GT(new_value0, new_value1)
            }
            Condition::LT(value0, value1) => {
                let new_value0 = self.rename_value(value0);
                let new_value1 = self.rename_value(value1);
                Condition::LT(new_value0, new_value1)
            }
            Condition::GEQ(value0, value1) => {
                let new_value0 = self.rename_value(value0);
                let new_value1 = self.rename_value(value1);
                Condition::GEQ(new_value0, new_value1)
            }
            Condition::LEQ(value0, value1) => {
                let new_value0 = self.rename_value(value0);
                let new_value1 = self.rename_value(value1);
                Condition::LEQ(new_value0, new_value1)
            }
        }
    }

    fn rename_value(&self, value: Value) -> Value {
        match value {
            Value::Num(_) => value.clone(),
            Value::Id(id) => Value::Id(self.rename_indentifier(id)),
        }
    }

    fn rename_indentifier(&self, identifier: Identifier) -> Identifier {
        match identifier {
            Identifier::Pid(id) => Identifier::Pid((format!("{}@{}", id.0, self.name), id.1)),
            Identifier::NumIndexed(id, num) => {
                Identifier::NumIndexed((format!("{}@{}", id.0, self.name), id.1), num)
            }
            Identifier::PidIndexed(id, index_id) => Identifier::PidIndexed(
                (format!("{}@{}", id.0, self.name), id.1),
                (format!("{}@{}", index_id.0, self.name), id.1),
            ),
        }
    }

    fn rename_pidentifier(&self, pidentifier: Pidentifier) -> Pidentifier {
        // TODO const for iterator
        (format!("{}@{}", pidentifier.0, self.name), pidentifier.1)
    }
}
