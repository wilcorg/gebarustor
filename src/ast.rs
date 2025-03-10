pub type Num = i64;
pub type Pidentifier = (String, usize);
#[derive(Debug, Clone)]
pub enum LoopDirection {
    Up,
    Down,
}

#[derive(Debug, Clone)]
pub enum Identifier {
    Pid(Pidentifier), // todo rename to Pid
    NumIndexed(Pidentifier, Num),
    PidIndexed(Pidentifier, Pidentifier),
}

#[derive(Debug, Clone)]
pub enum Value {
    Num(Num),
    Id(Identifier),
}

#[derive(Debug, Clone)]
pub enum Condition {
    EQ(Value, Value),
    NEQ(Value, Value),
    GT(Value, Value),
    LT(Value, Value),
    GEQ(Value, Value),
    LEQ(Value, Value),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Val(Value),
    ADD(Value, Value),
    SUB(Value, Value),
    MUL(Value, Value),
    DIV(Value, Value),
    MOD(Value, Value),
}

pub type Arguments = Vec<Pidentifier>;

#[derive(Debug, Clone)]
pub enum ArgumentsDeclarationVariant {
    Val(Pidentifier),
    Array(Pidentifier),
}

pub type ArgumentsDeclaration = Vec<ArgumentsDeclarationVariant>;

#[derive(Debug, Clone)]
pub enum VarDeclVariant {
    Val(Pidentifier),
    NumIndexed(Pidentifier, Num, Num),
}

pub type Declarations = Vec<VarDeclVariant>;

pub type ProcedureCall = (Pidentifier, Arguments);

pub type ProcedureHead = (Pidentifier, ArgumentsDeclaration);

#[derive(Debug, Clone)]
pub enum Command {
    Assign(Identifier, Expression),
    If(Condition, Commands, Option<Commands>),
    While(Condition, Commands),
    Repeat(Commands, Condition),
    For(Pidentifier, Value, LoopDirection, Value, Commands),
    ProcCall(ProcedureCall),
    Read(Identifier),
    Write(Value),
}

pub type Commands = Vec<Command>;

pub type Main = (Option<Declarations>, Commands);

pub type Procedure = (ProcedureHead, Option<Declarations>, Commands);

pub type Procedures = Vec<Procedure>;

pub type Program = (Option<Procedures>, Main);

#[derive(Debug, Clone)]
pub enum CodeError {
    UndeclaredVariable(String, usize),
    UninitializedVariable(String, usize),
    IteratorUsedAsVariable(String, usize),
    UndeclaredProcedure(String, usize),
    IndexOutOfBounds(String, usize),
    VariableUsedAsArray(String, usize),
    ArrayUsedAsIndex(String, usize),
    WrongArgumentType(String, usize),
    WrongNumberOfArguments(String, usize),
    DuplicateVariableDeclaration(String, usize),
    DuplicateProcedureDeclaration(String, usize),
    RecursiveProcedureCall(String, usize),
}

impl CodeError {
    pub fn get_var_ptr(&self) -> usize {
        match self {
            CodeError::UndeclaredVariable(_, var_ptr) => *var_ptr,
            CodeError::UndeclaredProcedure(_, var_ptr) => *var_ptr,
            CodeError::IndexOutOfBounds(_, var_ptr) => *var_ptr,
            CodeError::VariableUsedAsArray(_, var_ptr) => *var_ptr,
            CodeError::ArrayUsedAsIndex(_, var_ptr) => *var_ptr,
            CodeError::WrongArgumentType(_, var_ptr) => *var_ptr,
            CodeError::WrongNumberOfArguments(_, var_ptr) => *var_ptr,
            CodeError::DuplicateVariableDeclaration(_, var_ptr) => *var_ptr,
            CodeError::DuplicateProcedureDeclaration(_, var_ptr) => *var_ptr,
            CodeError::RecursiveProcedureCall(_, var_ptr) => *var_ptr,
            CodeError::UninitializedVariable(_, var_ptr) => *var_ptr,
            CodeError::IteratorUsedAsVariable(_, var_ptr) => *var_ptr,
        }
    }
}

pub fn unwrap_identifier(identifier: Identifier) -> Pidentifier {
    match identifier {
        Identifier::Pid(pid) => pid,
        Identifier::NumIndexed(pid, _) => pid,
        Identifier::PidIndexed(pid, _) => pid,
    }
}
