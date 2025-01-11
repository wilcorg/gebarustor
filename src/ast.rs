pub type Num = u64;
pub type Pidentifier = (String, usize);
#[derive(Debug, Clone)]
pub enum LoopDirection {
    Up,
    Down,
}

#[derive(Debug, Clone)]
pub enum Identifier {
    Base(Pidentifier),
    NumIndexed(Pidentifier, Num),  // TODO why this order?
    PidIndexed(Pidentifier, Pidentifier),
}

#[derive(Debug, Clone)]
pub enum Value {
    Num(Num),
    Id(Identifier),
}

#[derive(Debug, Clone)]
pub enum Condition {
    Equal(Value, Value),
    NotEqual(Value, Value),
    Greater(Value, Value),
    Lower(Value, Value),
    GreaterOrEqual(Value, Value),
    LowerOrEqual(Value, Value),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Val(Value),
    Add(Value, Value),
    Subtract(Value, Value),
    Multiply(Value, Value),
    Divide(Value, Value),
    Modulo(Value, Value),
}

pub type Arguments = Vec<Pidentifier>;

#[derive(Debug, Clone)]
pub enum ArgumentsDeclarationVariant {
    Base(Pidentifier),
    Table(Pidentifier),
}

pub type ArgumentsDeclaration = Vec<ArgumentsDeclarationVariant>;

#[derive(Debug, Clone)]
pub enum DeclarationVariant {
    Base(Pidentifier),
    // NumIndexed(Pidentifier, Num),
    NumIndexed(Pidentifier, Num, Num)
}

pub type Declarations = Vec<DeclarationVariant>;

pub type ProcedureCall = (Pidentifier, Arguments);

pub type ProcedureHead = (Pidentifier, ArgumentsDeclaration);

#[derive(Debug, Clone)]
pub enum Command {
    Assign(Identifier, Expression),
    If(Condition, Commands, Option<Commands>),
    While(Condition, Commands),
    Repeat(Commands, Condition),
    For(Pidentifier, Value, LoopDirection, Value, Commands),  // modified
    ProcCall(ProcedureCall),
    Read(Identifier),
    Write(Value),
}


pub type Commands = Vec<Command>;

pub type Main = (Option<Declarations>, Commands);

pub type Procedure = (ProcedureHead, Option<Declarations>, Commands);

pub type Procedures = Vec<Procedure>;

pub type Program = (Option<Procedures>, Main);
