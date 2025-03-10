#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Instruction {
    Get(u64),
    Put(u64),
    Load(u64),
    Store(u64),
    LoadI(u64),
    StoreI(u64),
    Add(u64),
    Sub(u64),
    AddI(u64),
    SubI(u64),
    Set(i64),
    Half,
    Jump(i64),
    Jpos(i64),
    Jzero(i64),
    Jneg(i64),
    Rtrn(i64),
    Halt,
    Mul(u64), // address
    Div(u64),
    Mod(u64),
}

// instruction size of pseudo operations; used for if-else
impl Instruction {
    pub(crate) fn len(&self) -> u64 {
        match self {
            Instruction::Mul(_) => 64,
            Instruction::Div(_) => 65,
            Instruction::Mod(_) => 62,
            _ => 1,
        }
    }
}
