use crate::address_cache::AddressCache;
use crate::ast::{CodeError, Identifier, Value};
use crate::instruction::Instruction;
use crate::utils::create_asm_value;
use std::collections::{HashMap, HashSet};

#[derive(Debug, PartialEq)]
pub enum DeclType {
    Variable,
    Array(i64, i64), // from, to
    Iterator,
    Pointer(u64), // to
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Variable(u64),        // addr
    Array(u64, i64, i64), // addr, from, to
    Iterator(u64),        // addr
    Pointer(u64, u64),    // addr, to
}

#[derive(Debug)]
pub struct Memory {
    pub table_: HashMap<String, Type>,
    pub identifiers_: HashSet<String>,
    pub next_address_: u64,
    pub address_cache_: AddressCache,
}

impl Memory {
    pub fn new(pseudo: &mut Vec<Instruction>) -> Memory {
        let mut table: HashMap<String, Type> = HashMap::new();
        let mut identifiers: HashSet<String> = HashSet::new();

        table.insert("#internal_0".to_string(), Type::Variable(0));
        pseudo.push(Instruction::Set(0));
        pseudo.push(Instruction::Store(50));
        identifiers.insert("#internal_0".to_string());

        table.insert("#internal_1".to_string(), Type::Variable(1));
        pseudo.push(Instruction::Set(1));
        pseudo.push(Instruction::Store(51));
        identifiers.insert("#internal_1".to_string());

        Memory {
            table_: table,
            identifiers_: identifiers,
            next_address_: 500,
            address_cache_: AddressCache::new(100, 300),
        }
    }

    pub fn declare_variable(
        &mut self,
        url: String,
        class: DeclType,
    ) -> Result<Vec<Instruction>, CodeError> {
        let mut pseudo = vec![];
        // if self.table_.contains_key(&url) {
        //     return Err(CodeError::DuplicateVariableDeclaration(url, 0));
        // }

        match class {
            DeclType::Variable => {
                self.table_
                    .insert(url.clone(), Type::Variable(self.next_address_));
                pseudo.extend(
                    self.address_cache_
                        .insert(url.clone(), self.next_address_)
                        .unwrap_or(vec![]),
                );
                self.next_address_ += 1;
            }
            DeclType::Array(from, to) => {
                if (self.next_address_ as i64 - from) < 0 {
                    return Err(CodeError::IndexOutOfBounds(url.clone(), 0));
                }

                self.table_
                    .insert(url.clone(), Type::Array(self.next_address_, from, to));
                for _ in from..=to {
                    self.next_address_ += 1;
                }
            }
            DeclType::Iterator => {
                self.table_
                    .insert(url.clone(), Type::Iterator(self.next_address_));
                self.identifiers_.insert(url.clone());
                self.next_address_ += 1;
            }
            DeclType::Pointer(to_addr) => {
                self.table_
                    .insert(url.clone(), Type::Pointer(self.next_address_, to_addr));
                self.identifiers_.insert(url.clone());
                self.next_address_ += 1;
            }
        }

        Ok(pseudo)
    }

    pub fn addr_from_url(&self, url: String) -> Result<u64, CodeError> {
        let variable_type = self
            .table_
            .get(&url)
            .ok_or(CodeError::UndeclaredVariable(url.clone(), 0))?;
        match variable_type {
            Type::Variable(addr) => Ok(*addr),
            Type::Array(addr, _, _) => Ok(*addr),
            Type::Iterator(addr) => Ok(*addr),
            Type::Pointer(_, to_addr) => Ok(*to_addr), // warning: is it really correct?
        }
    }

    pub fn get_variable(&self, url: &String) -> Result<&Type, CodeError> {
        match self.table_.get(url) {
            Some(t) => Ok(&t),
            None => Err(CodeError::UndeclaredVariable(url.clone(), 0)),
        }
    }

    pub fn get_variable_by_addr(&self, addr: u64) -> Result<&Type, CodeError> {
        for (_, t) in self.table_.iter() {
            match t {
                Type::Variable(a) => {
                    if *a == addr {
                        return Ok(t);
                    }
                }
                Type::Array(a, _, _) => {
                    if *a == addr {
                        return Ok(t);
                    }
                }
                Type::Iterator(a) => {
                    if *a == addr {
                        return Ok(t);
                    }
                }
                Type::Pointer(a, _) => {
                    if *a == addr {
                        return Ok(t);
                    }
                }
            }
        }
        Err(CodeError::UndeclaredVariable("".to_string(), 0))
    }

    pub fn load_value(&self, value: Value) -> Result<Vec<Instruction>, CodeError> {
        let mut pseudo: Vec<Instruction> = vec![];
        match value {
            Value::Num(num) => {
                pseudo.extend(create_asm_value(0, num));
                Ok(pseudo)
            }
            Value::Id(identifier) => {
                match identifier.clone() {
                    Identifier::Pid(pid) => {
                        if !self.identifiers_.contains(&pid.0) {
                            Err(CodeError::UndeclaredVariable(pid.0.clone(), pid.1))?;
                        }
                    }
                    _ => {}
                }
                pseudo.extend(self.load_identifier_addr(identifier)?);
                if let Some(last) = pseudo.last_mut() {
                    match last {
                        Instruction::Load(addr) => {
                            *last = Instruction::LoadI(*addr);
                        }
                        _ => {}
                    }
                }
                Ok(pseudo)
            }
        }
    }

    pub fn load_identifier_addr(
        &self,
        identifier: Identifier,
    ) -> Result<Vec<Instruction>, CodeError> {
        let mut pseudo = vec![];
        match identifier {
            Identifier::Pid(pid) => match self.get_variable(&pid.0)? {
                Type::Variable(addr) => {
                    if let Some(cache_pseudo) = self.address_cache_.fetch(&pid.0) {
                        Ok(cache_pseudo)
                    } else {
                        println!("Warning: cache miss for {}", pid.0);
                        pseudo.extend(create_asm_value(0, *addr as i64));
                        Ok(pseudo)
                    }
                }
                Type::Array(_, _, _) => Err(CodeError::VariableUsedAsArray(pid.0.clone(), 0))?,
                Type::Iterator(addr) => {
                    pseudo.extend(create_asm_value(0, *addr as i64));
                    Ok(pseudo)
                }
                Type::Pointer(_, to) => {
                    pseudo.extend(create_asm_value(0, *to as i64));
                    Ok(pseudo)
                }
            },
            Identifier::NumIndexed(pid, index) => {
                let (addr, from, to) = match self.get_variable(&pid.0)? {
                    Type::Pointer(_, to) => {
                        let to_addr = *to;
                        match self.get_variable_by_addr(to_addr)? {
                            Type::Array(addr, from, to) => (*addr, *from, *to),
                            _ => return Err(CodeError::VariableUsedAsArray(pid.0, pid.1)),
                        }
                    }
                    Type::Array(addr, from, to) => (*addr, *from, *to),
                    _ => return Err(CodeError::VariableUsedAsArray(pid.0, pid.1)),
                };
                if index < from || index > to {
                    return Err(CodeError::IndexOutOfBounds(pid.0, pid.1));
                }

                let pid_0_addr = addr as i64 - from;
                pseudo.extend(create_asm_value(0, pid_0_addr + index));
                Ok(pseudo)
            }
            Identifier::PidIndexed(pid, index) => {
                let index_addr = match self.get_variable(&index.0)? {
                    Type::Variable(addr) => *addr,
                    Type::Array(_, _, _) => Err(CodeError::ArrayUsedAsIndex(pid.0.clone(), 0))?,
                    Type::Iterator(to_addr) => *to_addr,
                    Type::Pointer(_, to) => *to,
                };
                pseudo.extend(create_asm_value(0, index_addr as i64));

                let pid_0_addr = match self.get_variable(&pid.0)? {
                    Type::Array(addr, from, _) => *addr as i64 - *from,
                    Type::Pointer(_, to) => {
                        let to_addr = *to;
                        match self.get_variable_by_addr(to_addr)? {
                            Type::Array(addr, from, _) => *addr as i64 - *from,
                            _ => return Err(CodeError::VariableUsedAsArray(pid.0, pid.1)),
                        }
                    }
                    _ => return Err(CodeError::VariableUsedAsArray(pid.0, pid.1)),
                };
                pseudo.push(Instruction::LoadI(0));
                pseudo.push(Instruction::Store(1));

                pseudo.extend(create_asm_value(0, pid_0_addr));
                pseudo.push(Instruction::Add(1));
                Ok(pseudo)
            }
        }
    }
}
