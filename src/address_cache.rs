use crate::instruction::Instruction;
use std::collections::HashMap;

macro_rules! instruction_set {
    ($pseudo:expr, $addr:expr) => {
        if $addr == 0 {
            $pseudo.push(Instruction::Load(50));
        } else if $addr == 1 {
            $pseudo.push(Instruction::Load(51));
        } else {
            $pseudo.push(Instruction::Set($addr as i64));
        }
    };
}

#[derive(Debug)]
pub struct AddressCache {
    from_addr: u64,
    to_addr: u64,
    current_addr: u64,
    pid_addr: HashMap<String, u64>, // pid_name -> cache_addr
}

impl AddressCache {
    pub fn new(from_addr: u64, to_addr: u64) -> AddressCache {
        AddressCache {
            from_addr,
            to_addr,
            current_addr: from_addr,
            pid_addr: HashMap::new(),
        }
    }

    pub fn insert(&mut self, url: String, addr: u64) -> Option<Vec<Instruction>> {
        let mut pseudo = vec![];
        // todo what with arrays? 0 index?
        if self.current_addr < self.from_addr {
            return None;
        }

        println!("cache: saving {} in address: {}", &url, self.current_addr);
        self.pid_addr.insert(url, self.current_addr);
        // todo this set can be optimized
        instruction_set!(pseudo, addr);
        // pseudo.push(Instruction::Set(addr as i64));
        pseudo.push(Instruction::Store(self.current_addr));
        self.current_addr += 1;
        Some(pseudo)
    }

    pub fn fetch(&self, url: &String) -> Option<Vec<Instruction>> {
        let mut pseudo = vec![];
        let addr = self.pid_addr.get(url);
        println!("cache: fetching {} from address: {:?}", &url, addr);

        if addr.is_none() {
            None
        } else {
            pseudo.push(Instruction::Load(*addr.unwrap()));
            Some(pseudo)
        }
    }
}
