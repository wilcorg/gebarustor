use crate::instruction::Instruction;

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

pub fn add_index_into_url(url: &String, index: i64) -> String {
    let url_cleaned = remove_index_from_url(url);
    let parts = url_cleaned.split('@').collect::<Vec<&str>>();
    format!("{}#{}@{}", parts[0], index, parts[1])
}

pub fn remove_index_from_url(url: &String) -> String {
    let parts = url.split('@').collect::<Vec<&str>>();
    let parts_pid = parts[0].split('#').collect::<Vec<&str>>();
    format!("{}@{}", parts_pid[0], parts[1])
}

pub fn create_asm_value(addr: u64, value: i64) -> Vec<Instruction> {
    let mut pseudo = vec![];
    // todo make better algorithm
    instruction_set!(pseudo, value);
    // pseudo.push(Instruction::Set(value));
    if addr != 0 {
        pseudo.push(Instruction::Store(addr));
    }
    pseudo
}

#[cfg(test)]
mod tests {
    use crate::utils::{add_index_into_url, remove_index_from_url};
    #[test]
    fn test_add_index_into_url() {
        let url = "variable@scope".to_string();
        let index = 5;
        let result = add_index_into_url(&url, index);
        assert_eq!(result, "variable#5@scope");
    }

    #[test]
    fn test_remove_index_from_url() {
        let url = "variable#5@scope".to_string();
        let result = remove_index_from_url(&url);
        assert_eq!(result, "variable@scope");
    }

    #[test]
    fn test_remove_index_from_url_2() {
        let url = "variable@scope".to_string();
        let result = remove_index_from_url(&url);
        assert_eq!(result, "variable@scope");
    }
}
