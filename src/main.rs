mod address_cache;
mod ast;
mod instruction;
mod memory;
mod procedure_builder;
mod translator;
mod utils;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub lexparse);

use std::env;
use std::fs;
use std::fs::File;
use std::io::{BufRead, BufReader};

use crate::ast::CodeError;
use translator::*;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 2 {
        panic!("Supply 2 argumments");
    }
    let input_file_path = args.get(1).unwrap();
    let output_file_path = args.get(2).unwrap();
    let compilee = fs::read_to_string(input_file_path).unwrap();

    let parser_output = lexparse::ProgramParser::new().parse(&compilee);
    match parser_output {
        Ok(ast) => match Translator::new(ast) {
            Ok(mut pseudo_assembler) => match pseudo_assembler.translate() {
                Ok(_) => {
                    let ass = pseudo_assembler.final_translation();
                    fs::write(output_file_path, ass).expect("Unable to write to file");
                }
                Err(error) => write_message_and_exit(error, input_file_path),
            },
            Err(error) => write_message_and_exit(error, input_file_path),
        },
        Err(_) => {
            println!("Syntax Error")
        }
    };
}

fn write_message_and_exit(error: CodeError, input_file_path: &String) {
    let line_no = find_line_number(input_file_path, error.get_var_ptr() as usize).unwrap();
    match error {
        CodeError::UndeclaredVariable(mut id, _) => {
            id = id.split('@').next().unwrap().to_string();
            println!("ERROR: Undeclared variable `{id}` line: {line_no}");
        }
        CodeError::UndeclaredProcedure(id, _) => {
            println!("ERROR: Undeclared procedure `{id}` line: {line_no}");
        }
        CodeError::VariableUsedAsArray(mut id, _) => {
            id = id.split('@').next().unwrap().to_string();
            println!("ERROR: Undeclared variable `{id}` line: {line_no}");
        }
        CodeError::IndexOutOfBounds(mut id, _) => {
            id = id.split('@').next().unwrap().to_string();
            println!("ERROR: Index out of bounds for variable `{id}` line: {line_no}");
        }
        CodeError::ArrayUsedAsIndex(mut id, _) => {
            id = id.split('@').next().unwrap().to_string();
            println!("ERROR: Array used as index for variable `{id}` line: {line_no}");
        }
        CodeError::WrongArgumentType(mut id, _) => {
            id = id.split('@').next().unwrap().to_string();
            println!("ERROR: Wrong argument type for variable `{id}` line: {line_no}");
        }
        CodeError::DuplicateVariableDeclaration(mut id, _) => {
            id = id.split('@').next().unwrap().to_string();
            println!("ERROR: Duplicate variable declaration for variable `{id}` line: {line_no}");
        }
        CodeError::DuplicateProcedureDeclaration(id, _) => {
            println!("ERROR: Duplicate procedure declaration for procedure `{id}` line: {line_no}");
        }
        CodeError::RecursiveProcedureCall(id, _) => {
            println!("ERROR: Recursive procedure call for procedure `{id}` line: {line_no}");
        }
        CodeError::WrongNumberOfArguments(id, _) => {
            println!("ERROR: Wrong number of arguments for procedure `{id}` line: {line_no}");
        }
        CodeError::UninitializedVariable(id, _) => {
            println!("dupa")
        }
        CodeError::IteratorUsedAsVariable(id, _) => {
            println!("dupadupa")
        }
    }
    std::process::exit(1);
}

fn find_line_number(file_path: &str, n: usize) -> Option<usize> {
    let file = File::open(file_path).unwrap();
    let reader = BufReader::new(file);

    let mut total_bytes = 0;
    for (i, line) in reader.lines().enumerate() {
        let line = line.unwrap();
        total_bytes += line.len() + 1; // +1 for the '\n' character
        if total_bytes >= n {
            return Some(i + 1); // +1 because line numbers start from 1
        }
    }
    None // return None if n is greater than the total number of bytes
}
