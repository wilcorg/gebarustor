mod ast;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub lexparse);

fn main() {
    println!("Hello, world!");
}
