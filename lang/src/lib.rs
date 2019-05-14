pub mod ast;
pub mod walker;
pub use ast::*;
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(c);

pub fn parse(input: &str) -> Result<Program, String> {
    match c::ProgramParser::new().parse(input) {
        Ok(ast) => Ok(ast),
        Err(err) => Err(format!("{}", err)),
    }
}
