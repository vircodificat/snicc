mod ast;
mod tac;
mod asm;
mod lexer;
mod compile;
mod assembler;
mod tac_vm;

use bstr::BString;
use lalrpop_util::lalrpop_mod;

use crate::ast::*;
use crate::tac::*;

use crate::{grammar::ProgramParser, lexer::Lexer};
lalrpop_mod!(grammar);

fn main() -> anyhow::Result<()> {
    let source_code = std::fs::read_to_string("main.toy")?;
    let lexer = Lexer::new(&source_code);
    let parser = ProgramParser::new();
    let ast = parser
        .parse(lexer)
        .map_err(|e| anyhow::anyhow!("Parse error {e:?}"))?;

//    eprintln!("{:#?}", ast);

    let mut f = std::io::stdout();
    let tac = compile::compile(&ast);
    tac.pprint(&mut f);

    let mut vm = tac_vm::TacVm::new(&tac);
    vm.run();

    return Ok(());

    eprintln!("{tac:#?}");

    let asm = assembler::assemble(&tac);
    eprintln!("{asm:#?}");


    Ok(())
}
