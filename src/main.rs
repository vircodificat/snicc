mod asm;
//mod assembler;
mod ast;
mod compile;
mod lexer;
mod tac;
mod tac_vm;

use bstr::BString;
use clap::Parser;
use lalrpop_util::lalrpop_mod;

use crate::ast::*;
//use crate::tac::*;

use crate::{grammar::ProgramParser, lexer::Lexer};
lalrpop_mod!(grammar);

#[derive(Parser)]
struct Cli {
    filepath: Option<String>,
}

fn main() -> anyhow::Result<()> {
    let args = Cli::parse();
    let filepath = args.filepath.unwrap_or_else(|| "main.toy".to_string());

    let source_code = std::fs::read_to_string(&filepath)?;
    println!("{source_code}");
    println!();

    let lexer = Lexer::new(&source_code);
    let parser = ProgramParser::new();
    let ast = parser.parse(lexer).map_err(|e| {
        match &e {
            lalrpop_util::ParseError::InvalidToken { location } => todo!(),
            lalrpop_util::ParseError::UnrecognizedEof { location, expected } => todo!(),
            lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                let (ll, token, rr) = token;
                let (line, col) = pos_to_linecol(*ll, &source_code);
                eprintln!("ERROR AT {line}:{col}");
            }
            lalrpop_util::ParseError::ExtraToken { token } => todo!(),
            lalrpop_util::ParseError::User { error } => todo!(),
        }
        anyhow::anyhow!("Parse error {e:?}")
    })?;

    //eprintln!("{:#?}", ast);

    let mut f = std::io::stdout();
    let tac = compile::compile(&ast);
    tac.pprint(&mut f);
    println!();

    let mut vm = tac_vm::TacVm::new(&tac);
    vm.run();

    return Ok(());

    //    eprintln!("{tac:#?}");
    //
    //    let asm = assembler::assemble(&tac);
    //    eprintln!("{asm:#?}");
    //
    //    Ok(())
}

fn pos_to_linecol(pos: usize, text: &str) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;

    for (i, ch) in text.chars().enumerate() {
        if i == pos {
            return (line, col);
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    return (usize::MAX, usize::MAX);
}
