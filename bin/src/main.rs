#[macro_use]
extern crate clap;

#[allow(unused_imports)]
use rox::*;
use clap::{Arg, App};
use std::fs;

fn main() {
    let args = App::new("rox")
        .version(crate_version!())
        .arg(Arg::with_name("file")
            .short("f")
            .long("file")
            .value_name("FILE")
            .help("Run from a file")
            .takes_value(true))
        .arg(Arg::with_name("code")
            .short("c")
            .long("code")
            .value_name("STRING")
            .help("Run code directly")
            .takes_value(true))
        .get_matches();

    let content = if let Some(filename) = args.value_of("file") {
        fs::read_to_string(filename).expect("")
    } else if let Some(code) = args.value_of("code") {
        code.to_string()
    } else {
        panic!("At least one input type (file/code) must be specified.");
    };

    let tokens = rox::lexer::lex(&content);
    // println!("{:?}", tokens);

    let ast = rox::parser::main(&tokens);
    // println!("{:?}", ast);

    use eval::Run;
    let mut scope = eval::Scope::new();
    for stmt in ast.expect("oopsie") {
        println!("## {}", stmt);
        stmt.run(&mut scope);
    }
}
