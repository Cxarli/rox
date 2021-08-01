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

    let content = {
        if let Some(filename) = args.value_of("file") {
            fs::read_to_string(filename).expect("")
        } else if let Some(code) = args.value_of("code") {
            code.to_string()
        } else {
            panic!("At least one input type (file/code) must be specified.");
        }
    };

    let tokens = rox::lexer::lex(&content);

    /*
    for token in &tokens {
        println!("{}", token);
    }
    */

    let ast = rox::parser::parse(&tokens);
    // println!("{:#?}", ast);

    if let Ok(ast) = ast {
        use eval::{Run, Value};
        let mut scope = eval::Scope::new();

        for decl in ast {
            if scope.get("__verbose").into() {
                println!("#! {}", decl);
            }
            decl.run(&mut scope);
        }
    } else {
        for err in ast.unwrap_err() {
            println!("oops: {:?}", err);
        }
    }
}
