mod lexer;
mod parser;

use lexer::LexerError;
use parser::ParserError;
use std::io;

fn main() {
    let reader = io::BufReader::new(io::stdin());
    let lexer = lexer::Lexer::new(reader);
    if lexer.is_err() {
        eprintln!("failed to read: {:?}", lexer.unwrap_err());
        std::process::exit(1);
    }

    let mut parser = parser::Parser::new(lexer.unwrap());
    loop {
        match parser.next() {
            Ok(Some(decl)) => {
                println!("Parsed declaration: {:?}", decl);
            }
            Ok(None) => {
                println!("No more declarations.");
                break;
            }
            Err(err) => {
                match err {
                    ParserError::UnexpectedEof =>
                        eprintln!("parser: unexpected end of file"),
                    ParserError::UnexpectedToken(token) =>
                        eprintln!("parser: unexpected token: {:?}", token),
                    ParserError::Lexer(err) => match err {
                        LexerError::UnexpectedCharacter(ch) =>
                            eprintln!("lexer: unexpected character: '{}'", ch),
                        LexerError::Io(err) =>
                            eprintln!("lexer: i/o error: {}", err),
                    },
                }
                std::process::exit(1);
            }
        }
    }
}
