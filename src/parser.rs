use crate::lexer::{self, Keyword, Lexer, Token};
use std::io::Read;

macro_rules! expect {
    ($self:expr, Token::$variant:ident) => {
        match $self.next_token()? {
            Some(Token::$variant) => Ok(Token::$variant),
            Some(token) => Err(ParserError::UnexpectedToken(token)),
            None => Err(ParserError::UnexpectedEof),
        }
    };
    ($self:expr, Token::$variant:ident(_)) => {
        match $self.next_token()? {
            Some(token @ Token::$variant(..)) => Ok(token),
            Some(token) => Err(ParserError::UnexpectedToken(token)),
            None => Err(ParserError::UnexpectedEof),
        }
    };
}

pub struct Parser<R: Read> {
    lexer: Lexer<R>,
}
impl<R: Read> Parser<R> {
    pub fn new(lexer: Lexer<R>) -> Self {
        Self {
            lexer
        }
    }

    pub fn next(&mut self) -> Result<Option<Declaration>, ParserError> {
        match self.lexer.next().map_err(ParserError::Lexer)? {
            Some(Token::Keyword(Keyword::Function)) => self.read_function_decl(),
            Some(tok) => Err(ParserError::UnexpectedToken(tok)),
            None => Ok(None),
        }
    }

    fn read_function_decl(&mut self) -> Result<Option<Declaration>, ParserError> {
        let name = match expect!(self, Token::Identifier(_))? {
            Token::Identifier(name) => name,
            _ => unreachable!(),
        };

        Ok(Some(Declaration {
            name,
            kind: DeclarationType::Function(self.read_function()?),
        }))
    }

    fn read_function(&mut self) -> Result<Function, ParserError> {
        expect!(self, Token::OpenParen)?;
        expect!(self, Token::CloseParen)?;

        let type_name = match self.next_token()? {
            Some(Token::Identifier(name)) => name,
            Some(token) => return Err(ParserError::UnexpectedToken(token)),
            None => return Err(ParserError::UnexpectedEof),
        };

        Ok(Function {
            type_name
        })
    }

    fn next_token(&mut self) -> Result<Option<Token>, ParserError> {
        self.lexer.next().map_err(ParserError::Lexer)
    }
}

#[derive(Debug)]
pub struct Declaration {
    name: String,
    kind: DeclarationType,
}

#[derive(Debug)]
pub enum DeclarationType {
    Function(Function)
}

#[derive(Debug)]
pub struct Function {
    type_name: String,
}

#[derive(Debug)]
pub enum ParserError {
    UnexpectedEof,
    UnexpectedToken(lexer::Token),
    Lexer(lexer::LexerError),
}
