use crate::lexer::{Keyword, Lexer, Token};
use std::io::Read;

macro_rules! token_match {
    ($self:expr, { $( $pattern:pat => $body:expr ),+ $( , )? }) => {
        match $self.next_token() {
            $( Ok($pattern) => $body, )*
            Err(err) => Err(err),
        }
    };
    ($self:expr, Ok({ $( $pattern:pat => $body:expr ),+ $( , )? })) => {
        token_match!($self, {
            $( $pattern => Ok($body), )*
        })
    };
}

macro_rules! token_match_some {
    ($self:expr, { $( $pattern:pat => $body:expr ),+ $( , )? }) => {
        token_match!($self, {
            $( Some($pattern) => $body, )*
            None => Err(ParserError::UnexpectedEof)
        })
    };
    ($self:expr, Ok({ $( $pattern:pat => $body:expr ),+ $( , )? })) => {
        token_match!($self, {
            $( Some($pattern) => Ok($body), )*
            None => Err(ParserError::UnexpectedEof)
        })
    };
}

macro_rules! token_match_one {
    ($self:expr, $pattern:pat => $body:expr) => {
        token_match_some!($self, {
            $pattern => Ok($body),
            token => Err(ParserError::UnexpectedToken(token)),
        })
    };
}

macro_rules! token_expect {
    ($self:expr, $pattern:pat) => {
        token_match_one!($self, value @ $pattern => value)
    }
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

    pub fn next(&mut self) -> ParserResult<Option<Declaration>> {
        token_match!(self, {
            Some(Token::Keyword(Keyword::Function)) =>
                self.read_function_decl(),
            Some(token) => Err(ParserError::UnexpectedToken(token)),
            None => Ok(None),
        })
    }

    fn read_function_decl(&mut self) -> ParserResult<Option<Declaration>> {
        let name = token_match_one!(self, Token::Identifier(name) => name)?;

        Ok(Some(Declaration {
            name,
            kind: DeclarationType::Constant,
            value: Expression::Function(self.read_function()?),
        }))
    }

    fn read_function(&mut self) -> ParserResult<Function> {
        let parameters = self.read_parameters()?;
        token_expect!(self, Token::Colon)?;
        let type_name =
            token_match_one!(self, Token::Identifier(name) => name)?;

        Ok(Function {
            parameters,
            type_name,
        })
    }

    fn read_parameters(&mut self) -> ParserResult<Parameters> {
        token_expect!(self, Token::OpenParen)?;
        let mut parameters = Vec::new();

        if let Some(Token::CloseParen) =
            self.lexer.peek().map_err(ParserError::Lexer)?
        {
            self.next_token()?;
            return Ok(parameters);
        }

        loop {
            println!("1 {:?}", self.lexer.peek());
            let name = token_match_one!(self, Token::Identifier(name) => name)?;
            println!("{:?}", self.lexer.peek());
            token_expect!(self, Token::Colon)?;
            println!("{:?}", self.lexer.peek());
            let type_name = token_match_one!(self, Token::Identifier(name) => name)?;

            parameters.push(Parameter {
                name,
                type_name,
            });

            token_match_some!(self, {
                Token::Comma => {
                    if let Some(Token::CloseParen) =
                        self.lexer.peek().map_err(ParserError::Lexer)?
                    {
                        self.next_token()?;
                        break;
                    }
                    continue;
                },
                Token::CloseParen => break,
                token => Err(ParserError::UnexpectedToken(token)),
            })?;
        }

        Ok(parameters)
    }

    fn next_token(&mut self) -> ParserResult<Option<Token>> {
        loop {
            match self.read_token()? {
                Some(Token::Comment(_)) => continue,
                other => return Ok(other),
            }
        }
    }

    fn read_token(&mut self) -> Result<Option<Token>, ParserError> {
        self.lexer.next().map_err(ParserError::Lexer)
    }
}

#[derive(Debug)]
pub struct Declaration {
    name: String,
    kind: DeclarationType,
    value: Expression,
}

#[derive(Debug)]
pub enum DeclarationType {
    Constant,
    Variable,
    MutableVariable,
}

#[derive(Debug)]
pub enum Expression {
    Function(Function),
}

#[derive(Debug)]
pub struct Function {
    type_name: String,
    parameters: Parameters,
}

pub type Parameters = Vec<Parameter>;

#[derive(Debug)]
pub struct Parameter {
    name: String,
    type_name: String,
}

pub type ParserResult<T> = Result<T, ParserError>;

pub enum ParserError {
    UnexpectedEof,
    UnexpectedToken(Token),
    Lexer(crate::lexer::LexerError),
}
