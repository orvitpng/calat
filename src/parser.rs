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
            None => Err(ParserError::UnexpectedEof),
        })
    };
    ($self:expr, Unexpected{ $( $pattern:pat => $body:expr ),+ $( , )? }) => {
        token_match_some!($self, {
            $( $pattern => $body, )*
            token => Err(ParserError::UnexpectedToken(token)),
        })
    };
    ($self:expr, Ok{ $( $pattern:pat => $body:expr ),+ $( , )? }) => {
        token_match_some!($self, Unexpected{
            $( $pattern => Ok($body), )*
        })
    };
}

macro_rules! token_match_one {
    ($self:expr, $pattern:pat => $body:expr) => {
        token_match_some!($self, Ok{
            $pattern => $body,
        })
    };
}

macro_rules! peek_if {
    ($self:expr, $pattern:pat => $body:expr) => {
        if let Some($pattern) = $self.peek_token()? {
            $body
        }
    };
    ($self:expr, $pattern:pat => $body:expr, consume) => {
        peek_if!($self, $pattern => {
            $self.next_token()?;
            $body
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
            value: self.read_function()?,
        }))
    }

    fn read_function(&mut self) -> ParserResult<Expression> {
        let parameters = self.read_parameters()?;
        token_expect!(self, Token::Colon)?;
        let type_name =
            token_match_one!(self, Token::Identifier(name) => name)?;
        token_expect!(self, Token::Equals)?;

        Ok(Expression::Function {
            parameters,
            type_name,
            value: Box::new(self.read_expression()?),
        })
    }

    fn read_parameters(&mut self) -> ParserResult<Vec<Parameter>> {
        token_expect!(self, Token::OpenParen)?;
        let mut parameters = Vec::new();

        peek_if!(self, Token::CloseParen => return Ok(parameters), consume);
        loop {
            let name = token_match_one!(self, Token::Identifier(name) => name)?;
            token_expect!(self, Token::Colon)?;
            let type_name = token_match_one!(self, Token::Identifier(name) => name)?;

            parameters.push(Parameter {
                name,
                type_name,
            });

            token_match_some!(self, Unexpected{
                Token::Comma => {
                    peek_if!(self, Token::CloseParen => break, consume);
                    continue;
                },
                Token::CloseParen => break,
            })?;
        }

        Ok(parameters)
    }

    fn read_expression(&mut self) -> ParserResult<Expression> {
        token_match_some!(self, Ok{
            Token::OpenBrace => self.read_block()?,
            Token::Identifier(name) => self.read_or_call(name)?,
        })
    }

    fn read_block(&mut self) -> ParserResult<Expression> {
        let mut statements = Vec::new();

        loop {
            peek_if!(self, Token::CloseBrace => break, consume);

            peek_if!(self, Token::Keyword(Keyword::Return) => {
                let expr = if let Some(Token::Semicolon) = self.peek_token()? {
                    None
                } else {
                    Some(self.read_expression()?)
                };
                token_expect!(self, Token::Semicolon)?;

                statements.push(Statement::Return(expr));
                continue;
            }, consume);

            let expr = self.read_expression()?;
            token_match_some!(self, Ok{
                Token::Semicolon =>
                    statements.push(Statement::Expression(expr)),
                Token::CloseBrace =>
                    statements.push(Statement::Return(Some(expr))),
            })?;
        }

        Ok(Expression::Block(statements))
    }

    fn read_or_call(&mut self, name: String) -> ParserResult<Expression> {
        if let Some(Token::OpenParen) = self.peek_token()? {
            self.next_token()?;
            Ok(Expression::Call {
                name,
                arguments: self.read_arguments()?,
            })
        } else {
            Ok(Expression::Identifier(name))
        }
    }

    fn read_arguments(&mut self) -> ParserResult<Vec<Expression>> {
        let mut arguments = Vec::new();

        if let Some(Token::CloseParen) = self.peek_token()? {
            return Ok(arguments);
        }

        loop {
            arguments.push(self.read_expression()?);

            token_match_some!(self, Unexpected{
                Token::Comma => {
                    if let Some(Token::CloseParen) = self.peek_token()? {
                        self.read_token()?;
                        break;
                    }
                    continue;
                },
                Token::CloseParen => break,
            })?;
        }

        Ok(arguments)
    }

    fn next_token(&mut self) -> ParserResult<Option<Token>> {
        loop {
            match self.read_token()? {
                Some(Token::Comment(_)) => continue,
                other => return Ok(other),
            }
        }
    }

    fn read_token(&mut self) -> ParserResult<Option<Token>> {
        self.lexer.next().map_err(ParserError::Lexer)
    }

    fn peek_token(&mut self) -> ParserResult<Option<&Token>> {
        self.lexer.peek().map_err(ParserError::Lexer)
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
}

#[derive(Debug)]
pub enum Expression {
    Function {
        type_name: String,
        parameters: Vec<Parameter>,
        value: Box<Expression>,
    },
    Call {
        name: String,
        arguments: Vec<Expression>,
    },
    Block(Vec<Statement>),
    Identifier(String),
}

#[derive(Debug)]
pub struct Parameter {
    name: String,
    type_name: String,
}

#[derive(Debug)]
pub enum Statement {
    Return(Option<Expression>),
    Expression(Expression),
}

pub type ParserResult<T> = Result<T, ParserError>;

pub enum ParserError {
    UnexpectedEof,
    UnexpectedToken(Token),
    Lexer(crate::lexer::LexerError),
}
