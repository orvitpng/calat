use std::io::{Bytes, Read};


macro_rules! consume {
    ($self:expr, $ret:expr) => {
        {
            $self.set_current()?;
            Ok(Some($ret))
        }
    }
}

#[derive(Debug)]
pub struct Lexer<R: Read> {
    input: Bytes<R>,
    current: Option<char>,
}
impl<R: Read> Lexer<R> {
    pub fn new(input: R) -> Result<Self, LexerError> {
        let mut lexer = Self {
            input: input.bytes(),
            current: None,
        };
        lexer.set_current()?;
        Ok(lexer)
    }

    pub fn next(&mut self) -> Result<Option<Token>, LexerError> {
        self.skip_whitespace()?;
        if self.current.is_none() {
            return Ok(None);
        }

        match self.current.unwrap() {
            ch if ch.is_ascii_alphabetic() => return self.read_identifier(),

            '(' => consume!(self, Token::OpenParen),
            ')' => consume!(self, Token::CloseParen),
            ';' => consume!(self, Token::Terminator),

            ch => return Err(LexerError::UnexpectedCharacter(ch)),
        }
    }

    fn read_identifier(&mut self) -> Result<Option<Token>, LexerError> {
        let mut ident = String::new();

        while let Some(ch) = self.current {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                ident.push(ch);
                self.set_current()?;
            } else {
                break;
            }
        }

        Ok(Some(match ident.as_str() {
            "fn" => Token::Keyword(Keyword::Function),
            _ => Token::Identifier(ident),
        }))
    }

    fn set_current(&mut self) -> Result<(), LexerError> {
        match self.input.next() {
            Some(Ok(byte)) => {
                self.current = Some(byte as char);
                Ok(())
            }
            None => {
                self.current = None;
                Ok(())
            }
            Some(Err(err)) => Err(LexerError::Io(err)),
        }
    }

    fn skip_whitespace(&mut self) -> Result<(), LexerError> {
        while let Some(ch) = self.current {
            match ch {
                ' ' | '\t' | '\r' | '\n' => {
                    self.set_current()?;
                }
                _ => break,
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum Token {
    Identifier(String),
    Keyword(Keyword),

    OpenParen,
    CloseParen,
    Terminator,
}

#[derive(Debug)]
pub enum Keyword {
    Function,
}

#[derive(Debug)]
pub enum LexerError {
    UnexpectedCharacter(char),
    Io(std::io::Error),
}
