use std::io::{Bytes, Read};

macro_rules! consume {
    ($self:expr, $ret:expr) => {
        {
            $self.set_current()?;
            Ok(Some($ret))
        }
    };
}

#[derive(Debug)]
pub struct Lexer<R: Read> {
    input: Bytes<R>,
    current: Option<char>,
    peeked: Option<Token>,
}
impl<R: Read> Lexer<R> {
    pub fn new(input: R) -> LexerResult<Self> {
        let mut lexer = Self {
            input: input.bytes(),
            current: None,
            peeked: None,
        };
        lexer.set_current()?;
        Ok(lexer)
    }

    pub fn next(&mut self) -> LexerResult<Option<Token>> {
        if let Some(token) = self.peeked.take() {
            return Ok(Some(token));
        }
        self.next_token()
    }

    pub fn peek(&mut self) -> LexerResult<Option<&Token>> {
        if self.peeked.is_none() {
            self.peeked = self.next_token()?;
        }
        Ok(self.peeked.as_ref())
    }

    pub fn next_token(&mut self) -> LexerResult<Option<Token>> {
        self.skip_whitespace()?;
        if self.current.is_none() {
            return Ok(None);
        }

        match self.current.unwrap() {
            ch if ch.is_ascii_alphabetic() => return self.read_identifier(),

            '/' => {
                self.set_current()?;
                return if self.current == Some('/') {
                    consume!(self, self.read_comment()?)
                } else {
                    Err(LexerError::UnexpectedCharacter('/'))
                }
            },

            '(' => consume!(self, Token::OpenParen),
            ')' => consume!(self, Token::CloseParen),
            ',' => consume!(self, Token::Comma),
            ':' => consume!(self, Token::Colon),
            ';' => consume!(self, Token::Semicolon),

            ch => return Err(LexerError::UnexpectedCharacter(ch)),
        }
    }

    fn read_identifier(&mut self) -> LexerResult<Option<Token>> {
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

    fn read_comment(&mut self) -> LexerResult<Token> {
        let mut comment = String::new();

        while let Some(ch) = self.current {
            if ch == '\n' {
                self.set_current()?;
                break;
            } else {
                comment.push(ch);
                self.set_current()?;
            }
        }

        Ok(Token::Comment(comment))
    }

    fn set_current(&mut self) -> LexerResult<()> {
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

    fn skip_whitespace(&mut self) -> LexerResult<()> {
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
    Comment(String),
    Identifier(String),
    Keyword(Keyword),

    OpenParen,
    CloseParen,
    Comma,
    Colon,
    Semicolon,
}

#[derive(Debug)]
pub enum Keyword {
    Function,
}

pub type LexerResult<T> = Result<T, LexerError>;

#[derive(Debug)]
pub enum LexerError {
    UnexpectedCharacter(char),
    Io(std::io::Error),
}
