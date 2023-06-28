use logos::Logos;

use crate::lexer::common::TokenKind;

#[derive(Logos, Debug, PartialEq)]
#[logos()]
pub enum Token {
    #[token("class")]
    Class,
    #[token("for")]
    For,
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("match")]
    Match,
    #[token("struct")]
    Struct,
    #[token("ln")]
    Ln,
    #[token("while")]
    While,
    #[regex("[A-Za-z]+[a-zA-Z0-9_]*")]
    Ident,
    #[regex("[0-9]+")]
    Number,
    #[regex(r"[ \t\n\f]+")]
    Whitespace,
    #[token("=")]
    Eq,
    #[token(">")]
    Gt,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token(";")]
    Semicolon,
    // ()
    #[token("(")]
    ParensOpen,
    #[token(")")]
    ParensClose,
    /// [
    #[token("[")]
    BracketOpen,
    /// ]
    #[token("]")]
    BracketClose,
    /// {
    #[token("{")]
    BraceOpen,
    /// }
    #[token("}")]
    BraceClose,
}

impl From<TokenKind> for Token {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Class => Token::Class,
            TokenKind::For => Token::For,
            TokenKind::Fn => Token::Fn,
            TokenKind::Let => Token::Let,
            TokenKind::Match => Token::Match,
            TokenKind::Struct => Token::Struct,
            TokenKind::Ln => Token::Ln,
            TokenKind::While => Token::While,
            TokenKind::Ident => Token::Ident,
            TokenKind::Number => Token::Number,
            TokenKind::Whitespace => Token::Whitespace,
            TokenKind::Eq => Token::Eq,
            TokenKind::Gt => Token::Gt,
            TokenKind::Plus => Token::Plus,
            TokenKind::Minus => Token::Minus,
            TokenKind::Star => Token::Star,
            TokenKind::Colon => Token::Colon,
            TokenKind::Comma => Token::Comma,
            TokenKind::Dot => Token::Dot,
            TokenKind::Semicolon => Token::Semicolon,
            TokenKind::ParensOpen => Token::ParensOpen,
            TokenKind::ParensClose => Token::ParensClose,
            TokenKind::BracketOpen => Token::BracketOpen,
            TokenKind::BracketClose => Token::BracketClose,
            TokenKind::BraceOpen => Token::BraceOpen,
            TokenKind::BraceClose => Token::BraceClose,
        }
    }
}