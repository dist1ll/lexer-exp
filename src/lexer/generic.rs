/*
 * Copyright (c) Adrian Alic <contact@alic.dev>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! This module contains a generic lexer implementation for platforms without
//! special support.

use std::{iter::Peekable, str::Chars};

use super::common::{keyword_hash, Token, TokenKind, KEYWORD_MAP, MAX_KW_LEN};

/// Generic lexer implementation. This is used as a fallback implementation
/// for platforms without a specialized SIMD implementation.
pub struct Lexer<'a> {
    pub chars: Peekable<Chars<'a>>,
    text: &'a [u8],
    pos: usize,
    peeked: Option<Token>,
    current: Option<Token>,
    current_len: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(s: &str) -> Lexer<'_> {
        Lexer {
            chars: s.chars().peekable(),
            text: s.as_bytes(),
            pos: 0,
            peeked: None,
            current: None,
            current_len: 0,
        }
    }
    #[inline]
    pub fn current_token(&mut self) -> Option<Token> {
        self.current.clone()
    }
    #[inline]
    pub fn slice(&mut self) -> &'a [u8] {
        &self.text[(self.pos - self.current_len as usize)..(self.pos)]
    }
    #[inline]
    pub fn next_token(&mut self) -> Option<Token> {
        let old_pos = self.pos;
        let kind = match self.advance()? {
            c if is_ident_prefix(c) => self.read_name(c),
            c if c.is_ascii_whitespace() => self.read_whitespace(),
            '0'..='9' => self.read_numeral(),
            ';' => TokenKind::Semicolon,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            '(' => TokenKind::ParensOpen,
            ')' => TokenKind::ParensClose,
            '{' => TokenKind::BraceOpen,
            '}' => TokenKind::BraceClose,
            '[' => TokenKind::BracketOpen,
            ']' => TokenKind::BracketClose,
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Star,
            '=' => TokenKind::Eq,
            '>' => TokenKind::Gt,
            ':' => TokenKind::Colon,
            _ => TokenKind::Ident,
        };
        self.current_len = self.pos as u32 - old_pos as u32;
        self.current = Some(Token { kind });
        self.current.clone()
    }
    #[inline]
    pub fn peek_token(&mut self) -> Option<&Token> {
        if self.peeked.is_some() {
            return self.peeked.as_ref();
        }
        self.peeked = self.next_token().clone();
        self.peeked.as_ref()
    }
    // use advance() instead of self.text.next()
    #[inline]
    fn advance(&mut self) -> Option<char> {
        self.pos += 1;
        self.chars.next()
    }
    #[inline]
    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }
    #[inline]
    fn read_numeral(&mut self) -> TokenKind {
        while let Some(c) = self.peek() {
            if !c.is_ascii_digit() {
                break;
            }
            self.advance();
        }
        TokenKind::Number
    }
    /// Returns either Ident or Keyword
    #[inline]
    fn read_name(&mut self, first_char: char) -> TokenKind {
        let mut cursor = 1;
        let mut kw_buf = [0u8; MAX_KW_LEN];
        let mut keyword_candidate = true;
        if first_char.is_ascii_lowercase() {
            kw_buf[0] = first_char as u8;
        } else {
            keyword_candidate = false;
        }
        while let Some(c) = self.chars.peek() {
            // requirement for keyword
            if keyword_candidate
                && cursor < MAX_KW_LEN
                && c.is_ascii_lowercase()
            {
                kw_buf[cursor] = *c as u8;
                cursor += 1;
            } else if c.is_ascii_alphanumeric() {
                keyword_candidate = false;
            } else {
                break;
            }
            self.advance();
        }

        if keyword_candidate {
            let (hash, keyword) = keyword_hash(&kw_buf);
            if kw_buf == KEYWORD_MAP[hash] {
                return keyword.expect("hash fn and keyword map don't match");
            }
        }
        TokenKind::Ident
    }
    #[inline]
    fn read_whitespace(&mut self) -> TokenKind {
        while let Some(c) = self.chars.peek() {
            if !c.is_ascii_whitespace() {
                break;
            }
            self.advance();
        }
        TokenKind::Whitespace
    }
}

// Returns true if the given character can be the start of an identifier.
#[inline]
pub fn is_ident_prefix(c: char) -> bool {
    c.is_ascii_alphabetic()
}
