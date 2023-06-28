/*
 * Copyright (c) Adrian Alic <contact@alic.dev>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
mod aarch64;
pub mod generic;
pub mod common;

#[cfg(not(target_arch = "aarch64"))]
pub use generic::Lexer;

#[cfg(target_arch = "aarch64")]
pub use aarch64::Lexer;

#[cfg(test)]
mod test {
    use super::*;
    use super::common::TokenKind;

    #[test]
    pub fn peek() {
        let mut t = Lexer::new("+ +");
        // peek, peek, next
        assert_eq!(t.peek_token().unwrap().kind, TokenKind::Plus);
        assert_eq!(t.peek_token().unwrap().kind, TokenKind::Plus);
        assert_eq!(t.next_token().unwrap().kind, TokenKind::Plus);

        // peek, peek, next
        assert_eq!(t.peek_token().unwrap().kind, TokenKind::Whitespace);
        assert_eq!(t.peek_token().unwrap().kind, TokenKind::Whitespace);
        assert_eq!(t.next_token().unwrap().kind, TokenKind::Whitespace);
    }
}
