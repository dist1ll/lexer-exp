/*
 * Copyright (c) Adrian Alic <contact@alic.dev>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
mod aarch64;
pub mod common;
pub mod generic;

#[cfg(not(target_arch = "aarch64"))]
pub use generic::Lexer;

#[cfg(target_arch = "aarch64")]
pub use aarch64::Lexer;

#[cfg(test)]
mod test {
    use super::Lexer;
    use super::super::experimental::Token;
    const SOURCE: &'static str = include_str!("../../src/example.ln");

    #[test]
    fn asm() {
        println!("testing assembly");
        let mut y = Lexer::new(SOURCE);
        y.asm_exp();
    }
    #[test]
    fn parity() {
        let mut x = <Token as logos::Logos>::lexer(SOURCE);
        let mut y = Lexer::new(SOURCE);
        for i in 0..10000 {
            let xn = x.next().unwrap();
            if xn.is_err() {
                panic!("{:?}", xn);
            }
            let logos = xn.unwrap();
            let lnpl = y.next_token().unwrap();
            eprintln!(
                "Token #{i} logos: {: <8} lnpl: {: <8}",
                std::str::from_utf8(x.slice().as_bytes()).unwrap(),
                std::str::from_utf8(y.slice()).unwrap()
            );
            assert_eq!(logos, lnpl.kind.into(), "{i}");
        }
    }
}
