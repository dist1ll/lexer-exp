/*
 * Copyright (c) Adrian Alic <contact@alic.dev>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

#![feature(test)]

use std::hint::black_box;

use lnpl::lexer::Lexer;
use logos::Logos;
use test::Bencher;

extern crate test;

#[repr(align(4096))]
struct SourceWrapper(&'static str);
const SOURCE: SourceWrapper = SourceWrapper(include_str!("../src/example.ln"));

#[bench]
fn lnpl_version(b: &mut Bencher) {
    // assert_impl_parity();

    b.iter(|| {
        let mut x = Lexer::new(SOURCE.0);
        while let Some(_) = x.next_token() { }
        // while let Some(t) = x.next_token() {
        //     println!("{i} {:?}", t.kind);
        //     i += 1;
        //     if i > 57 {
        //         break;
        //     }
        // }
        black_box(x);
    })
}
fn assert_impl_parity() {
    let mut x = lnpl::experimental::Token::lexer(SOURCE.0);
    let mut y = Lexer::new(SOURCE.0);
    for i in 0..20000 {
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
#[bench]
fn logos_version(b: &mut Bencher) {
    use lnpl::experimental;
    use logos::Logos;
    b.iter(|| {
        let mut x = experimental::Token::lexer(SOURCE.0);
        #[allow(unused)]
        #[inline(never)]
        fn non_inlined_next(
            l: &mut logos::Lexer<experimental::Token>,
        ) -> Option<Result<experimental::Token, ()>> {
            l.next()
        }
        // while let Some(_) = non_inlined_next(&mut x) {}
        while let Some(_) = x.next() { }
        black_box(x);
    })
}
