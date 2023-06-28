/*
 * Copyright (c) Adrian Alic <contact@alic.dev>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::{hint::black_box, time::Instant};

const SOURCE: &str = include_str!("../example.ln");

fn main() {
    let start = Instant::now();
    let mut lexer = lnpl::lexer::Lexer::new(SOURCE);
    let mut token = lexer.next_token();
    while token.is_some() {
        token = lexer.next_token();
    }
    let end = start.elapsed();
    println!("Time elapsed: {:?}", end);
    black_box(token);
    black_box(lexer);
}
