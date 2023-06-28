/*
 * Copyright (c) Adrian Alic <contact@alic.dev>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::collections::BTreeSet;

fn main() {
    println!("Collision-free hash function:\n");

    let kw_raw = [
        "class",  //
        "for",    //
        "fn",     //
        "let",    //
        "match",  //
        "struct", //
        "ln",     //
        "while",  //
    ];
    let keywords: Vec<[u8; 8]> = kw_raw
        .iter()
        .map(|kw| {
            let mut res = [0u8; 8];
            for (idx, b) in kw.as_bytes().iter().enumerate() {
                res[idx] = *b;
            }
            res
        })
        .collect();

    let mut map = BTreeSet::<u8>::new();
    for i in 1..1000 {
        let mut indices = Vec::with_capacity(16);
        for b in keywords.iter() {
            let mut hash = 0usize;
            hash += b[0] as usize + i;
            hash += (b[2] as usize) * i;
            hash &= 0b1111;
            indices.push(hash as u8);
        }
        // check collision free-ness
        map.clear();
        let mut collision_free = true;
        for idx in indices.iter() {
            if !map.insert(*idx) {
                collision_free = false;
                break;
            }
        }
        if collision_free {
            println!("#[rustfmt::skip]\nconst KEYWORD_MAP: [[u8; 8]; 16] = [");
            let mut v: Vec<_> =
                indices.iter().zip(kw_raw.into_iter()).collect();
            v.sort_unstable_by(|a, b| a.0.cmp(b.0));
            let mut k = 0;
            (0..16).for_each(|i| {
                if i == *v[k].0 {
                    println!("    conv(\"{}\"),", v[k].1);
                    k += 1;
                } else {
                    // empty line
                    println!("    [0, 0, 0, 0, 0, 0, 0, 0],");
                }
            });
            println!("];\nconst KW_MAGIC_NUMBER: usize = {};\n", i);
            println!("    let keyword = match hash {{");
            v.into_iter().for_each(|(&idx, kw)| {
                println!("        {} => Keyword::{},", idx, title_case(kw))
            });
            println!("        _ => return (hash, None),\n    }};\n");
            break;
        }
    }
}

fn title_case(s: &str) -> String {
    let mut upper_case = String::from(s);
    upper_case.get_mut(0..1).unwrap().make_ascii_uppercase();
    upper_case
}
