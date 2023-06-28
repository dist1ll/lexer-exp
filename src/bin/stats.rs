use std::{
    collections::BTreeMap,
    fs::{read_dir, DirEntry},
};

use lnpl::lexer::common::TokenKind;

const FILE_EXT: &str = ".ln";

fn main() {
    let working_dir = std::env::current_dir().unwrap();
    let mut source_files = vec![];
    let mut dir_stack: Vec<DirEntry> = read_dir(working_dir.as_path())
        .unwrap()
        .map(Result::unwrap)
        .filter(|d| d.metadata().unwrap().is_dir())
        .collect();

    while let Some(d) = dir_stack.pop() {
        for f in std::fs::read_dir(d.path()).unwrap().map(Result::unwrap) {
            let metadata = f.metadata().unwrap();
            if metadata.is_dir() {
                dir_stack.push(f);
            } else if metadata.is_file()
                && f.path().to_str().unwrap().ends_with(FILE_EXT)
            {
                source_files.push(f);
            }
        }
    }

    eprintln!("{:?}", source_files);
    let result = source_files
        .iter()
        .map(|s| (s.file_name(), get_histograms(s)));

    for (file_name, r) in result {
        println!("[{}]", file_name.to_str().unwrap());
        for (k, v) in r {
            let sum: usize = v.values.into_iter().sum();
            println!(
                "\t{: <8?}: {:?}",
                k,
                v.values
                    .iter()
                    .map(|x| (*x as f32) / (sum as f32))
                    .collect::<Vec<_>>(),
            );
            println!("\t{: <8?}: {:?}\n\tSum: {}", k, v.values, sum);
        }
    }
}

#[derive(Default)]
struct Histogram {
    values: [usize; 8],
}

fn get_histograms(f: &DirEntry) -> BTreeMap<TokenKind, Histogram> {
    let mut res = BTreeMap::<TokenKind, Histogram>::new();
    // Add token kinds
    res.insert(TokenKind::Ident, Default::default());
    res.insert(TokenKind::Number, Default::default());
    res.insert(TokenKind::Whitespace, Default::default());

    let buf = std::fs::read_to_string(f.path()).unwrap();
    let mut lexer = lnpl::lexer::Lexer::new(buf.as_str());
    while let Some(t) = lexer.next_token() {
        use TokenKind::*;
        match t.kind {
            Class | For | Fn | Let | Match | Struct | Ln | While | Ident => {
                res.get_mut(&Ident).unwrap().values[lexer.slice().len()] += 1;
            }
            t @ (Whitespace | Number) => {
                res.get_mut(&t).unwrap().values[lexer.slice().len()] += 1;
            }
            _ => continue,
        }
    }
    println!("buffer length: {}", buf.len());
    res
}
