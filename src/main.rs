#![allow(dead_code)]
#![allow(unused_imports)]

#[macro_use]
extern crate nom;

mod nom_parser;

#[allow(unused_imports)]
use chess5dlib::{game::*, moves::*, moveset::*, resolve::*, tree::*, parse::parse};
#[allow(unused_imports)]
use std::env;
#[allow(unused_imports)]
use std::fs::File;
#[allow(unused_imports)]
use std::io::prelude::*;
use std::path::Path;
extern crate json;
use std::fs::{self, DirEntry};




fn main() -> std::io::Result<()> {
    env_logger::builder()
        .format_timestamp(None)
        .init();
        reading_grammar();
    //A Rust attempt a parsing 5d chess annotation 
    Ok(())
}


// #[test]
fn reading_grammar() {
    use std::io::prelude::*;
     let mut itter = Path::new(file!()).ancestors();
     itter.next();
     let this_file = itter.next().unwrap().to_str().unwrap();
    let this_file_a = fs::canonicalize(&this_file).unwrap();
    let file_path = format!("{}/{}", this_file_a.to_str().unwrap(), "../5dchess-notation/fen.ebnf");
    let mut f = File::open(file_path).unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();


    use nom_parser::{nom_parse};

    let res = nom_parse(&s[..]);
    println!("- opencypher parse: {:?}", res);
    assert!(res.is_ok());
}

use std::io;
#[allow(dead_code)]
fn visit_dirs(dir: &Path, cb: &dyn Fn(&DirEntry)) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                continue;
                // visit_dirs(&path, cb)?;
            } else {
                cb(&entry);
            }
        }
    }
    Ok(())
}