
#![allow(dead_code)]
#![allow(unused_imports)]
use std::fmt::Debug;
use nom::{named, do_parse, is_not};
use nom::{ Err, Needed};
// use nom::ErrorKind;
// use nom::{Done, Error, Incomplete};
use std::str;
use std::io;
use std::str::from_utf8;
// use nom::Err::{Code, Node, Position, NodePosition};

// TODO doesn't support exception -

#[derive(Debug, PartialEq, Clone)]
pub struct Parser {
    items: Vec<Item>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Rule {
    name: String,
    production: Production
}

#[derive(Debug, PartialEq, Clone)]
pub enum Item {
    Comment(String),
    Rule(Rule)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Production {
    Terminal(String),
    Identifier(String),
    Optional(Box<Production>),
    Repetition(Box<Production>),
    Repetition1(Box<Production>),
    Group(Box<Production>),
    Alternation(Vec<Production>),
    Concatenation(Vec<Production>),
    Regex(String),
    
    Exception(Box<Production>, Box<Production>),
}

#[allow(dead_code)]
pub fn nom_parse(input: &str) -> Result<Parser, String> {
    // let test = format!("{}",&input);

    let result = grammar(&input.as_bytes()[..]);
    println!("{:?}",result );
    let p : Parser = Parser {items: vec![]};
    Ok(p)
    //handle_parse(grammar(&input.as_bytes()[..]))
}

use nom::{
    IResult,
    bytes::complete::{tag, take_while_m_n},
    combinator::map_res,
    combinator::opt,
    sequence::tuple,
    character::complete::multispace0 as multispace,
    character::complete::alpha0 as alpha,
    character::complete::alphanumeric0 as alphanumeric,
    Err::{Incomplete, Error, Failure}
};


fn handle_parse<T: Debug>(res: IResult<&[u8], T>) -> Result<T, String> {
    match res {
        Ok((x, tree)) => {
            // println!("--- done: {:?}", from_utf8(x));
            if x.len() > 0 {
                Err(format!("Incomplete, didn't consume the following: `{:?}`. Parsed: {:?}",
                            from_utf8(x).unwrap(),
                            tree))
            } else {
                Ok(tree)
            }
        }
        // TODO need to improve errors a ton
        Err(e) => match e {
            Error(x) => Err(get_error_msg(x)),
            Incomplete(n) => Err(format!("Incomplete: {:?}", n)),
            Failure(e) => Err(format!("Incomplete: {:?}", e))
        }
    }
}

fn get_error_msg(res: nom::error::Error<&[u8]>) -> String {
    format!("We got a error of '{}' at {}", 
        res.code.description(),
        str::from_utf8(res.input).unwrap()
    )
}


named!(double_not_escaped_seq<&[u8], &[u8]>, is_not!("\\\""));
named!(double_escaped_seq,
       alt!(tag!("\\r") | tag!("\\n") | tag!("\\t") | tag!("\\\"") | tag!("\\\\")));

named!(single_not_escaped_seq<&[u8], &[u8]>, is_not!(&b"\\'"[..]));
named!(single_escaped_seq,
       alt!(tag!("\\r") | tag!("\\n") | tag!("\\t") | tag!("\\'") | tag!("\\\\")));


named!(single_quote_literal<&[u8], String>,
    do_parse!(tag!("'") >>
           s: many0!(map_res!(alt!(single_escaped_seq | single_not_escaped_seq),
                              from_utf8)) >>
           tag!("'") >>
           (
               s.into_iter().fold(String::new(), |mut accumulator, slice| {
                   accumulator.push_str(slice);
                   accumulator
               })
)));

named!(double_quote_literal<&[u8], String>,
    do_parse!(tag!("\"") >>
           s: many0!(map_res!(alt!(double_escaped_seq | double_not_escaped_seq),
                              from_utf8)) >>
           tag!("\"") >>
           (
               s.into_iter().fold(String::new(), |mut accum, slice| {
                   accum.push_str(slice);
                   accum
               })
           )));


named!(terminal<&[u8], String>,
    do_parse!(
        t: alt!(single_quote_literal | double_quote_literal) >>
           (
               //println!("Terminal: {:?}", t);
               t
)));

named!(rule<&[u8], Item>, 
  do_parse!( 
      opt!(multispace) >>
      line: map_res!(take_until1!(";"), from_utf8) >>
      alt!(tag!(";") | tag!(".")) >>
        (Item::Rule(
            Rule {
                name : line.to_string(),
                production : Production::Terminal("hi".to_string())
            }
                
        ))   
    )
);

named!(identifier<&[u8], String>,
    do_parse!(
        first: alpha >>
        rest: many0!(alt!(alphanumeric | tag!("_"))) >>
        ( || {
               let s = rest.into_iter().fold(first.to_vec(), |mut l, x| {
                   l.extend_from_slice(x);
                   l
               });
               println!("Identifier: {:?}", from_utf8(s.as_slice()));
               from_utf8(s.as_slice()).unwrap().to_owned()
            }
        )
    )
);

named!(comment<&[u8], Item>, 
    do_parse!(   
      opt!(multispace) >>
      tag!("(*") >>
      comment: map_res!(take_until!("*)"), from_utf8) >>
      tag!("*)") >>
      opt!(multispace) >>
    (Item::Comment(comment.to_string()))
    )
);
    

named!(grammar<&[u8],Parser>,
  do_parse!(   
      opt!(multispace) >>
      items: many1!(alt!(rule | comment))  >> 
      opt!(multispace) >>
      (Parser {
        items: items
    })
    )
);

#[test]
fn comment_works() { 
    let res = handle_parse(comment("            (* hi *)".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());
}

#[test]
fn identifier_works() {
    let res = handle_parse(identifier("Abasekr".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());

    let res = handle_parse(identifier("a".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());

    let res = handle_parse(identifier("A".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());
}

#[test]
fn terminal_works() {
    let res = handle_parse(single_quote_literal("'A'".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());

    let res = handle_parse(double_quote_literal("\"A\"".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());

    let res = handle_parse(terminal("\"A\"".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());

    let res = handle_parse(terminal("\";\"".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());
}

#[test]
fn rhs_works() {
    let res = handle_parse(rhs("\"A\" | \"B\" | \"C\" | \"D\" | \"E\" | \"F\" | \"G\"".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());

    let res = handle_parse(rhs("letter | digit | symbol | \"_\"".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());
}

#[test]
fn rule_works() {
    let res = handle_parse(rule("character = letter | digit | symbol | \"_\" ;".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());
    assert_eq!(res.unwrap(), Item::Rule(Rule{
        name: "character".to_string(),
        production: Production::Alternation(vec![
            Production::Identifier("letter".to_string()),
            Production::Identifier("digit".to_string()),
            Production::Identifier("symbol".to_string()),
            Production::Terminal("_".to_string()),
        ])
    }));

    let res = handle_parse(rule("identifier = letter , { letter | digit | \"_\" } ;".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());
    assert_eq!(res.unwrap(), Item::Rule(Rule{
        name: "identifier".to_string(),
        production: Production::Concatenation(vec![
            Production::Identifier("letter".to_string()),
            Production::Repetition(Box::new(Production::Alternation(vec![
                Production::Identifier("letter".to_string()),
                Production::Identifier("digit".to_string()),
                Production::Terminal("_".to_string()),
            ]))),
        ])
    }));

    let res = handle_parse(rule("Cypher = WS AllOptions WS Statements ;".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());
    assert_eq!(res.unwrap(), Item::Rule(Rule{
        name: "Cypher".to_string(),
        production: Production::Concatenation(vec![
            Production::Identifier("WS".to_string()),
            Production::Identifier("AllOptions".to_string()),
            Production::Identifier("WS".to_string()),
            Production::Identifier("Statements".to_string()),
        ])
    }));

    let res = handle_parse(rule("StringLiteral = ('\", { ANY - ('\"' | '\') | EscapedChar }, '\"')
  | (\"'\", { ANY - (\"'\" | '\') | EscapedChar }, \"'\")
  ;".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());
    assert_eq!(res.unwrap(), Item::Rule(Rule{
        name: "Cypher".to_string(),
        production: Production::Concatenation(vec![
            Production::Identifier("WS".to_string()),
            Production::Identifier("AllOptions".to_string()),
            Production::Identifier("WS".to_string()),
            Production::Identifier("Statements".to_string()),
        ])
    }));
}

#[test]
fn it_works() {
    // Test parsing it's own grammar
    let res = handle_parse(grammar("letter = \"A\" | \"B\" | \"C\" | \"D\" | \"E\" | \"F\" | \"G\"
       | \"H\" | \"I\" | \"J\" | \"K\" | \"L\" | \"M\" | \"N\"
       | \"O\" | \"P\" | \"Q\" | \"R\" | \"S\" | \"T\" | \"U\"
       | \"V\" | \"W\" | \"X\" | \"Y\" | \"Z\" | \"a\" | \"b\"
       | \"c\" | \"d\" | \"e\" | \"f\" | \"g\" | \"h\" | \"i\"
       | \"j\" | \"k\" | \"l\" | \"m\" | \"n\" | \"o\" | \"p\"
       | \"q\" | \"r\" | \"s\" | \"t\" | \"u\" | \"v\" | \"w\"
       | \"x\" | \"y\" | \"z\" ;
digit = \"0\" | \"1\" | \"2\" | \"3\" | \"4\" | \"5\" | \"6\" | \"7\" | \"8\" | \"9\" ;
symbol = \"[\" | \"]\" | \"{\" | \"}\" | \"(\" | \")\" | \"<\" | \">\"
       | \"'\" | '\"' | \"=\" | \"|\" | \".\" | \",\" | \";\" ;
character = letter | digit | symbol | \"_\" ;

identifier = letter , { letter | digit | \"_\" } ;
terminal = \"'\" , character , { character } , \"'\" 
| '\"' , character , { character } , '\"' ;

lhs = identifier ;
rhs = identifier
| terminal
| \"[\" , rhs , \"]\"
| \"{\" , rhs , \"}\"
| \"(\" , rhs , \")\"
| rhs , \"|\" , rhs
| rhs , \",\" , rhs ;

rule = lhs , \"=\" , rhs , \";\" ;
grammar = { rule } ;".as_bytes()));
    println!("- res: {:?}", res);
    assert!(res.is_ok());
}
