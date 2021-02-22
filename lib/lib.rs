#[macro_use]
extern crate lazy_static;
extern crate colored;
extern crate itertools;
extern crate rand;
extern crate scoped_threadpool;
extern crate serde;
extern crate serde_json;

pub mod prelude;
pub use prelude::*;

pub mod goals;
pub mod parse;
pub mod traversal;
pub mod random;
pub mod gen;
pub mod check;
pub mod mate;
pub mod tree;
