#![feature(slice_patterns)]
#![feature(advanced_slice_patterns)]
use std::fs::File;
use std::path::Path;
use std::io::prelude::*;
use std::str::Chars;
use std::collections::VecDeque;
use std::error::Error;

pub mod defs;
pub mod parser;
pub mod repl;

#[macro_use]
pub use defs::*;
use parser::*;
use repl::eval;

// outputs csv file with the following spec:
// TAG,TYPE,COUNT1,COUNT2,COUNT3,...
// where TAG is the tag of the nation owning the army
// TYPE is either "ARMY" or "NAVY"
// and COUNT[N] is the number of units of that type in that army/navy
// in the order presented in the structs

impl<'a> Nation<'a>
{
    fn to_csv_str(&self) -> String
    {
        String::new()
    }
}

impl Mil
{
    fn to_csv_str(&self) -> String
    {
        match self
        {
            &Mil::A(ref a) => a.to_csv_string(),
            &Mil::N(ref n) => n.to_csv_string()
        }
    }
}

impl Army
{
    fn new(tag : &Tag) -> Army
    {
        Army
        {
            tag: tag.clone(),
            irr: 0, 
            cav: 0, 
            inf: 0, 
            hus: 0, 
            cui: 0, 
            dra: 0, 
            art: 0, 
            eng: 0, 
            gua: 0, 
            arm: 0, 
            pla: 0 
        }
    }
    fn to_csv_string(&self) -> String
    {
        format!("ARMY,{},{},{},{},{},{},{},{},{},{},{},{}",
                parser::to_string(&self.tag),
                self.irr,
                self.cav,
                self.inf,
                self.hus,
                self.cui,
                self.dra,
                self.art,
                self.eng,
                self.gua,
                self.arm,
                self.pla)
    }
}

impl Navy
{
    fn new(tag : &Tag) -> Navy
    {
        Navy
        {
            tag: tag.clone(),
            cli: 0,
            fri: 0,
            man: 0,
            com: 0,
            tra: 0,
            mon: 0,
            iro: 0,
            cru: 0,
            bat: 0,
            dre: 0
        }
    }

    fn to_csv_string(&self) -> String
    {
        format!("NAVY,{},{},{},{},{},{},{},{},{},{},{}",
                parser::to_string(&self.tag),
                self.cli,
                self.fri,
                self.man,
                self.com,
                self.tra,
                self.mon,
                self.iro,
                self.cru,
                self.bat,
                self.dre)
    }
}
