use std::str::Chars;
use std::path::Path;
use std::fs::File;
use std::io::Write;

use defs::*;

/// Evaluates the query, then formats and returns the results (if any)
pub fn eval(query : String) -> String
{
    query
}

pub fn make_csv(save : &Save) -> String
{
    let mut s = String::from("type,tag,count1,count2,count3,count4,count5,count6,count7,count8,count9,count10,count11");

    for n in save.iter()
    {
        s.push_str("\n");
        s.push_str(&n.to_csv_str());
    }

    s
}

fn save_csv(save : &Save, dest : &str) -> String
{
    let csv : String = make_csv(save);
    let mut out : File = File::create(dest)
        .expect("Could not create file to output");

    match out.write(csv.as_bytes())
    {
        Ok(_) => (),
        Err(_) => return "Error writing to file.".into()
    }

    "File outputted!".into()
    
}
