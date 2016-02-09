use std::str::Chars;
use std::path::Path;
use std::fs::File;
use std::io::Write;

use defs::*;

/// Evaluates the query, then formats and returns the results (if any)
pub fn eval(query: String) -> String {
    query
}

pub fn make_csv(save: &Save) -> String {
    String::new()
}

fn save_csv(save: &Save, dest: &str) -> String {
    let csv: String = make_csv(save);
    let mut out: File = File::create(dest).expect("Could not create file to output");

    match out.write(csv.as_bytes()) {
        Ok(_) => (),
        Err(_) => return "Error writing to file.".into(),
    }

    "File outputted!".into()

}
