extern crate efs;

use std::path::Path;
use std::fs::File;
use std::io::Write;

fn main()
{
    let args : Vec<String> = std::env::args().collect();

    if args.len() < 3
    {
        println!("Please input the name of the save and then the destination file");
        return ();
    }

    let save : &Path = Path::new(&args[1]);
    let output : &Path = Path::new(&args[2]);

    println!("Read command lines");

    let data : Vec<efs::Mil> = match efs::parse(&save)
    {
        Ok(v) => v,
        Err(e) =>
        {
            println!("Parse error in file: {}", e.desc);
            return ();
        }
    };

    println!("Parsed {}...", args[0]);

    let csv : String = efs::make_csv(&data[..]);
     
    println!("Made csv...");

    let mut out : File = File::create(output)
        .expect("Could not create file to output");

    match out.write(csv.as_bytes())
    {
        Ok(_) => (),
        Err(_) => println!("Error writing to file.")
    }

    println!("Wrote csv output to: {}", args[2]);
}
