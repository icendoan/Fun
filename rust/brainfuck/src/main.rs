use std::io::prelude::*; // contains file IO
use std::fs::File; // for file IO
use std::path::Path; // for file IO
use std::error::Error;
use std::env; // contains program environment

fn main() -> ()
{
    // load the source file
    let args : Vec<String> = env::args().collect(); // collect the program arguments

    let mut debug = false;
    if args.len() < 2 // didn't get source file - first argument is always the program path
    {
        println!("Didn't get source file in argument!");
        return ();
    }

    if args.len() >= 3
    {
        debug = args[2] == "1";
    }

    let path = Path::new(&args[1]);
    let mut source_file = match File::open(&path)
    {
        Err(why) => panic!("Couldn't open {}: {}", args[1], Error::description(&why)),
        Ok(file) => file,
    };
    let mut src = String::new();
    match source_file.read_to_string(&mut src)
    {
        Err(reason) => panic!("Couldn't read {}: {}", args[1], Error::description(&reason)),
        Ok(_) => {},
    }
       
    let source : Vec<char> = src.chars().collect(); // get source as a list of characters
    

    // brainfuck setup 
    let mut pointer : usize = 0;
    let mut src_index : usize = 0;
    let mut tape : Vec<i64> = Vec::with_capacity(128);
    tape.push(0);
    while src_index < source.len()
    {
        if debug { println!("command: {}; index: {}; tape cell: {}", source[src_index], src_index, pointer); }
        run(&source, source[src_index], &mut tape, &mut pointer, &mut src_index); // run the command
    }
}

fn run(src : &Vec<char>, c : char, tape : &mut Vec<i64>, ptr : &mut usize, src_ptr : &mut usize) -> ()
{
    match c
    {
        '>' => { *ptr += 1; // increment tape pointer
                  while *ptr >= tape.len() // make sure that the tape has enough cells!
                  {
                      tape.push(0);
                  }
                  *src_ptr += 1;
        },
        '<' => { if *ptr > 0 { *ptr -= 1 }; *src_ptr += 1; }, // if we can, decrement tape pointer. Bad calls are ignored
        '+' => { tape[*ptr] += 1; *src_ptr += 1;}, // increment value at tape pointer
        '-' => { tape[*ptr] -= 1; *src_ptr += 1; }, // decrement value at tape pointer
        '!' => { print!("{}", tape[*ptr]); *src_ptr += 1; }, // output value as an int
        '.' => { print!("{}", tape[*ptr] as u8 as char); *src_ptr += 1; }, // output value as a char
        ',' => { 
            let stdin = std::io::stdin();
            let val_str : String = match stdin.lock().lines().next()
            {
                Some(s) => match s
                {
                    Ok(ss) => ss,
                    Err(e) => panic!("{}", Error::description(&e))
                },
                None => panic!("Stdin failed!"),
            };
            //print!("{}", val_str);
            match val_str.parse::<i64>()
            {
                Ok(v) => { tape[*ptr] = v; *src_ptr += 1 },
                Err(e) => { println!("{}", Error::description(&e)) }
            }

        },
        '[' => { if tape[*ptr] == 0 { *src_ptr += 1; find_forward(src, src_ptr, ']', '[') } else { *src_ptr += 1 } }, // search forward for the corresponding ]
        ']' => { if tape[*ptr] != 0 { *src_ptr -= 1; find_backward(src, src_ptr,'[', ']') } else { *src_ptr += 1 } }, // search backwards for the corresponding [
        _ => { *src_ptr += 1 }, // all other characters are comments
        };
}

fn find_forward(src : &Vec<char>, src_ptr : &mut usize, to_find : char, matching : char) -> ()
{
    let mut count = 0;
    while *src_ptr < src.len()
    {
        if src[*src_ptr] == to_find
        {
            if count == 0
            {
                return ();
            }
            else
            {
                count -= 1;
            }
        }
        else if src[*src_ptr] == matching
        {
            count += 1;
        }
        *src_ptr += 1;
    }
    return ();
}

fn find_backward(src : &Vec<char>, src_ptr : &mut usize, to_find : char, matching : char) -> ()
{
    let mut count = 0;
    while *src_ptr < src.len()
    {
        if src[*src_ptr] == to_find
        {
            if count == 0
            {
                return ();
            }
            else
            {
                count -= 1;
            }
        }
        else if src[*src_ptr] == matching
        {
            count += 1;
        }
        *src_ptr -= 1;
    }
}
