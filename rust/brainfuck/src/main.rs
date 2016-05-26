#![feature(box_syntax, box_patterns)]
use std::io::prelude::*;
use std::fs::File;
use std::path::Path;
use std::error::Error;
use std::env;

enum Option
{
    Interpret(&str, bool),
    Compile(CompileFlags)
}

struct CompileFlags
{
    srcpath: &str,
    out: Option<&str>,
    opt: u32
}

fn main()
{
    // load the source file
    let args : Vec<String> = env::args().collect(); // collect the program arguments

    match parse_args(args)
    {
        Interpret(path, debug) =>
        {
            let mut src = String::new();
            match File::open(path)
            {
                Ok(f) => f.read_to_string(&src),
                Err(e) => return println!("Error: {}", e)
            }

            interpret(src, debug)
        },

        Compile(c) =>
        {
            match compile(c)
            {
                Ok(_) => (),
                Err(errors) =>
                {
                    for e in errors
                    {
                        println!("{}", e);
                    }
                }
            }
        }
    }
}

fn interpret(src: &str, debug: bool)
{
    
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

fn run(src: &Vec<char>, c: char, tape: &mut Vec<i64>, ptr: &mut usize, src_ptr: &mut usize)
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

enum ErrType
{
    Warning,
    ParseError,
    FileError
}

struct Err
{
    errtype: ErrType,
    msg: String,
    linenum: u64,
    colnum: u64
}

enum ParseTree
{
    Span(Vec<Instruction>),
    Cond(Vec<ParseTree>, box ParseTree), // in order to handle [___[___]___]_____
}

enum Instruction
{
    MoveL,
    MoveR,
    OpenLoop(usize),
    CloseLoop(usize),
    Inc,
    Dec,
    Input,
    Output
}

fn compile(c: CompileFlags) -> Result<(), Vec<Err>>
{
    let src = match File::open(c.srcpath)
    {
        Ok(f) =>
        {
            let mut s = String::new();
            if let Err(e) = f.read_to_string(&mut s)
            {
                return vec![Err { errtype: ErrType::FileError, msg: e.description.to_owned(), linenum: 0, colnum: 0 }]
            }

            s
        },
        Err(e) => return vec![Err{ errtype: ErrType::FileError, msg: e.description.to_owned(), linenum: 0, colnum: 0 }]
    };

    let mut parse_tree = parse(&src)?;
    if c.opt { opt(&mut parse_tree) }
    eval_jmps(&mut parse_tree);
    let text = asm(parse_tree);
}

struct Span
{
    code: Vec<Instruction>,
    depth: usize
}

// todo: work out how to convert to a parse tree
// maybe build a stack of spans with depth
// and compile an actual tree later?
fn parse(src: &str) -> ParseTree
{

    let mut lc = 1;
    let mut cc = 1;
    let mut n = 0;

    let mut parens_nests = Vec::new();

    let mut current_span = Vec::new();
    let mut span_stack = Vec::new();
    
    for c in src.chars()
    {
        match c
        {
            '+' => current_span.push(Instruction::Inc),
            '-' => current_span.push(Instruction::Dec),
            '<' => current_span.push(Instruction::MoveL),
            '>' => current_span.push(Instruction::MoveR),
            '[' =>
            {
                paren_nests.push((n, lc, cc));

            },
            ']' =>,
            '\n' => { lc += 1; cc = 1; n += 1; continue },
            _ => ()
        }

        cc += 1;
        n += 1;
    }
}

fn opt(tree: &mut ParseTree)
{
    
}

fn eval_jmps(tree: &mut ParseTree)
{
    
}
