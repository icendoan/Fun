#![feature(box_syntax, box_patterns, slice_patterns)]
use std::io::prelude::*;
use std::fs::File;
use std::path::Path;
use std::error::Error;
use std::{env, ptr};
use std::cmp::{min, max};

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

enum Instruction
{
    MoveL,
    MoveR,
    OpenLoop,
    CloseLoop,
    Inc,
    Dec,
    Input,
    OutputChar,
    OutputInt
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
    
    let mut out = match File::create(if let Some(o) = c.out { o } else { "a.out" })
    {
        Ok(f) => f,
        Err(e) => return vec![Err
                              {
                                  errtype: ErrType::FileError,
                                  msg: e.description.to_owned(),
                                  linenum: 0,
                                  colnum: 0
                              }]
    };

    out.write_all(text.as_bytes());
}

fn parse(src: &str) -> Vec<Instruction>
{
    let mut v = Vec::with_capacity(src.len());
    
    for c in src.chars()
    {
        match c
        {
            '>' => v.push(Instruction::MoveL),
            '<' => v.push(Instruction::MoveR),
            '[' => v.push(Instruction::OpenLoop),
            ']' => v.push(Instruction::CloseLoop),
            '+' => v.push(Instruction::Inc),
            '-' => v.push(Instruction::Dec),
            ',' => v.push(Instruction::Input),
            '.' => v.push(Instruction::OutputChar),
            '!' => v.push(Instruction::OutputInt),
            _ => (),
        }
    }
    
    v
}

enum MergedInstruction
{
    MoveR(usize),
    MoveL(usize),
    Add(usize),
    Sub(usize),
    OpenL,
    CloseL,
    In,
    OutI,
    OutC,
    NOP,
}

fn opt(mut code: Vec<Instruction>) -> Vec<MergedInstruction>
{
    let mut tape_changed = false;
    let mut min_changed: i64 = 0;
    let mut max_changed: i64 = 0;
    let mut cur_pos: i64 = 0;

    let mut buffer = Vec::with_capacity(code.len());

    // strip leading shifts
    for i in code
    {
        match i
        {
            Instruction::MoveL |
            Instruction::MoveR if !tape_changed => (),
            Instruction::Dec |
            Instruction::Inc =>
            {
                tape_changed = true;
                buffer.push(i);
            },
            _ => buffer.push(i)
        }
    }

    ptr::swap(&mut code, &mut buffer);
    buffer.clear();

    // strip adjacent canceling operations
    // do this in a loop until they are all removed
    let mut has_adjacents = true;

    while has_adjacents
    {
        let mut dropped = false;

        for chunk in code.chunks(2)
        {
            match chunk
            {
                [x] => buffer.push(x), // last instruction, shouldn't drop
                [Instruction::MoveL, Instruction::MoveR] |
                [Instruction::MoveR, Instruction::MoveL] |
                [Instruction::Inc, Instruction::Dec] |
                [Instruction::Dec, Instruction::Inc] => dropped = true,
                [x, y] =>
                {
                    buffer.push(x);
                    buffer.push(y);
                }
            }
        }

        has_adjacents = dropped;
        ptr::swap(&mut code, &mut buffer);
        buffer.clear();
    }
   
    // calculate conservative bounds on tape changes
    // and strip dead loops


    let mut skipping = 0;
    let mut copy_all = false;
    for i in &code
    {
        if copy_all
        {
            // escape hatch once OOB analysis fails
            buffer.push(i);
            continue
        }

        if skipping > 0
        {
            match i
            {
                Instruction::OpenLoop => skipping += 1,
                Instruction::CloseLoop => skipping -= 1,
                _ => ()
            }

            continue
        }

        match *i
        {
            Instruction::Inc |
            Instruction::Dec =>
            {
                min_changed = min(cur_pos, min_changed);
                max_changed = max(cur_pos, max_changed);
                buffer.push(i);
            },

            Instruction::MoveL =>
            {
                cur_pos -= 1;
                buffer.push(i);
            },

            Instruction::MoveR =>
            {
                cur_pos += 1;
                buffer.push(i);
            },

            Instruction::OpenLoop if cur_pos > max_changed || cur_pos < min_changed =>
            {
                skipping = 1;
            },

            Instruction::OpenLoop =>
            {
                buffer.push(i);
                copy_all = true;
            },

            _ => buffer.push(i),

        }
    }
    
    // merge instructions
    let mut merged = Vec::with_capacity(code.len());
    
    let mut instruction_size = 0;
    let mut acc = NOP;
    for i in code
    {
        match (acc, i)
        {
            (MergedInstruction::MoveR(x), Instruction::MoveR) => acc = MergedInstruction::MoveR(x+1),
            (MergedInstruction::MoveR(1), Instruction::MoveL) => acc = MergedInstruction::NOP,
            (MergedInstruction::MoveR(x), Instruction::MoveL) => acc = MergedInstruction::MoveR(x-1), 
            (MergedInstruction::MoveL(x), Instruction::MoveL) => acc = MergedInstruction::MoveL(x+1),
            (MergedInstruction::MoveL(1), Instruction::MoveR) => acc = MergedInstruction::NOP,
            (MergedInstruction::MoveL(x), Instruction::MoveR) => acc = MergedInstruction::MoveL(x-1),
            (MergedInstruction::Add(x), Instruction::Inc) => acc = MergedInstruction::Add(x+1),
            (MergedInstruction::Add(1), Instruction::Dec) => acc = MergedInstruction::NOP,
            (MergedInstruction::Sub(1), Instruction::Inc) => acc = MergedInstruction::NOP,
            (MergedInstruction::Sub(x), Instruction::Dec) => acc = MergedInstruction::Sub(x+1),
            (a, i) =>
            {
                merged.push(a);
                acc = match i
                {
                    Instruction::MoveL => MergedInstruction::MoveL(1),
                    Instruction::MoveR => MergedInstruction::MoveR(1),
                    Instruction::OpenLoop => MergedInstruction::OpenL,
                    Instruction::CloseLoop => MergedInstruction::CloseL,
                    Instruction::Inc => MergedInstruction::Add(1),
                    Instruction::Dec => MergedInstruction::Sub(1),
                    Instruction::Input => MergedInstruction::In,
                    Instruction::OutputChar => MergedInstruction::OutC,
                    Instruction::OutputInt => MergedInstruction::OutI
                }
            }
        }
    }

    merged
}

fn asm(code: &Vec<MergedInstruction>) -> String
{
    // register use:
    // max known tape   - 
    // min known tape   - 
    // tape start addr  - 
    // current position - 
    // current value    -
    let mut open_ctr = 0;
    let mut close_ctr = 0;
    let mut asm_output = String::new();

    for instr in code
    {
        let s = match instr
        {
            MergedInstruction::MoveR(x) =>,
            MergedInstruction::MoveL(x) => ,
            MergedInstruction::Add(x) => format!("add %eax {}", x),
            MergedInstruction::Sub(x) => format!("sub %eax {}", x),
            MergedInstruction::OpenL =>,
            MergedInstruction::CloseL =>,
            MergedInstruction::In =>,
            MergedInstruction::OutI =>,
            MergedInstruction::OutC =>,
            MergedInstruction::NOP => String::new()
        };

        asm_output.push_str(&s);
    }
}
