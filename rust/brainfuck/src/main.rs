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

// ignore 32bit registers for this
enum Register {
    RAX,
    RBX,
    RCX,
    RDX,
    RBP,
    RSI,
    RDI,
    RSP,
    R(u8),
    RFLAGS
}

fn rax() -> Term { Term::Reg(Register::RAX) }
fn rbx() -> Term { Term::Reg(Register::RBX) }
fn rcx() -> Term { Term::Reg(Register::RCX) }
fn rdx() -> Term { Term::Reg(Register::RDX) }
fn rbp() -> Term { Term::Reg(Register::RBP) }
fn rsi() -> Term { Term::Reg(Register::RSI) }
fn rdi() -> Term { Term::Reg(Register::RDI) }
fn rsp() -> Term { Term::Reg(Register::RSP) }
fn r(n:u8) -> Term { Term::Reg(Register::R(n)) }
fn rflags() -> Term { Term::Reg(Register::RFLAGS) }

fn lit(x:i64) -> Term { Term::Lit(x) }
fn label<T: Into<String>>(s:T)->Term { Term::Label(s.into()) }
fn ptr(t:Term) -> Term { Term::Ptr(Box::new(t)) }

enum Term {
    Reg(Register),
    Lit(i64),
    Label(String),
    Ptr(Box<Term>),
    UnknownConst
}

enum X64 {
    Inc(Term),
    Dec(Term),
    Add(Term, Term),
    Sub(Term, Term),
    Mul(Term, Term),
    Div(Term, Term),
    Addi(Term, Term),
    Subi(Term, Term),
    Muli(Term, Term),
    Divi(Term, Term),
    Mov(Term, Term),
    Push(Term),
    Pop(Term),
    Call(Term),
    Jmp(Term, bool), // bool: true if a relative jmp
    Cmp(Term, Term),
    Je(Term, bool),
    Jne(Term, bool),
    Jl(Term, bool),
    Jle(Term, bool),
    Jg(Term, bool),
    Jge(Term, bool),
    Jz(Term, bool),
    Syscall(String),
    Loop(Term, bool),
    Mod(Term, Term),
    NOP
}

fn inc(t: Term) -> X64 { X64::Inc(t) }
fn dec(t: Term) -> X64 { X64::Dec(t) }
fn add(t: Term, t2: Term) -> X64 { X64::Add(t, t2) }
fn sub(t: Term, t2: Term) -> X64 { X64::Sub(t, t2) }
fn mul(t: Term, t2: Term) -> X64 { X64::Mul(t, t2) }
fn div(t: Term, t2: Term) -> X64 { X64::Div(t, t2) }
fn addi(t: Term, t2: Term) -> X64 { X64::Addi(t, t2) }
fn subi(t: Term, t2: Term) -> X64 { X64::Subi(t, t2) }
fn muli(t: Term, t2: Term) -> X64 { X64::Muli(t, t2) }
fn divi(t: Term, t2: Term) -> X64 { X64::Divi(t, t2) }
fn mov(t: Term, t2: Term) -> X64 { X64::Mov(t, t2) }
fn push(t: Term) -> X64 { X64::Push(t) }
fn pop(t: Term) -> X64 { X64::Pop(t) }
fn call(t: Term) -> X64 { X64::Call(t) }
fn jmp(t: Term, b: bool) -> X64 { X64::Jmp(t, b) }
fn cmp(t: Term, t2: Term) -> X64 { X64::Cmp(t, t2) }
fn je(t: Term, b: bool) -> X64 { X64::Je(t, b) }
fn jne(t: Term, b: bool) -> X64 { X64::Jne(t, b) }
fn jl(t: Term, b: bool) -> X64 { X64::Jl(t, b) }
fn jle(t: Term, b: bool) -> X64 { X64::Jle(t, b) }
fn jg(t: Term, b: bool) -> X64 { X64::Jg(t, b) }
fn jge(t: Term, b: bool) -> X64 { X64::Jge(t, b) }
fn jz(t: Term, b: bool) -> X64 { X64::Jz(t, b) }
fn syscall<T: Into<String>>(s: T) -> X64 { X64::Syscall(s.into()) }
fn loop_(t: Term, b: bool) -> X64 { X64::Loop(t, b) }

type ASM = Vec<X64>;
// todo: change ASM to a struct containing a Vec<u8> and just write machine code directly

fn asm(code: &Vec<MergedInstruction>) -> ASM
{
    // register use:
    // RDX - instruction to return to on fn exit
    // RBP - end of stack - loop stack moves downward!
    // RSP - start of stack
    // R8  - start of tape 
    // R9  - end of tape
    // R10 - current tape position
    // R12 - current tape value (r11 is killed by syscall)
    // R12 - stackptr of last open loop - Not using - OutI pollutes stack

    /// writes the registers to the top of the stack
    fn spill(code: &mut Vec<X64>, registers: &[Register]) {
        for reg in registers {
            code.push(mov(ptr(rsp()), reg));
            code.push(add(rsp(), lit(8)));
        }
        code.push(sub(rsp(), lit(8*(registers.len()))));
    }

    /// reads the registers from the top of the stack
    fn read(asm: &mut Vec<X64>, registers: &[Register]) {
        for reg in registers {
            asm.push(mov(reg, ptr(rsp())));
            asm.push(add(rsp(), lit(8)));
        }
        asm.push(sub(rsp(), lit(8*registers.len())));
    }

    fn callconv(asm: &mut Vec<X64>, fn_start: i64) {
        asm.push(mov(rdx(), rip()));
        asm.push(add(rdx(), lit(2))); // skip the jmp when returning!
        asm.push(jmp(lit(fn_start), false))
    }

    const SBRK: Term = Term::Lit(0x2D);
    const CALL_LEN: Term = Term::Lit(3);
    const EXIT: Term = Term::Lit(0x01);
    const WRITE: Term = Term::Lit(0x01);

    let mut asm = Vec::new();
    let alloc_addr: usize = 1;
    let realloc_addr: usize = 12;

    // prelude
    // zero all the registers (?)
    // allocate a page of tape space on the heap
    // allocate stack register overflow space

    asm.push(sub(rsp(), lit(128)));

    // allocates a new page via sbrk
    asm.push(mov(rax(), SBRK));
    asm.push(mov(rdi(), lit(0)));    // get current break point
    asm.push(syscall());
    asm.push(mov(rdi(), lit(4096))); // result in rax
    asm.push(add(rdi(), rax()));     // so add to rbx, call again
    asm.push(mov(rax(), SBRK));
    asm.push(syscall());
    asm.push(cmp(rax(), lit(0))); // check if actually alloc'd - if oom, exit
    asm.push(jle(lit(3), true));
    asm.push(mov(rdi(), rax()));
    asm.push(mov(rax(), EXIT));
    asm.push(syscall());
    asm.push(jmp(rdx(), false));

    asm.push(mov(r(8), rax()));     // set start of tape
    asm.push(mov(r(9), rax()));     // set end of tape   
    asm.push(add(r(9), lit(4096))); // add tape length
    asm.push(mov(r(10), lit(2048))); // set current position to middle of tape
    asm.push(jmp(lit(12), true)); // skip runtime functions

    // inline runtime to move the tape forward a page

    callconv(&mut asm, alloc_addr); // allocate a new page on the end of the tape
    asm.push(mov(rcx(), r(9)));
    asm.push(sub(rcx(), r(8))); // rcx := len(tape)
    asm.push(mov(rax(), r(9)));
    asm.push(add(rax(), lit(4096))); // rax := dest
    asm.push(mov(rbx(), r(9))); // rbx := src
    asm.push(mov(ptr(rbx()), ptr(rax()))); // src -> dest
    asm.push(dec(rax()));
    asm.push(dec(rbx()));
    asm.push(loop_(lit(-3), true));
    asm.push(add(r(9), lit(4096)));
    asm.push(jmp(rdx(), false));


    // add user code

    // todo: calculating positions at compile is probably better
    // stack of locations to update with jump destinations
    // push on OpenL, set and pop on CloseL
    // let mut jumpstack: Vec<usize> = Vec::new();

    for instr in code {
        match instr {
            MergedInstruction::Add(x) => asm.push(add(r(12), lit(x))),
            MergedInstruction::Sub(x) => asm.push(sub(r(12), lit(x))),
            MergedInstruction::MoveR(x) => {
                asm.push(mov(ptr(r(10)), r(12))); // move value onto the tape
                asm.push(mov(rax(), r(10))); // check that moving x to the right does not go off the end of the tape
                asm.push(add(rax(), lit(x)));
                asm.push(cmp(rax(), r(9)));
                asm.push(jl(CALL_LEN, true));
                callconv(&mut asm, alloc_addr);
                asm.push(add(r(10), lit(x))); // move the tape pointer right x
                asm.push(mov(r(12), ptr(r(10)))); // move the value at the tape pointer into the value register
            },
            MergedInstruction::MoveL(x) => {
                asm.push(mov(ptr(r(10)), r(12))); // move value onto the tape
                asm.push(mov(rax(), r(8))); // bounds check
                asm.push(sub(rax(), lit(x)));
                asm.push(cmp(rax(), r(8)));
                asm.push(jg(CALL_LEN, true));
                callconv(&mut asm, realloc_addr);
                asm.push(sub(r(10), lit(x)));
                asm.push(mov(r(12), ptr(r(10))));
            },
            MergedInstruction::OutC => {
                asm.push(mov(rax(), WRITE));
                asm.push(mov(rdi(), lit(1))); // 1 = stdout
                asm.push(mov(ptr(rbp()), r(11))); // write the char to the stack
                asm.push(mov(rsi(), rbp())); // push ptr to the correct register
                asm.push(mov(rdx(), lit(1))); // write 1 char
                asm.push(syscall()); // print
                // todo: error handling
            },
            MergedInstruction::OutI => {
                // write the number as a number
                // to the stack in reverse order
                asm.push(mov(rax(), r(12))); // copy the tape value into rax
                asm.push(mov(rsi(), rbp())); // set up addr pointer
                asm.push(mov(rdx(), lit(0)));// #chars

                asm.push(mov(rbx(), rax()));
                asm.push(mov(rcx(), rbx()));
                asm.push(div(rcx(), lit(10)));
                asm.push(mul(rcx(), lit(10)));
                asm.push(sub(rbx(), rcx()));
                asm.push(mov(ptr(rsi()), rbx()));
                asm.push(inc(rsi()));
                asm.push(div(rax(), lit(10)));
                asm.push(inc(rdi()));
                asm.push(cmp(rax(), lit(0)));
                asm.push(je(lit(-10), true));
                
                asm.push(mov(rax(), WRITE));
                asm.push(mov(rdi(), STDOUT));
                // rsi and rdx already have correct params
                asm.push(syscall);
                // asm.push(cmp(rax(), lit(0))); // todo: error handling
                // asm.push()
            },
            MergedInstruction::In => {
                asm.push(mov(rax(), READ));
                asm.push(mov(rdi(), STDIN));
                asm.push(mov(rsi(), rsp())); // just write to the top of the stack
                asm.push(mov(rdx(), lit(1))); // read a single char
                asm.push(syscall);
                // todo: error handling
                asm.push(mov(r(10), ptr(rsp()))); // read char into tape value
            },
            MergedInstruction::OpenL => {
                asm.push(X64::NOP); // pushed here as a marker
            },
            MergedInstruction::CloseL => {
                asm.push(cmp(r(12), lit(0)));
                asm.push(je(Term::UnknownConst, true));
            }
        }
    }

    // add exit syscall
    asm.push(rax(), EXIT);
    asm.push(rdi(), lit(0));
    asm.push(syscall());
}

fn machine(asm: Vec<X64>) -> Vec<u8> {

    Vec::new()
}
