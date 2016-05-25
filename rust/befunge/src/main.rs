use std::collections::VecDeque;
use std::io::BufRead;

extern crate rand;

fn main() {
    println!("Hello, world!");
}

struct P
{
    w: usize,
    f: Vec<Vec<char>>,
    s: VecDeque<i64>,
    sm: bool,
    ptrs: Vec<Ptr>
}

struct Ptr
{
    s: VecDeque<i64>,
    x: usize,
    y: usize,
    dir: D,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum D { U, R, D, L }

fn n() -> P
{
    P
    {
        w: 80,
        f: Vec::new(),
        s: VecDeque::new(),
        ptrs: Vec::new()
    }
}

fn w(p: &mut P, s: &str)
{
    let mut c = s.chars().collect();
    if c.len() > p.w
    {
        for v in &mut p.f
        {
            v.reserve(c.len() - p.w);
            v.extend((0..c.len() - p.w).map(||' '));
        }
        p.w = c.len();
    }
    else
    {
        c.reserve(p.w - c.len());
        c.extend(0..p.w - c.len()).map(||' ');
    }
    p.f.push(c);
}

fn r(p: &mut P)
{
    for ptr in &mut p.ptrs
    {
        if ptr.sm
        {
            
        }
        else
        {
            e(p, ptr);
        }
    }
}

fn e(p: &mut P, ptr: &mut Ptr)
{
    match p.f[ptr.y][ptr.x]
    {
        '+' => // plus
        {
            let x = match (ptr.s.pop(), ptr.s.pop())
            {
                (Some(a),Some(b)) => a + b,
                (Some(a),_) |
                (_, Some(a)) => a,
                _ => 0
            };
            ptr.s.push(x);
        },
        '-' =>
        {
            let x = match (ptr.s.pop(), ptr.s.pop())
            {
                (Some(a), Some(b)) => a - b,
                (Some(a),_) => a,
                (_, Some(a)) => -a,
                _ => 0
            };
            ptr.s.push(x);
        },
        '*' =>
        {
            let x = match (ptr.s.pop(), ptr.s.pop())
            {
                (Some(a),Some(b)) => a * b,
                (Some(a),_) | (_, Some(a)) => a,
                _ => 1,
            };
            ptr.s.push(x);
        },
        '/' =>
        {
            let x = match (ptr.s.pop(), ptr.s.pop())
            {
                (Some(a),Some(b))=>a/b,
                (Some(a),_)=>a,
                (_,Some(a))=>1,
                _=>1
            };
            ptr.s.push(x);
        },
        '%' =>
        {
            let x = match (ptr.s.pop(), ptr.s.pop())
            {
                (Some(a),Some(b))=>a%b,
                _=>0
            };
            ptr.s.push(x);
        },
        '!' =>
        {
            let x = match ptr.s.pop()
            {
                0 => 1,
                _ => 0
            };
            ptr.s.push(x);
        },
        '`' =>
        {
            let x = match (ptr.s.pop(),ptr.s.pop())
            {
                (Some(a),Some(b)) if a > b => 1,
                _ => 0
            };
            ptr.s.push(x);
        },
        '>' => ptr.d = D::R,
        '<' => ptr.d = D::L,
        '^' => ptr.d = D::U,
        'v' => ptr.d = D::D,
        '?' =>
        {
            ptr.d = match rand::random::<u8>() % 4
            {
                0 => D::U,
                1 => D::L,
                2 => D::D,
                3 => D::R,
                _ => panic!("Mathematically impossible.")
            };
        },
        '_' =>
        {
            ptr.d = match ptr.s.pop()
            {
                Some(0) => D::L,
                _ => D::R
            };
        },
        '|' =>
        {
            ptr.d = match ptr.s.pop()
            {
                Some(0) => D::D,
                _ => D::U
            };
        },
        '"' =>{},
        ':' =>{},
        '\\' =>{},
        '$' =>{},
        '.' =>{},
        ',' =>{},
        '#' =>{},
        'g' =>{},
        'p' =>{},
        '&' =>{},
        '~' =>{},
        '@' =>{},
        '0' =>{},
        '1' =>{},
        '2' =>{},
        '3' =>{},
        '4' =>{},
        '5' =>{},
        '6' =>{},
        '7' =>{},
        '8' =>{},
        '9' =>{},
        // new additions
        's' =>, // pop top number from the stack, choose ptr of that number, switch their locations
    }
}
