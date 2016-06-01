use std::collections::VecDeque;
use std::io::{self, BufRead};

extern crate rand;

fn main() {
    println!("Hello, world!");
}

struct P
{
    w: usize,
    f: Vec<Vec<char>>,
    s: VecDeque<i64>,
    ptrs: Vec<Ptr>
}

struct Ptr
{
    s: VecDeque<i64>,
    x: usize,
    y: usize,
    sm: bool,
    sk: bool,
    d: D,
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

fn np() -> Ptr
{
    Ptr
    {
        s: VecDeque::new(),
        x: 0,
        y: 0,
        d: D::R,
        sm: false,
        sk: false
    }
}

fn w(p: &mut P, s: &str)
{
    let mut c: Vec<_> = s.chars().collect();
    if c.len() > p.w
    {
        for v in &mut p.f
        {
            v.reserve(c.len() - p.w);
            v.extend((0..c.len() - p.w).map(|_|' '));
        }
        p.w = c.len();
    }
    else
    {
        c.reserve(p.w - c.len());
        c.extend((0..p.w - c.len()).map(|_|' '));
    }
    p.f.push(c);
}

fn r(p: &mut P)
{
    for ptr in &mut p.ptrs
    {
        if ptr.sm
        {
            match p.f[ptr.y][ptr.x]
            {
                '"' => ptr.sm = false,
                _ =>
                {
                    let x = ptr.s.pop_front().unwrap_or(0) as u8 as char;
                    p.f[ptr.y][ptr.x] = x;
                }
            }
        }
        else if ptr.sk
        {
            match p.f[ptr.y][ptr.x]
            {
                '#' => ptr.sk = false,
                _ => ()
            }
        }
        else
        {
            e(p, ptr);
        }

        match ptr.d
        {
            D::U => ptr.y = (ptr.y + 1) % p.f.len(),
            D::D => ptr.y = (ptr.y - 1) % p.f.len(),
            D::R => ptr.x = (ptr.x + 1) % p.w,
            D::L => ptr.x = (ptr.x - 1) % p.w,
        }
    }
}

fn e(p: &mut P, ptr: &mut Ptr) -> bool
{
    match p.f[ptr.y][ptr.x]
    {
        '+' => // plus
        {
            let x = match (ptr.s.pop_front(), ptr.s.pop_front())
            {
                (Some(a),Some(b)) => a + b,
                (Some(a),_) |
                (_, Some(a)) => a,
                _ => 0
            };
            ptr.s.push_front(x);
        },
        '-' =>
        {
            let x = match (ptr.s.pop_front(), ptr.s.pop_front())
            {
                (Some(a), Some(b)) => a - b,
                (Some(a),_) => a,
                (_, Some(a)) => -a,
                _ => 0
            };
            ptr.s.push_front(x);
        },
        '*' =>
        {
            let x = match (ptr.s.pop_front(), ptr.s.pop_front())
            {
                (Some(a),Some(b)) => a * b,
                (Some(a),_) | (_, Some(a)) => a,
                _ => 1,
            };
            ptr.s.push_front(x);
        },
        '/' =>
        {
            let x = match (ptr.s.pop_front(), ptr.s.pop_front())
            {
                (Some(a),Some(b))=>a/b,
                (Some(a),_)=>a,
                (_,Some(a))=>1,
                _=>1
            };
            ptr.s.push_front(x);
        },
        '%' =>
        {
            let x = match (ptr.s.pop_front(), ptr.s.pop_front())
            {
                (Some(a),Some(b))=>a%b,
                _=>0
            };
            ptr.s.push_front(x);
        },
        '!' =>
        {
            let x = match ptr.s.pop_front()
            {
                Some(0) => 1,
                _ => 0
            };
            ptr.s.push_front(x);
        },
        '`' =>
        {
            let x = match (ptr.s.pop_front(),ptr.s.pop_front())
            {
                (Some(a),Some(b)) if a > b => 1,
                _ => 0
            };
            ptr.s.push_front(x);
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
            ptr.d = match ptr.s.pop_front()
            {
                Some(0) => D::L,
                _ => D::R
            };
        },
        '|' =>
        {
            ptr.d = match ptr.s.pop_front()
            {
                Some(0) => D::D,
                _ => D::U
            };
        },
        '"' => ptr.sm = true,
        ':' =>
        {
            let x = if let Some(x) = ptr.s.pop_front() { x } else { 0 };
            ptr.s.push_front(x);
            ptr.s.push_front(x);
        },
        '\\' =>
        {
            let x = ptr.s.pop_front().unwrap_or(0);
            let y = ptr.s.pop_front().unwrap_or(0);
            ptr.s.push_front(y);
            ptr.s.push_front(x);
        },
        '$' => {ptr.s.pop_front();},
        ',' =>
        {
            let x = ptr.s.pop_front().unwrap_or(0);
            println!("{}", x as u8 as char);
        },
        '.' =>
        {
            let x = ptr.s.pop_front().unwrap_or(0);
            println!("{}", x);
        },
        '#' => ptr.sk = true,
        'g' =>
        {
            let x = ptr.s.pop_front().unwrap_or(0);
            let y = ptr.s.pop_front().unwrap_or(0);

            let g = p.f.get(y as u64 as usize).and_then(|v| v.get(x as u64 as usize)).map(|x| *x as u8 as u64).unwrap_or(0);
            ptr.s.push_front(g as u8 as char);
        },
        'p' =>
        {
            let x = ptr.s.pop_front().unwrap_or(0);
            let y = ptr.s.pop_front().unwrap_or(0);
            let z = ptr.s.pop_front().unwrap_or(0);

            if (x as usize) > p.w
            {
                ex(p, (x as usize) - p.w);
            }

            if (y as usize) > p.f.len()
            {
                ey(p, (y as usize) - p.f.len());
            }

            p[y][x] = z as u8 as char;
        },
        '&' | '~' => 
        {
            let mut buf = [' '];
            match io::stdin().read(&mut buf)
            {
                Some(1) => ptr.s.push_front(buf[0] as u8 as u64),
                _ => ()
            }
        },
        '@' => return true,
        '0' => ptr.s.push_front(0),
        '1' => ptr.s.push_front(1),
        '2' => ptr.s.push_front(2),
        '3' => ptr.s.push_front(3),
        '4' => ptr.s.push_front(4),
        '5' => ptr.s.push_front(5),
        '6' => ptr.s.push_front(6),
        '7' => ptr.s.push_front(7),
        '8' => ptr.s.push_front(8),
        '9' => ptr.s.push_front(9),
        // new additions
        _ => ()
    }

    false
}

fn ey(p: &mut P, w: usize)
{
    for v in &mut p.f
    {
        v.extend(w);
        v.append((0..w).map(|_|' '));
    }
}

fn ex(p: &mut P, h: usize)
{
    for _ in 0..h
    {
        p.f.push(vec![' ';p.w]);
    }
}
