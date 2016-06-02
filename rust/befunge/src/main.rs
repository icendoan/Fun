use std::collections::VecDeque;
use std::io::{self, BufRead};

extern crate rand;

fn main()
{
    println!("Hello, world!");
}

struct P
{
    w: usize,
    f: Vec<Vec<char>>,
    ptrs: Vec<Ptr>,
    g: u64,
    r: [i64; 16],
}

struct Ptr
{
    s: VecDeque<i64>,
    x: usize,
    y: usize,
    sm: bool,
    sk: bool,
    d: D,
    f: bool,
    r: usize,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum D
{
    U,
    R,
    D,
    L,
}

fn n() -> P
{
    P {
        w: 80,
        f: Vec::new(),
        s: VecDeque::new(),
        ptrs: Vec::new(),
    }
}

fn np() -> Ptr
{
    Ptr {
        s: VecDeque::new(),
        sf: VecDeque::new(),
        x: 0,
        y: 0,
        d: D::R,
        sm: false,
        sk: false,
        f: true,
        r: 0,
        fm: false,
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
            v.extend((0..c.len() - p.w).map(|_| ' '));
        }
        p.w = c.len();
    }
    else
    {
        c.reserve(p.w - c.len());
        c.extend((0..p.w - c.len()).map(|_| ' '));
    }
    p.f.push(c);
}

fn r(p: &mut P)
{
    for ptr in &mut p.ptrs
    {
        if ptr.f
        {
            continue;
        }

        if ptr.sm
        {
            match p.f[ptr.y][ptr.x]
            {
                '"' => ptr.sm = false,
                _ =>
                {
                    let x = ptr.s.pop_back().unwrap_or(0) as u8 as char;
                    p.f[ptr.y][ptr.x] = x;
                },
            }
        }
        else if ptr.sk
        {
            match p.f[ptr.y][ptr.x]
            {
                '#' => ptr.sk = false,
                _ => (),
            }
        }
        else
        {
            e(p, ptr);
        }

        match ptr.d
        {
            // todo: respect topology
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
            let x = match (ptr.s.pop_back(), ptr.s.pop_back())
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
            let x = match (ptr.s.pop_back(), ptr.s.pop_back())
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
            let x = match (ptr.s.pop_back(), ptr.s.pop_back())
            {
                (Some(a),Some(b)) => a * b,
                (Some(a),_) | (_, Some(a)) => a,
                _ => 1,
            };
            ptr.s.push_front(x);
        },
        '/' =>
        {
            let x = match (ptr.s.pop_back(), ptr.s.pop_back())
            {
                (Some(a),Some(b)) => a/b,
                (Some(a),_) => a,
                (_,Some(a)) => 1,
                _ => 1
            };
            ptr.s.push_front(x);
        },
        '%' =>
        {
            let x = match (ptr.s.pop_back(), ptr.s.pop_back())
            {
                (Some(a),Some(b))=>a%b,
                _=>0
            };
            ptr.s.push_front(x);
        },
        '!' =>
        {
            let x = match ptr.s.pop_back()
            {
                Some(0) => 1,
                _ => 0
            };
            ptr.s.push_front(x);
        },
        '`' =>
        {
            let x = match (ptr.s.pop_back(),ptr.s.pop_back())
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
        }, // <
        '_' =>
        {
            ptr.d = match ptr.s.pop_back()
            {
                Some(0) => D::L,
                _ => D::R
            };
        },
        '|' =>
        {
            ptr.d = match ptr.s.pop_back()
            {
                Some(0) => D::D,
                _ => D::U
            };
        },
        '"' => ptr.sm = true,
        ':' =>
        {
            let x = if let Some(x) = ptr.s.pop_back() { x } else { 0 };
            ptr.s.push_front(x);
            ptr.s.push_front(x);
        },
        '\\' =>
        {
            let x = ptr.s.pop_back().unwrap_or(0);
            let y = ptr.s.pop_back().unwrap_or(0);
            ptr.s.push_front(y);
            ptr.s.push_front(x);
        },
        '$' => {ptr.s.pop_back();},
        ',' =>
        {
            let x = ptr.s.pop_back().unwrap_or(0);
            println!("{}", x as u8 as char);
        },
        '.' =>
        {
            let x = ptr.s.pop_back().unwrap_or(0);
            println!("{}", x);
        },
        '#' => ptr.sk = true,
        'g' =>
        {
            let x = ptr.s.pop_back().unwrap_or(0);
            let y = ptr.s.pop_back().unwrap_or(0);
            let g = p.f.get(y as u64 as usize)
                .and_then(|v| v.get(x as u64 as usize))
                .map(|x| *x as u8 as u64)
                .unwrap_or(0);

            ptr.s.push_back(g as u8 as char);
        },
        'p' =>
        {
            let x = ptr.s.pop_back().unwrap_or(0);
            let y = ptr.s.pop_back().unwrap_or(0);
            let z = ptr.s.pop_back().unwrap_or(0);

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
                Some(1) => ptr.s.push_back(buf[0] as u8 as u64),
                _ => ()
            }
        },
        '@' => { return true },
        '0' => ptr.s.push_back(0),
        '1' => ptr.s.push_back(1),
        '2' => ptr.s.push_back(2),
        '3' => ptr.s.push_back(3),
        '4' => ptr.s.push_back(4),
        '5' => ptr.s.push_back(5),
        '6' => ptr.s.push_back(6),
        '7' => ptr.s.push_back(7),
        '8' => ptr.s.push_back(8),
        '9' => ptr.s.push_back(9),
        // new additions

        // print entire stack as a string without emptying
        'P' =>
        {
        },

        // clear stack
        '¢' => { ptr.s.clear(); },
        // field changes
        // pop top number from the stack, switch topology to the surface of that genus.
        'Π' =>
        {
            let x = ptr.s.pop_back().unwrap_or(0);
            p.g = x as u64;
        },
        // pop 6 numbers from the stack, x0-x5, copy box [x0,x2]×[x1,x3]
        // to the square starting at (x4,x5)
        'c' =>
        {
            let mut coords = [0;6];
            for c in &mut coords
            {
                let x = ptr.s.pop_back().unwrap_or(0);
                *c = x as usize;
            }

            if coords[4] > w
            {
                ey(p, coords[4] - w);
            }

            if coords[5] > p.f.len()
            {
                ex(p, coords[5] - p.f.len());
            }

            let xs = coords[2] - coords[0];
            let ys = coords[3] - coords[1];

            for x in 0..xs
            {
                for y in 0..ys
                {
                    p.f[x + coords[4]][y + coords[5]] = p.f[x + coords[0]][y + coords[1]];
                }
            }
        },

        // other pointers and registers
        // pop a single number off the stack, freeze that pointer (if frozen, nop)
        'φ' =>
        {
            let x = ptr.s.pop_back().unwrap_or(0) as usize;
            p.ptrs[x % p.ptrs.len()].f = true;
        },
        // pop a single number off the stack, thaw that pointer (if not frozen, nop)
        'ð' =>
        {
            let x = ptr.s.pop_back().unwrap_or(0) as usize;
            p.ptrs[x % p.ptrs.len()].f = false;
        },
        // thaw all pointers
        'Ð' =>
        {
            for ptr in &mut p.ptrs
            {
                ptr.f = false;
            }
        },
        // pop a single number off the stack, switch active register to that register
        'ρ' =>
        {
            let x = ptr.s.pop_back().unwrap_or(0) as usize;
            ptr.r = x % 16;
        },
        // pop two values off the stack, spawn a new (thawed) pointer at (x0, x1)
        'ω' =>
        {
            let mut p = np();
            let mut x = ptr.s.pop_back().unwrap_or(0) as usize;
            let mut y = ptr.s.pop_back().unwrap_or(0) as usize;
            p.x = x;
            p.y = y;
            p.f = false;
        },

        // pop a single number off the stack, delete that pointer
        // if there are no more pointers, halt execution
        'Ω' =>
        {
            let x = ptr.s.pop_back().unwrap_or(0) as usize;
            p.ptrs.remove(x);

            if ptrs.is_empty()
            {
                return true;
            }
        },

        // pop a single number off the stack, move into active register
        'µ' =>
        {
            let x = ptr.s.pop_back().unwrap_or(0);
            p.r[ptr.r] = x;
        },
        // push the active register onto the stack
        // note: capital MU
        'Μ' =>
        {
            ptr.s.push_back(p.r[ptr.r]);
        },

        // increment contents of current register
        'ι' => { p.r[ptr.r] += 1; }

        // numerics

        // pop a single value, n, off the stack, push the nth prime number onto the stack
        'π' => {},
        // pop a single value off the stack, push its prime factors onto the stack
        'Φ' => {},
        // flatten the current stack as a number, with the radix the value of the active register
        '·' => {},
        // square the top value on the stack
        '²' => {},
        // cube the top stack value
        '³' => {},
        // square root the top stack value
        '½' => {},
        // invert the sign of the top stack value
        '±' => {},
        // pop a value off the stack (x)
        // considering the rest of the stack x1..xn as a polynomial p(x) = Σ i:0..n x_i * x^i
        // push the value of p(x) onto the stack
        'Σ' => {},
        // pop a value, n, off the stack
        // push the nth digit of τ onto the stack
        'τ' => {},
        _ => ()
    }

    false
}

fn ey(p: &mut P, w: usize)
{
    for v in &mut p.f
    {
        v.extend(w);
        v.append((0..w).map(|_| ' '));
    }
}

fn ex(p: &mut P, h: usize)
{
    for _ in 0..h
    {
        p.f.push(vec![' ';p.w]);
    }
}
