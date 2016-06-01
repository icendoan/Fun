#![feature(box_syntax, box_patterns, non_ascii_idents)]

use std::fmt::{self, Display};

fn main()
{
    println!("Hello, world!");
    println!("{}", k());
}

// use debruijn indices
// can just have a context vec
#[derive(Clone, Debug, PartialEq, Eq)]
enum λ
{
    L(Box<λ>),
    V(u64),
    A(Box<λ>, Box<λ>),
    E,
}


fn parse_ski(src: &str) -> λ
{
    λ::V(0)
}

fn parse_λ(src: &str) -> λ
{
    #[derive(PartialEq, Eq, Debug)]
    enum Emptyλ
    {
        L,
        V(u64),
        AO,
        AC,
        PO,
        PC,
    }

    fn lex(src: &str) -> Vec<Emptyλ>
    {
        let mut v = Vec::new();
        let mut n = 0;
        let mut ns = false;
        for c in src.chars()
        {
            match c
            {
                'λ' => {v.push(Emptyλ::L); ns = false},
                '(' => {v.push(Emptyλ::AO); ns = false},
                ')' => {v.push(Emptyλ::AC); ns = false},
                '[' => {v.push(Emptyλ::PO); ns = false},
                ']' => {v.push(Emptyλ::PC); ns = false},
                c if c.is_numeric && !ns =>
                {
                    ns = true;
                    n = if let Some(x) = c.to_digit(10)
                    {
                        x
                    }
                    else
                    {
                        println!("Lex error, could not read number");
                        return;
                    }
                },
                c if c.is_numeric && ns =>
                {
                    n = match c.to_digit(10)
                    {
                        Some(x) => (n * 10) + x,
                        None =>
                        {
                            println!("Lex error, could not read number");
                            return;
                        }
                    }
                },

                _ if ns => // end of a number
                {
                    v.push(Emptyλ::V(n));
                    n = 0;
                    ns = false;
                }
            }
        }

        v
    }

    let mut term = λbuilder::E;
    let mut termstack = Vec::new();
    let mut tokens = lex(src);
    tokens.reverse();

    for token in tokens
    {
        term = match token
        {
            Emptyλ::V(v) => match term
            {
                λ::E => λ::V(v),
                _ => return println!("Parse error"),
            },

            Emptyλ::L => λ::L(box term),
            Emptyλ::PO | Emptyλ::PC =>
            {
                termstack.push(term);
                term = λ::E;
            },
            Emptyλ::AC if term == λ::E => (),
            Emptyλ::AC => return println!("Parse error"),
            Emptyλ::AO =>
            {
                let p = match termstack.pop()
                {
                    Some(x) => x,
                    _ => return println!("Parse error"),
                };

                term = λ::A(box term, box x);
            },
        }
    }

    term
}

fn eval(l: &mut λ, c: &mut Vec<λ>)
{

}

fn β(l: &mut λ, r: &λ, ix: u64)
{
    let mut stack = Vec::new();
    let mut curr = l;
    let e = λ::E;

    while (*curr != λ::E) && stack.is_empty()
    {
        match *curr
        {
            λ::L(box ref mut x) => curr = x,
            λ::A(box ref mut x, box ref mut cont) =>
            {
                stack.push(cont);
                curr = x;
            },
            λ::V(v) if v == ix =>
            {
                *curr = r.clone();
                curr = match stack.pop()
                {
                    Some(ptr) => ptr,
                    None => &e,
                };
            },
            λ::V(v) =>
            {
                curr = match stack.pop()
                {
                    Some(ptr) => ptr,
                    None => &e,
                };
            },
        }
    }
}

fn s() -> λ
{
    // λλλ((2)[0])[(1)[0]]
    λ::L(box λ::L(box λ::L(box λ::A(box λ::A(box λ::V(2), box λ::V(0)),
                                        box λ::A(box λ::V(1), box λ::V(0))))))
}

fn k() -> λ
{
    // λλ1
    λ::L(box λ::L(box λ::V(1)))
}

impl Display for λ
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match *self
        {
            λ::L(box ref lam) => write!(f, "λ{}", lam),
            λ::V(ref x) => write!(f, "{}", x),
            λ::A(box ref l, box ref r) => write!(f, "({})[{}]", l, r),
        }
    }
}

fn scan_matching(x: &str, s: char, e: char) -> &str
{
    let mut ctr = 0;
    for (ix, c) in x.chars().enumerate()
    {
        match c
        {
            c if c == s => ctr += 1,
            c if c == e => if ctr == 0
            {
                return x[0..ix];
            }
            else
            {
                ctr -= 1
            },
            c => (),
        }
    }
}
