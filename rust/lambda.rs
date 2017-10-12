#![feature(box_syntax, box_patterns)]
#[macro_use]
extern crate nom;
use std::io;
use std::io::BufRead;
use std::collections::HashMap;

use untyped::{parse, sub, Expr};

static STD: [&'static str; 27] = ["S=λn.λf.λx.(f$((n$f)$x))",
                                  "P=λn.λf.λx.(((n$λg.λh.(h$(g$f)))$λu.x)$λu.u)",
                                  "0=λf.λx.x",
                                  "1=(S$0)",
                                  "2=(S$1)",
                                  "3=(S$2)",
                                  "4=(S$3)",
                                  "5=(S$4)",
                                  "6=(S$5)",
                                  "7=(S$6)",
                                  "8=(S$7)",
                                  "9=(S$8)",
                                  "X=(S$9)",
                                  "+=λm.λn.λf.λx.((m$f)$((n$f)$x))",
                                  "*=λm.λn.λf.λx.((m$(n$f))$x)",
                                  "-=λm.λn.((n$P)$m)",
                                  "^=λm.λn.λf.λx.(((n$m)$f)$x)",
                                  "⊤=λa.λb.a",
                                  "T=λa.λb.a",
                                  "⊥=λa.λb.b",
                                  "F=λa.λb.b",
                                  "&=λp.λq.((p$q)$p)",
                                  "|=λp.λq.((p$p)$q)",
                                  "~=λp.((p$⊥)$⊤)",
                                  "Z=λn.((n$λx.⊥)$⊤)",
                                  "≤=λm.λn.(Z$((-$m)$n))",
                                  "==λm.λn.((&$((≤$m)$n))$((≤$n)$m))"];

fn main() {

    let stdin = io::stdin();
    let mut s = String::with_capacity(128);
    let mut g: HashMap<char, Expr> = HashMap::new();

    for line in &STD {
        eval(&line, &mut g);
    }

    while let Ok(x) = stdin.lock().read_line(&mut s) {

        if x == 0 {
            break;
        }

        if x == 1 {
            // this is going to be \n
            continue;
        }

        eval(&s, &mut g);

        s.clear();
    }
}

fn eval(s: &str, g: &mut HashMap<char, Expr>) {
    let mut var = '_';

    let src = if (&s).chars().nth(1).unwrap() == '=' {
        var = s.chars().next().unwrap();
        let (from, _) = (&s).char_indices().nth(2).unwrap();
        (&s[from..]).trim()
    } else {
        (&s[..]).trim()
    };

    match parse(src) {

        nom::IResult::Done(_, mut expr) => {

            for (k, v) in g.iter() {
                expr = sub(expr, *k, box v.clone());
            }

            let mut num_reductions = 1;
            let mut reduced = expr.clone().reduce();

            println!("{}", &expr);
            println!("----------------------------------------------------");
            println!("{}", reduced);

            while &reduced != &expr {

                expr = reduced;
                for (k, v) in g.iter() {
                    expr = sub(expr, *k, box v.clone());
                }

                num_reductions += 1;
                reduced = expr.clone().reduce();

                println!("({}) {}", num_reductions, &expr);
                println!("----------------------------------------------------");
                println!("{}", &reduced);
            }

            g.insert(var, reduced);
        }

        _ => println!("Invalid expr."),
    }
}

mod untyped {
    use std::fmt;
    #[derive(PartialEq, Eq, Clone, Debug)]
    pub enum Expr {
        Var(char),
        App(Box<Expr>, Box<Expr>),
        Lam(char, Box<Expr>),
    }

    impl fmt::Display for Expr {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match *self {
                Expr::Var(c) => write!(f, "{}", c),
                Expr::App(ref l, ref r) => write!(f, "({} $ {})", l, r),
                Expr::Lam(v, ref c) => write!(f, "λ{}.{}", v, c),
            }
        }
    }

    pub fn sub(e: Expr, v: char, r: Box<Expr>) -> Expr {
        match e {
            Expr::Var(s) => if s == v { *r } else { Expr::Var(s) },
            Expr::Lam(s, c) => {
                if s == v {
                    Expr::Lam(s, c)
                } else {
                    Expr::Lam(s, box sub(*c, v, r))
                }
            }
            Expr::App(s, t) => Expr::App(box sub(*s, v, r.clone()), box sub(*t, v, r)),
        }
    }

    impl Expr {
        pub fn reduce(self) -> Self {
            match self {
                Expr::App(box s, box t) => {
                    match s {
                        Expr::Lam(v, e) => sub(*e, v, box t),
                        s => Expr::App(box s.reduce(), box t.reduce()),
                    }
                }
                Expr::Lam(s, box c) => Expr::Lam(s, box c.reduce()),
                Expr::Var(s) => Expr::Var(s),
            }
        }
    }

    named!(pub parse(&str) -> Expr,
           alt!(lambda |
               delimited!(tag_s!("("), do_parse!(
                   l: parse >>
                       ws!(tag_s!("$"))  >>
                       r: parse >>
                       (Expr::App(box l, box r))
               ),
                          tag_s!(")")) |
               var));

    named!(lambda(&str) -> Expr, do_parse!(
        char!('λ') >>
            v: map!(take_s!(1), |x: &str| x.chars().next().unwrap()) >>
            char!('.') >>
            e: parse >>
            (Expr::Lam(v, box e))));

    named!(var(&str) -> Expr, map!(map!(take_s!(1), |x: &str| x.chars().next().unwrap()), |x| Expr::Var(x)));
}
