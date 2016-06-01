#![feature(box_syntax, box_patterns)]
pub type Code = &[u64];

pub enum Bytecode
{
    Ret, 
    Pred,
    Lam, 
    Ap,
    Lit(u64), 
    Ty,
    Get,
    Put,
    Mut,
    New
}

pub fn compile(src : &str) -> Box<[u64]> {
    box [0;1]
}

// this should be good enough for λ calculus
enum Ast<'a> {
    LitStr(&'a str), // string literal
    Func(&'a str, Vec<&'a str>, Box<Ast<'a>>), // define a function with value of params->eval(ast)
    Defn(&'a str, Box<Ast<'a>>), // define a name with value of eval(ast)
    VarInt(&'a str, Option<i64>), // variables
    VarStr(&'a str, Option<&'a str>),
    VarFlt(&'a str, Option<&'a f64>),
    If(Box<Ast<'a>>, Box<Ast<'a>>, Box<Ast<'a>>),
    Raw(&'a str, Box<Ast<'a>>), // call raw (internal) function with param
    Call(Box<Ast<'a>>, Vec<Ast<'a>>) // call ast function with vec of params
}

fn parse<'a>(src: &'a str) -> Ast<'a> {
    LitStr("todo")
}
