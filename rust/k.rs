// todos: advs, add C (control) token type, verbs, reverse verb lists on fetch,
// refactor ev to return last value on the stack, for use with @
// kv
// done verbs: + - * !: #: , =
// todo verbs: ! # =: @ $ & | < > ^ ` ~ ? % .
// done adverbs: /: \: ' / :
// todo adverbs: \ ':
#![feature(slice_patterns, advanced_slice_patterns, custom_attribute)]
#![feature(conservative_impl_trait, box_syntax, box_patterns)]
#![allow(unused_variables, dead_code, unused_attributes)]
#![deny(unreachable_patterns)]

use std::cmp;
use std::collections::HashMap;
use std::io::{self, Write};
use std::rc::Rc;
use std::str::FromStr;

type I = i64;
type F = f64;
type S = usize;
type B = bool;

#[derive(Clone, PartialOrd, PartialEq, Debug)]
enum K0<T> {
    A(T),
    L(Vec<T>),
}

type K<T> = Rc<K0<T>>;

#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Debug)]
enum Tok<'a> {
    V(&'a str),
    N(&'a str),
    A(&'a str),
}

trait KT: PartialEq + PartialOrd + Clone {}
impl KT for B {}
impl KT for I {}
impl KT for char {}
impl KT for F {}
impl<T: KT> KT for K0<T> {}
impl KT for KA {} // int char mix err
impl KT for T {}

#[derive(Clone, PartialOrd, PartialEq, Debug)]
enum KA {
    KB(K<B>),
    KI(K<I>),
    KC(K<char>),
    KF(K<F>),
    KK(K<KA>),
    KE(&'static str),
    KL(K<T>),
    // only allow pointfree chains in variables for now
    KZ,
}

impl<T: KT> K0<T> {
    fn len(&self) -> S {
        match *self {
            K0::A(_) => 1,
            K0::L(ref v) => v.len(),
        }
    }
    fn i(&self) -> KIt<T> {
        KIt(self, 0)
    }
    fn a(&self) -> bool {
        match *self {
            K0::A(_) => true,
            K0::L(_) => false,
        }
    }
}

struct KIt<'a, T: 'a + KT>(&'a K0<T>, S);

impl<'a, T: 'a + KT> Iterator for KIt<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        let KIt(k0, ref mut ix) = *self;
        match *k0 {
            K0::A(ref a) => {
                if *ix == 0 {
                    *ix = 1;
                    Some(a)
                } else {
                    None
                }
            }

            K0::L(ref v) => {
                if *ix >= v.len() {
                    None
                } else {
                    *ix += 1;
                    Some(&v[*ix - 1])
                }
            }
        }
    }
}



#[derive(Debug, Clone)]
struct KAIt<'a>(&'a KA, S);

impl<'a> Iterator for KAIt<'a> {
    type Item = KA;
    #[rustfmt_skip]
    fn next(&mut self) -> Option<KA>
    {
        let KAIt(ka, ref mut ix) = *self;
        let x = *ix;
        match *ka
            {
                KA::KB(ref k0) => {
                    *ix += 1;
                    k0.i().nth(x).map(|x| KA::KB(mk(0, x.clone())))
                },
                KA::KC(ref k0) => {
                    *ix += 1;
                    k0.i().nth(x).map(|x| KA::KC(mk(0, x.clone())))
                },
                KA::KK(ref k0) => {
                    *ix += 1;
                    k0.i().nth(x).map(|x| x.clone())
                },
                KA::KF(ref k0) => {
                    *ix += 1;
                    k0.i().nth(x).map(|x| KA::KF(mk(0, x.clone())))
                },
                KA::KI(ref k0) => {
                    *ix += 1;
                    k0.i().nth(x).map(|x| KA::KI(mk(0, x.clone())))
                },
                KA::KL(ref k0) => {
                    *ix += 1;
                    k0.i().nth(x).map(|x| KA::KL(mk(0, x.clone())))
                },
                _ => None,
            }
    }
}

impl KA {
    fn len(&self) -> S {
        match *self {
            KA::KB(ref kb) => kb.len(),
            KA::KI(ref ki) => ki.len(),
            KA::KC(ref kc) => kc.len(),
            KA::KF(ref kf) => kf.len(),
            KA::KK(ref kk) => kk.len(),
            KA::KL(ref kl) => kl.len(),
            KA::KE(_) => 0,
            KA::KZ => 0,
        }
    }

    fn a(&self) -> bool {
        match *self {
            KA::KB(ref k) => k.a(),
            KA::KI(ref k) => k.a(),
            KA::KC(ref k) => k.a(),
            KA::KF(ref k) => k.a(),
            KA::KK(ref k) => k.a(),
            KA::KL(ref k) => k.a(),
            KA::KE(_) => false,
            KA::KZ => false,
        }
    }

    fn i<'a>(&'a self) -> KAIt<'a> {
        KAIt(self, 0)
    }
}

fn wr<T: KT>(v: Vec<T>) -> K<T> {
    if v.len() == 1 {
        let x: T = v[0].clone();
        mk(0, x)
    } else {
        Rc::new(K0::L(v))
    }
}

fn mk<T: KT>(n: S, d: T) -> K<T> {
    Rc::new(if n == 0 { K0::A(d) } else { K0::L(vec![d; n]) })
}
// add
fn dplus(l: KA, r: KA) -> KA {
    match (l, r) {
        (KA::KI(li), KA::KI(ri)) => {
            if li.len() == ri.len() {
                rz(KA::KI(wr(li.i().zip(ri.i()).map(|(x, y)| x + y).collect())))
            } else if li.len() == 0 {
                rz(eachr(&dplus, KA::KI(li), KA::KI(ri)))
            } else if ri.len() == 0 {
                rz(eachl(&dplus, KA::KI(li), KA::KI(ri)))
            } else {
                KA::KE("rank")
            }
        }
        (KA::KF(lf), KA::KF(rf)) => {
            if lf.len() == rf.len() {
                rz(KA::KF(wr(lf.i().zip(rf.i()).map(|(x, y)| x + y).collect())))
            } else {
                KA::KE("rank")
            }
        }
        (KA::KK(lk), KA::KK(rk)) => eachb(&dplus, KA::KK(lk), KA::KK(rk)),
        _ => KA::KE("type"),
    }
}
// verbs
// flip
fn mplus(r: KA) -> KA {
    let len = r.i().map(|x| x.len()).fold(0, cmp::max);
    if len == 0 {
        KA::KK(wr(r.i().collect()))
    } else {
        let mut vs = vec![Vec::new(); len];
        for (j, k) in r.i().enumerate() {
            for (i, kk) in k.i().enumerate() {
                vs[i].push(kk);
            }
        }

        rz(KA::KK(wr(vs.into_iter().map(|x| rz(KA::KK(wr(x)))).collect())))
    }
}
// sub
fn dmin(l: KA, r: KA) -> KA {
    match (l, r) {
        (KA::KI(li), KA::KI(ri)) => {
            if li.len() == ri.len() {
                KA::KI(wr(li.i().zip(ri.i()).map(|(x, y)| x - y).collect()))
            } else {
                KA::KE("rank")
            }
        }
        (KA::KF(lf), KA::KF(rf)) => {
            if lf.len() == rf.len() {
                KA::KF(wr(lf.i().zip(rf.i()).map(|(x, y)| x - y).collect()))
            } else {
                KA::KE("rank")
            }
        }
        (KA::KK(lk), KA::KK(rk)) => eachb(&dplus, KA::KK(lk), KA::KK(rk)),
        _ => KA::KE("type"),
    }
}
// neg
fn mmin(r: KA) -> KA {
    match &r {
        &KA::KI(_) => dstar(KA::KI(mk(0, -1)), r),
        &KA::KF(_) => dstar(KA::KF(mk(0, -1f64)), r),
        &KA::KK(_) => each(&mmin, r),
        _ => KA::KE("type"),
    }
}

// first
fn mstar(r: KA) -> KA {
    r.i().next().map(|x| x.clone()).unwrap_or(KA::KZ)
}

// mul
fn dstar(l: KA, r: KA) -> KA {
    match (l, r) {
        (KA::KI(li), KA::KI(ri)) => {
            if li.len() == ri.len() {
                KA::KI(wr(li.i().zip(ri.i()).map(|(x, y)| x * y).collect()))
            } else {
                KA::KE("rank")
            }
        }
        (KA::KF(lf), KA::KF(rf)) => {
            if lf.len() == rf.len() {
                KA::KF(wr(lf.i().zip(rf.i()).map(|(x, y)| x * y).collect()))
            } else {
                KA::KE("rank")
            }
        }
        (KA::KK(lk), KA::KK(rk)) => eachb(&dplus, KA::KK(lk), KA::KK(rk)),
        _ => KA::KE("type"),
    }
}

// til
fn mexcl(r: KA) -> KA {
    let is_atom = r.a();
    if is_atom {
        match r {
            KA::KI(ref ki) => {
                let kp: &K0<I> = ki;
                match *kp {
                    K0::A(ref a) => KA::KI(wr((0..(*a as S)).map(|x| x as I).collect())),
                    K0::L(_) => KA::KE("???"),
                }
            }
            _ => KA::KE("type"),
        }
    } else {
        each(&mexcl, r)
    }
}
// map
fn dexcl(l: KA, r: KA) -> KA {
    KA::KE("nyi")
}
// shape
fn dhash(l: KA, r: KA) -> KA {
    match l {
        KA::KI(ki) => {
            match *ki {
                K0::A(ref x) => rz(KA::KK(wr(r.i().cycle().take(*x as S).collect()))),
                K0::L(ref v) => KA::KE("nyi"),
            }
        }

        _ => KA::KE("LHS of # must be integral"),
    }
}
// count
fn mhash(r: KA) -> KA {
    KA::KI(mk(0, r.len() as I))
}
// join
fn dcomm(l: KA, r: KA) -> KA {
    match (l, r) {
        (KA::KI(li), KA::KI(ri)) => KA::KI(wr(li.i().cloned().chain(ri.i().cloned()).collect())),
        (KA::KC(lc), KA::KC(rc)) => KA::KC(wr(lc.i().cloned().chain(rc.i().cloned()).collect())),
        (KA::KF(lf), KA::KF(rf)) => KA::KF(wr(lf.i().cloned().chain(rf.i().cloned()).collect())),
        (KA::KK(lk), KA::KK(rk)) => KA::KK(wr(lk.i().cloned().chain(rk.i().cloned()).collect())),
        (KA::KE(e), _) | (_, KA::KE(e)) => KA::KE(e),
        (KA::KK(lk), ra) => dcomm(KA::KK(lk), mcomm(ra)),
        (la, KA::KK(rk)) => dcomm(mcomm(la), KA::KK(rk)),
        (l, r) => KA::KK(wr(vec![l, r])),
    }
}
// enlist
fn mcomm(r: KA) -> KA {
    KA::KK(mk(0, r))
}
// eq
fn deq(l: KA, r: KA) -> KA {
    match (l.a(), r.a()) {
        (true, true) => KA::KB(mk(0, l == r)),
        (true, false) => rz(KA::KB(wr(r.i().map(|x| l == x).collect()))),
        (false, true) => rz(KA::KB(wr(l.i().map(|x| r == x).collect()))),
        (false, false) => eachb(&deq, l, r),
    }
}
// group
fn meq(r: KA) -> KA {
    KA::KE("nyi")
}

// apply
fn dat(l: KA, r: KA) -> KA {
    KA::KE("nyi")
}
// type
fn mat(r: KA) -> KA {
    KA::KE("nyi")
}
// str
fn mdol(r: KA) -> KA {
    KA::KC(wr(format!("{:?}", r).chars().collect()))
}

// inner product?
fn ddol(l: KA, r: KA) -> KA {
    KA::KE("nyi")
}


fn eachb<F: Fn(KA, KA) -> KA>(f: &F, l: KA, r: KA) -> KA {
    rz(KA::KK(wr(l.i()
                     .zip(r.i())
                     .map(|(x, y)| f(x.clone(), y.clone()))
                     .collect())))
}

fn each<F: Fn(KA) -> KA>(f: &F, k: KA) -> KA {
    rz(KA::KK(wr(k.i().map(|x| f(x.clone())).collect())))
}

fn eachr<F: Fn(KA, KA) -> KA>(f: &F, l: KA, r: KA) -> KA {
    (KA::KK(wr(r.i().map(|x| f(l.clone(), x.clone())).collect())))
}

fn eachl<F: Fn(KA, KA) -> KA>(f: &F, l: KA, r: KA) -> KA {
    (KA::KK(wr(l.i().map(|x| f(x.clone(), r.clone())).collect())))
}

fn fold0<F: Fn(KA, KA) -> KA>(f: &F, k: KA) -> KA {
    let mut it = k.i();
    let mut acc = if let Some(ka) = it.next() {
        ka
    } else {
        return KA::KZ;
    };
    for ka in it {
        acc = f(acc, ka)
    }

    acc
}

fn fold<F: Fn(KA, KA) -> KA>(f: &F, init: KA, k: KA) -> KA {
    let mut acc = init;
    for ka in k.i() {
        acc = f(acc, ka);
    }
    acc
}


// verbs:+!*(/:)#,-(\:)/\@$=
fn lex<'a>(s: &'a str) -> Vec<Tok<'a>> {
    // lexer
    #[derive(Copy, Clone, PartialEq, Eq, Debug)]
    enum LM {
        N,
        A,
        V,
        U,
    };
    struct L<'a> {
        s: &'a str,
        m: LM,
    }
    impl<'a> Iterator for L<'a> {
        type Item = Tok<'a>;
        fn next(&mut self) -> Option<Tok<'a>> {
            let verb_str = "#_!+-*%$=,~^?:@";
            let mut iter = self.s.chars().enumerate().peekable();
            let mut m = self.m;
            loop {
                println!("Considering {:?}: {:?}", m, iter.peek());
                match (m, iter.peek()) {
                    // end of str
                    (LM::N, None) => {
                        let token = self.s;
                        self.s = "";
                        self.m = LM::U;
                        return Some(Tok::N(token));
                    }
                    (LM::A, None) => {
                        let token = self.s;
                        self.s = "";
                        self.m = LM::U;
                        return Some(Tok::A(token));
                    }

                    (LM::V, None) => {
                        let token = self.s;
                        self.s = "";
                        self.m = LM::U;
                        return Some(Tok::V(token));
                    }

                    (LM::U, None) => return None,

                    // whitespace
                    (LM::N, Some(&(i, c))) if c.is_whitespace() => {
                        let _ = iter.next().unwrap();
                        let (token, text) = self.s.split_at(i);
                        self.s = text.trim();
                        self.m = LM::U;
                        return Some(Tok::N(token));
                    }
                    (LM::A, Some(&(i, c))) if c.is_whitespace() => {
                        let _ = iter.next().unwrap();
                        let (token, text) = self.s.split_at(i);
                        self.s = text.trim();
                        self.m = LM::U;
                        if !token.is_empty() {
                            return Some(Tok::A(token));
                        } else {
                            m = LM::U; // no spaces between verb and adverb
                        }
                    }
                    (LM::U, Some(&(i, c))) if c.is_whitespace() => {
                        let _ = iter.next().unwrap();
                        self.s = &self.s[(i + 1)..];
                    }

                    // end of noun
                    (LM::N, Some(&(i, c))) if verb_str.contains(c) => {
                        let (token, text) = self.s.split_at(i);
                        self.s = text;
                        self.m = LM::U;
                        return Some(Tok::N(token));
                    }

                    // start of adv
                    (LM::A, Some(&(0, '/'))) |
                    (LM::A, Some(&(0, '\\'))) |
                    (LM::A, Some(&(0, '\''))) => {
                        let _ = iter.next().unwrap();
                    }

                    // end of adv with suffix
                    // ' is also a suffix, but is caught via other rules
                    // as it can be a distinct token
                    (LM::A, Some(&(_, ':'))) => {
                        let (i, _) = iter.next().unwrap();
                        let (token, text) = self.s.split_at(i + 1);
                        self.s = text;
                        self.m = LM::U;
                        return Some(Tok::A(token));
                    }

                    // end of adv without suffix
                    (LM::A, Some(&(i, _))) => {
                        let (token, text) = self.s.split_at(i);
                        self.s = text;
                        self.m = LM::U;
                        m = LM::U;
                        if !token.is_empty() {
                            return Some(Tok::A(token));
                        }
                    }
                    // continuation of noun
                    (LM::N, Some(_)) => {
                        let _ = iter.next().unwrap();
                    }

                    // mode selection
                    // verbs are only 1 char except for ::
                    (LM::U, Some(&(_, ':'))) => {
                        m = LM::V;
                        let _ = iter.next().unwrap();
                    }

                    // pick up ::
                    (LM::V, Some(&(i, ':'))) => {
                        let _ = iter.next().unwrap();
                        let (token, text) = self.s.split_at(i + 1);
                        self.s = text;
                        self.m = LM::A;
                        return Some(Tok::V(token));
                    }

                    (LM::V, Some(&(i, _))) => {
                        let (token, text) = self.s.split_at(i);
                        self.s = text;
                        self.m = LM::A;
                        return Some(Tok::V(token));
                    }

                    (LM::U, Some(&(i, c))) if verb_str.contains(c) => {
                        let _ = iter.next().unwrap();
                        let (token, text) = self.s.split_at(i + 1);
                        self.s = text;
                        self.m = LM::A;
                        return Some(Tok::V(token));
                    }

                    (LM::U, Some(&(_, c))) if "/\\'".contains(c) => {
                        m = LM::A;
                        let _ = iter.next().unwrap();
                    }
                    (LM::U, Some(_)) => {
                        m = LM::N;
                        let _ = iter.next().unwrap();
                    }
                }
            }
        }
    }

    (L {
             s: s.trim(),
             m: LM::U,
         })
        .collect()
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
enum T {
    N(String, Vec<String>),
    V(String, Vec<String>),
    K(KA),
}
// just coalesces adverbs
fn pa<'a>(s: &'a str) -> Vec<T> {
    let mut adv = Vec::new();
    let mut ter = Vec::new();
    let mut tok = lex(s);
    while let Some(t) = tok.pop() {
        match t {
            Tok::N(n) => {
                adv.reverse();
                ter.push(T::N(n.to_owned(), adv));
                adv = Vec::new();
            }
            Tok::V(v) => {
                adv.reverse();
                ter.push(T::V(v.to_owned(), adv));
                adv = Vec::new();
            }
            Tok::A(a) => adv.push(a.to_owned()),
        }
    }

    ter.reverse();
    ter
}

fn pr(k: T) {
    println!("{:?}", k)
}

fn ca(s: &str, p: S) -> char {
    s.as_bytes()[p] as char
}

fn nn(n: &str, a: &[&str], g: &HashMap<String, KA>, l: &HashMap<String, KA>) -> KA {
    if n.is_empty() {
        KA::KE("noun?")
    } else if ca(n, 0).is_digit(10) {
        if n.contains(".") {
            match F::from_str(n) {
                Ok(x) => KA::KF(mk(0, x)),
                Err(_) => KA::KE("float?"),
            }
        } else {
            match I::from_str(n) {
                Ok(x) => KA::KI(mk(0, x)),
                Err(_) => KA::KE("int?"),
            }
        }
    } else if (ca(n, 0) == '"') && (ca(n, n.len() - 1) == '"') {
        KA::KC(wr((&n[1..n.len() - 1]).chars().collect()))
    } else {
        l.get(n)
            .or(g.get(n))
            .map(|x| x.clone())
            .unwrap_or(KA::KE("var?"))
    }
}

// verbs #_!+-*%$=,~^?:
// adverbs /\/:\:'':
fn mon<'a>(verb: &'a str, advs: &[&'a str], r: KA) -> KA {
    // sometimes a monadic positioned verb can be a dyad, if it
    // has /, /:, \, \: as an adverb

    if advs.contains(&"/") || advs.contains(&"\\") {
        println!("mon dyad");
        match verb {
            "+" => dadv(&dplus, advs, KA::KZ, r),
            "!" => dadv(&dexcl, advs, KA::KZ, r),
            "*" => dadv(&dstar, advs, KA::KZ, r),
            "#" => dadv(&dhash, advs, KA::KZ, r),
            "," => dadv(&dcomm, advs, KA::KZ, r),
            "@" => dadv(&dat, advs, KA::KZ, r),
            "$" => dadv(&ddol, advs, KA::KZ, r),
            "=" => dadv(&deq, advs, KA::KZ, r),
            "-" => dadv(&dmin, advs, KA::KZ, r),
            _ => KA::KE("nyi"),
        }
    } else {
        println!("mon monad");
        match verb {
            "+" => madv(&mplus, advs, r),
            "!" => madv(&mexcl, advs, r),
            "*" => madv(&mstar, advs, r),
            "#" => madv(&mhash, advs, r),
            "," => madv(&mcomm, advs, r),
            "@" => madv(&mat, advs, r),
            "$" => madv(&mdol, advs, r),
            "=" => madv(&meq, advs, r),
            "-" => madv(&mmin, advs, r),
            _ => KA::KE("nyi"),
        }
    }
}

fn dya<'a>(verb: &'a str, advs: &[&'a str], l: KA, r: KA) -> KA {
    if advs.contains(&":") {
        match verb {
            "+" => madv(&mplus, advs, r),
            "!" => madv(&mexcl, advs, r),
            "*" => madv(&mstar, advs, r),
            "#" => madv(&mhash, advs, r),
            "," => madv(&mcomm, advs, r),
            "@" => madv(&mat, advs, r),
            "$" => madv(&mdol, advs, r),
            "=" => madv(&meq, advs, r),
            "-" => madv(&mmin, advs, r),
            _ => KA::KE("nyi"),
        }
    } else {
        match verb {
            "+" => dadv(&dplus, advs, l, r),
            "!" => dadv(&dexcl, advs, l, r),
            "*" => dadv(&dstar, advs, l, r),
            "#" => dadv(&dhash, advs, l, r),
            "," => dadv(&dcomm, advs, l, r),
            "@" => dadv(&dat, advs, l, r),
            "$" => dadv(&ddol, advs, l, r),
            "=" => dadv(&deq, advs, l, r),
            "-" => dadv(&dmin, advs, l, r),
            _ => KA::KE("nyi"),
        }
    }
}

fn madv<'a, F: Fn(KA) -> KA>(f: &F, advs: &[&'a str], k: KA) -> KA {
    match advs {
        &["'"] => each(f, k),
        &["'", ref advs..] => {
            match k {
                KA::KK(ks) => rz(KA::KK(wr(ks.i().map(|k| madv(f, advs, k.clone())).collect()))),
                KA::KI(ki) => {
                    rz(KA::KK(wr(ki.i()
                                     .map(|i| madv(f, advs, f(KA::KI(mk(0, *i)))))
                                     .collect())))
                }
                KA::KC(kc) => {
                    rz(KA::KK(wr(kc.i()
                                     .map(|i| madv(f, advs, f(KA::KC(mk(0, *i)))))
                                     .collect())))
                }
                KA::KF(kf) => {
                    rz(KA::KK(wr(kf.i()
                                     .map(|i| madv(f, advs, f(KA::KF(mk(0, *i)))))
                                     .collect())))
                }
                KA::KB(kb) => {
                    rz(KA::KK(wr(kb.i()
                                     .map(|i| madv(f, advs, f(KA::KB(mk(0, *i)))))
                                     .collect())))
                }
                _ => k, // KE,KZ
            }
        }
        &[":", ref advs..] => madv(f, advs, k), // ignore : as it is a parsing artefact
        &[] => f(k),
        _ => KA::KE("adv?"),
    }
}

// if / or \ are at the top of adv, then ignore l, and just f(/|\)r
fn dadv<'a, F: Fn(KA, KA) -> KA>(f: &F, advs: &[&'a str], l: KA, r: KA) -> KA {
    match advs {
        &[] => f(l, r),

        &["'"] => eachb(f, l, r),

        &["'", ref advs..] => {
            rz(KA::KK(wr(l.i()
                             .zip(r.i())
                             .map(|(lk, rk)| dadv(f, advs, lk.clone(), rk.clone()))
                             .collect())))
        }

        &["/:"] => rz(eachr(f, l, r)),
        &["/:", ref advs..] => {
            rz(KA::KK(wr(r.i()
                             .map(|x| dadv(f, advs, l.clone(), x.clone()))
                             .collect())))
        }
        &["\\:"] => rz(eachl(f, l, r)),
        &["\\:", ref advs..] => {
            rz(KA::KK(wr(l.i()
                             .map(|x| dadv(f, advs, x.clone(), l.clone()))
                             .collect())))
        }
        &["/"] => {
            if let KA::KZ = l {
                rz(fold0(f, r))
            } else {
                rz(fold(f, l, r))
            }
        }

        _ => KA::KE("dadv nyi"),
    }
}

fn rz(k: KA) -> KA {
    #[derive(Debug)]
    enum ST {
        K(Vec<KA>),
        C(Vec<char>),
        B(Vec<bool>),
        I(Vec<I>),
        F(Vec<F>),
        L(Vec<T>),
        Z,
    }

    match k.clone() {
        KA::KK(ref kk) => {
            let mut st = match kk.i().next() {
                Some(&KA::KB(_)) => ST::B(Vec::new()),
                Some(&KA::KC(_)) => ST::C(Vec::new()),
                Some(&KA::KF(_)) => ST::F(Vec::new()),
                Some(&KA::KI(_)) => ST::I(Vec::new()),
                Some(&KA::KL(_)) => ST::L(Vec::new()),
                Some(&KA::KK(_)) => ST::K(Vec::new()),
                _ => return k,
            };

            for ka in kk.i() {
                match (&mut st, ka) {
                    (&mut ST::K(ref mut v), &KA::KK(ref k0)) => {
                        if let K0::A(ref a) = **k0 {
                            v.push(a.clone());
                        } else {
                            return k;
                        }
                    }
                    (&mut ST::I(ref mut v), &KA::KI(ref k0)) => {
                        if let K0::A(ref a) = **k0 {
                            v.push(*a);
                        } else {
                            return k;
                        }
                    }
                    (&mut ST::C(ref mut v), &KA::KC(ref k0)) => {
                        if let K0::A(ref a) = **k0 {
                            v.push(*a);
                        } else {
                            return k;
                        }
                    }
                    (&mut ST::B(ref mut v), &KA::KB(ref k0)) => {
                        if let K0::A(ref a) = **k0 {
                            v.push(*a);
                        } else {
                            return k;
                        }
                    }
                    (&mut ST::F(ref mut v), &KA::KF(ref k0)) => {
                        if let K0::A(ref a) = **k0 {
                            v.push(*a);
                        } else {
                            return k;
                        }
                    }
                    (&mut ST::L(ref mut v), &KA::KL(ref k0)) => {
                        if let K0::A(ref a) = **k0 {
                            v.push(a.clone());
                        } else {
                            return k;
                        }
                    }
                    (&mut ST::Z, _) => return KA::KZ,
                    _ => return KA::KE("nyi"),
                }
            }

            match st {
                ST::K(k) => KA::KK(wr(k)),
                ST::C(k) => KA::KC(wr(k)),
                ST::B(k) => KA::KB(wr(k)),
                ST::I(k) => KA::KI(wr(k)),
                ST::F(k) => KA::KF(wr(k)),
                ST::L(k) => KA::KL(wr(k)),
                ST::Z => KA::KZ,
            }
        }
        k => k,
    }
}

fn p<T: KT>(x: T, k: K<T>) -> K<T> {
    match *k {
        K0::A(ref a) => wr(vec![a.clone(), x]),
        K0::L(ref v) => {
            let mut v2 = v.clone();
            v2.push(x);
            wr(v2)
        }
    }
}

fn ev(mut ter: Vec<T>, g: &mut HashMap<String, KA>, v: &mut HashMap<String, KA>) {
    loop {
        println!("{:?}", ter);
        let (t2, t1, t0) = (ter.pop(), ter.pop(), ter.pop());
        match (t0, t1, t2)
        {
            (None, None, None) => return, // ZZZ
            (_, _, Some(T::K(KA::KE(e)))) => return pr(T::K(KA::KE(e))), // xxe
            (t0, Some(T::V(v1, a1)), Some(T::K(k2))) => // xvk
            {
                match t0
                {
                    Some(T::V(v0, a0)) => // vvk
                    {
                        if let KA::KL(kl) = k2
                        {
                            let kl = p(T::V(v1, a1), kl);
                            ter.push(T::V(v0, a0));
                            ter.push(T::K(KA::KL(kl)));
                        } else {
                            let a1s: Vec<&str> = a1.iter().map(AsRef::as_ref).collect();
                            ter.push(T::V(v0, a0));
                            ter.push(T::K(mon(&v1, &a1s, k2)));
                        }
                    },
                    Some(T::N(n0, a0)) => // nvk
                    {
                        if v1 == ":"
                        {
                            if a1.is_empty() && a0.is_empty()
                            {
                                println!("L {:?} := {:?}", n0, k2);
                                v.insert(n0, k2.clone());
                                ter.push(T::K(k2));
                            } else {
                                ter.push(T::K(KA::KE("adv!")))
                            }
                        } else if v1 == "::"
                        {
                            if a1.is_empty() && a0.is_empty()
                            {
                                println!("G {:?} := {:?}", n0, k2);
                                g.insert(n0, k2.clone());
                                ter.push(T::K(k2));
                            } else {
                                ter.push(T::K(KA::KE("adv!")))
                            }
                        }
                        else
                        {
                            if let KA::KL(kl) = k2
                            {
                                let kl = p(T::V(v1, a1), kl);
                                ter.push(T::N(n0, a0));
                                ter.push(T::K(KA::KL(kl)));
                            }
                            else
                            {
                                let kn0 =
                                {
                                    let a0s: Vec<&str> = a0.iter().map(AsRef::as_ref).collect();
                                    nn(&n0, &a0s[..], g, v)
                                };

                                // if noun is a verb chain, push the verb chain before continuing
                                // could lead to a recursion error for x::vx?
                                if let KA::KL(verbs) = kn0
                                {
                                    let mut vt: Vec<T> = verbs.i().cloned().collect();
                                    vt.reverse();
                                    ter.append(&mut vt);
                                    ter.push(T::V(v1, a1));
                                    ter.push(T::K(k2));
                                }
                                else
                                {
                                    let a1s: Vec<&str> = a1.iter().map(AsRef::as_ref).collect();
                                    ter.push(T::K(dya(&v1, &a1s, kn0, k2)));
                                }
                            }
                        }
                    },
                    Some(T::K(k0)) => // kvk
                    {
                        if let KA::KL(kl) = k2
                        {
                            let kl = p(T::V(v1, a1), kl);
                            ter.push(T::K(k0));
                            ter.push(T::K(KA::KL(kl)));
                        } else {
                            let a1s: Vec<&str> = a1.iter().map(AsRef::as_ref).collect();
                            ter.push(T::K(dya(&v1, &a1s, k0, k2)));
                        }
                    },
                    None => // Zvk
                    {
                        if let KA::KL(kl) = k2
                        {
                            let kl = p(T::V(v1, a1), kl);
                            ter.push(T::K(KA::KL(kl)));
                        } else {
                            let a1s: Vec<&str> = a1.iter().map(AsRef::as_ref).collect();
                            ter.push(T::K(mon(&v1, &a1s, k2)));
                        }
                    },
                }
            },
            (t0, Some(T::K(k1)), Some(T::K(k2))) => // xkk
            {
                if let Some(t0) = t0
                {
                    ter.push(t0);
                }

                // flatten KL onto the command stack

                if let KA::KL(verbs) = k1
                {
                    let mut vt: Vec<T> = verbs.i().cloned().collect();
                    vt.reverse();
                    ter.append(&mut vt);
                }
                else
                {
                    ter.push(T::K(dya("@", &[], k1, k2))); // @ can handle this
                }
            },
            // noun replacement rules
            (t0, t1, Some(T::N(n2, a2))) => // xxn
            {
                if let Some(t0) = t0
                {
                    ter.push(t0);
                }
                if let Some(t1) = t1
                {
                    ter.push(t1);
                }
                let a2s: Vec<&str> = a2.iter().map(AsRef::as_ref).collect();
                ter.push(T::K(nn(&n2, &a2s[..], g, v)));
            },
            (t0, Some(T::N(n1, a1)), t2) => // xnx
            {
                if let Some(t0) = t0
                {
                    ter.push(t0);
                }
                let a1s: Vec<&str> = a1.iter().map(AsRef::as_ref).collect();
                ter.push(T::K(nn(&n1, &a1s[..], g, v)));
                if let Some(t2) = t2
                {
                    ter.push(t2);
                }
            },
            (t0, t1, Some(T::V(v2, a2))) => // xxv
            {
                if let Some(t0) = t0
                {
                    ter.push(t0);
                }
                if let Some(t1) = t1
                {
                    ter.push(t1);
                }
                ter.push(T::K(KA::KL(mk(0, T::V(v2, a2)))));
            },
            (Some(T::N(n0, a0)), t1, t2) => // nxx
            {
                let a0s: Vec<&str> = a0.iter()
                    .map(AsRef::as_ref).collect();
                ter.push(T::K(nn(&n0, &a0s[..], g, v)));
                if let Some(t1) = t1
                {
                    ter.push(t1);
                }
                if let Some(t2) = t2
                {
                    ter.push(t2);
                }
            },
            (None, None, Some(t2)) => return pr(t2), // ZZx
            (t0, t1, t2) => // xxx
            {
                println!("DEBUG:\n t0: {:?}\nt1: {:?}\nt1: {:?}", t0, t1, t2);
            },
        }
    }
}

fn main() {
    let mut locs: HashMap<String, KA> = HashMap::new();
    let mut glob: HashMap<String, KA> = HashMap::new();
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut s = String::new();
    loop {
        print!("    ");
        stdout.flush().unwrap();
        s.clear();
        match stdin.read_line(&mut s) {
            Ok(0) => continue,
            Ok(_) => {
                match s.as_str() {
                    "\\\\\n" => return,
                    _ => (),
                }

                locs.clear();
                let ter = pa(&s);
                println!("{:?}", &ter);
                println!("G: {:?}", glob);
                ev(ter, &mut glob, &mut locs)
            }
            _ => return,
        }
    }
}
