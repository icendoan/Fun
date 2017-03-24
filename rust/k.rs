// todos: advs, (), rewrite lexer (again!)

#![feature(slice_patterns, advanced_slice_patterns, conservative_impl_trait, box_syntax, box_patterns)]
#![allow(dead_code, unused_variables)]
use std::collections::HashMap;
use std::io;
use std::rc::Rc;
use std::str::FromStr;
type I = i64;
type F = f64;
type S = usize;
type B = bool;
#[derive(Clone,PartialOrd,PartialEq,Debug)]
enum K0<T>
{
    A(T),
    L(Vec<T>),
}
type K<T> = Rc<K0<T>>;
#[derive(Clone,PartialOrd,Ord,PartialEq,Eq,Hash,Debug)]
enum Tok<'a>
{
    V(&'a str),
    N(&'a str),
    A(&'a str),
}
trait KT: PartialEq + PartialOrd + Clone
{
}
impl KT for B {}
impl KT for I {}
impl KT for char {}
impl KT for F {}
impl<T: KT> KT for K0<T> {}
impl KT for KA {} // int char mix err
#[derive(Clone,PartialOrd,PartialEq,Debug)]
enum KA
{
    KB(K<B>),
    KI(K<I>),
    KC(K<char>),
    KF(K<F>),
    KK(K<KA>),
    KE(&'static str),
    KZ,
}
impl<T: KT> K0<T>
{
    fn len(&self) -> S
    {
        match *self
        {
            K0::A(_) => 0,
            K0::L(ref v) => v.len(),
        }
    }
    fn i(&self) -> KIt<T>
    {
        KIt(self, 0)
    }
    fn a(&self) -> bool
    {
        match *self
        {
            K0::A(_) => true,
            K0::L(_) => false,
        }
    }
}
struct KIt<'a, T: 'a + KT>(&'a K0<T>, S);
impl<'a, T: 'a + KT> Iterator for KIt<'a, T>
{
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item>
    {
        let KIt(k0, ref mut ix) = *self;
        match *k0
        {
            K0::A(ref a) =>
            {
                if *ix == 0
                {
                    *ix = 1;
                    Some(a)
                }
                else
                {
                    None
                }
            },

            K0::L(ref v) =>
            {
                if *ix > v.len()
                {
                    None
                }
                else
                {
                    *ix += 1;
                    Some(&v[*ix - 1])
                }
            },
        }
    }
}

impl KA
{
    fn len(&self) -> S
    {
        match *self
        {
            KA::KB(ref kb) => kb.len(),
            KA::KI(ref ki) => ki.len(),
            KA::KC(ref kc) => kc.len(),
            KA::KF(ref kf) => kf.len(),
            KA::KK(ref kk) => kk.len(),
            KA::KE(_) => 0,
            KA::KZ => 0,
        }
    }

    fn a(&self) -> bool
    {
        match *self
        {
            KA::KB(ref k) => k.a(),
            KA::KI(ref k) => k.a(),
            KA::KC(ref k) => k.a(),
            KA::KF(ref k) => k.a(),
            KA::KK(ref k) => k.a(),
            KA::KE(_) => false,
            KA::KZ => false,
        }
    }
}
fn wr<T: KT>(v: Vec<T>) -> K<T>
{
    Rc::new(K0::L(v))
}
fn mk<T: KT>(n: S, d: T) -> K<T>
{
    Rc::new(if n == 0 { K0::A(d) } else { K0::L(vec![d;n]) })
}
/* add */
fn dplus(l: KA, r: KA) -> KA
{
    match (l, r)
    {
        (KA::KI(li), KA::KI(ri)) =>
        {
            if li.len() == ri.len()
            {
                KA::KI(wr(li.i()
                    .zip(ri.i())
                    .map(|(x, y)| x + y)
                    .collect()))
            }
            else
            {
                KA::KE("rank")
            }
        },
        (KA::KF(lf), KA::KF(rf)) =>
        {
            if lf.len() == rf.len()
            {
                KA::KF(wr(lf.i()
                    .zip(rf.i())
                    .map(|(x, y)| x + y)
                    .collect()))
            }
            else
            {
                KA::KE("rank")
            }
        },
        (KA::KK(lk), KA::KK(rk)) => eachb(dplus)(KA::KK(lk), KA::KK(rk)),
        _ => KA::KE("type"),
    }
}
/* verbs */
/* flip */
fn mplus(r: KA) -> KA
{
    KA::KE("nyi")
}
/* sub */
fn dmin(l: KA, r: KA) -> KA
{
    match (l, r)
    {
        (KA::KI(li), KA::KI(ri)) =>
        {
            if li.len() == ri.len()
            {
                KA::KI(wr(li.i()
                    .zip(ri.i())
                    .map(|(x, y)| x - y)
                    .collect()))
            }
            else
            {
                KA::KE("rank")
            }
        },
        (KA::KF(lf), KA::KF(rf)) =>
        {
            if lf.len() == rf.len()
            {
                KA::KF(wr(lf.i()
                    .zip(rf.i())
                    .map(|(x, y)| x - y)
                    .collect()))
            }
            else
            {
                KA::KE("rank")
            }
        },
        (KA::KK(lk), KA::KK(rk)) => eachb(dplus)(KA::KK(lk), KA::KK(rk)),
        _ => KA::KE("type"),
    }
}
/* neg */
fn mmin(r: KA) -> KA
{
    match &r
    {
        &KA::KI(_) => dstar(KA::KI(mk(0, -1)), r),
        &KA::KF(_) => dstar(KA::KF(mk(0, -1f64)), r),
        &KA::KK(_) => eachr(mmin)(r),
        _ => KA::KE("type"),
    }
}

/* first */
fn mstar(r: KA) -> KA
{
    match r
    {
        KA::KI(ki) =>
        {
            let kp: &K0<I> = &ki;
            match *kp
            {
                K0::A(ref a) => KA::KI(mk(0, a.clone())),
                K0::L(ref v) =>
                {
                    if let Some(a) = v.get(0)
                    {
                        KA::KI(mk(0, a.clone()))
                    }
                    else
                    {
                        KA::KE("empty")
                    }
                },
            }
        },

        KA::KC(ki) =>
        {
            let kp: &K0<char> = &ki;
            match *kp
            {
                K0::A(ref a) => KA::KC(mk(0, a.clone())),
                K0::L(ref v) =>
                {
                    if let Some(a) = v.get(0)
                    {
                        KA::KC(mk(0, a.clone()))
                    }
                    else
                    {
                        KA::KE("empty")
                    }
                },
            }
        },

        KA::KF(ki) =>
        {
            let kp: &K0<F> = &ki;
            match *kp
            {
                K0::A(ref a) => KA::KF(mk(0, a.clone())),
                K0::L(ref v) =>
                {
                    if let Some(a) = v.get(0)
                    {
                        KA::KF(mk(0, a.clone()))
                    }
                    else
                    {
                        KA::KE("empty")
                    }
                },
            }
        },

        KA::KK(ki) =>
        {
            let kp: &K0<KA> = &ki;
            match *kp
            {
                K0::A(ref a) => KA::KK(mk(0, a.clone())),
                K0::L(ref v) =>
                {
                    if let Some(a) = v.get(0)
                    {
                        KA::KK(mk(0, a.clone()))
                    }
                    else
                    {
                        KA::KE("empty")
                    }
                },
            }
        },

        _ => KA::KE("nyi"),
    }
}

/* mul */
fn dstar(l: KA, r: KA) -> KA
{
    match (l, r)
    {
        (KA::KI(li), KA::KI(ri)) =>
        {
            if li.len() == ri.len()
            {
                KA::KI(wr(li.i()
                    .zip(ri.i())
                    .map(|(x, y)| x * y)
                    .collect()))
            }
            else
            {
                KA::KE("rank")
            }
        },
        (KA::KF(lf), KA::KF(rf)) =>
        {
            if lf.len() == rf.len()
            {
                KA::KF(wr(lf.i()
                    .zip(rf.i())
                    .map(|(x, y)| x * y)
                    .collect()))
            }
            else
            {
                KA::KE("rank")
            }
        },
        (KA::KK(lk), KA::KK(rk)) => eachb(dplus)(KA::KK(lk), KA::KK(rk)),
        _ => KA::KE("type"),
    }

}

/* til */
fn mexcl(r: KA) -> KA
{

    let is_atom = r.a();
    if is_atom
    {
        match r
        {
            KA::KI(ref ki) =>
            {
                let kp: &K0<I> = ki;
                match *kp
                {
                    K0::A(ref a) => KA::KI(wr((0..(*a as S)).map(|x| x as I).collect())),
                    K0::L(_) => KA::KE("???"),
                }
            },
            _ => KA::KE("type"),
        }
    }
    else
    {
        eachr(mexcl)(r)
    }
}
/* map */
fn dexcl(l: KA, r: KA) -> KA
{
    KA::KE("nyi")
}
/* shape */
fn dhash(l: KA, r: KA) -> KA
{
    KA::KE("nyi")
}
/* count */
fn mhash(r: KA) -> KA
{
    KA::KI(mk(0, r.len() as I))
}
/* join */
fn dcomm(l: KA, r: KA) -> KA
{
    match (l, r)
    {
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
/* enlist */
fn mcomm(r: KA) -> KA
{
    KA::KK(mk(0, r))
}
/* eq */
fn deq(l: KA, r: KA) -> KA
{
    KA::KE("nyi")
}
/* group */
fn meq(r: KA) -> KA
{
    KA::KE("nyi")
}

/* apply */
fn dat(l: KA, r: KA) -> KA
{
    KA::KE("nyi")
}
/* type */
fn mat(r: KA) -> KA
{
    KA::KE("nyi")
}
/* str */
fn mdol(r: KA) -> KA
{
    KA::KE("nyi")
}
/* if/cast/dot/mmul/
(c:bool atom)$a,b->a if c=t, b if c=f
(t:int atom)$(x:s) -> x:tassoc t
(t:int list)$(x:s) -> t$'x
(a:float matrix l n)$(b: float matrix n r)->float matrix l r
*/
fn ddol(l: KA, r: KA) -> KA
{
    KA::KE("nyi")
}


fn eachl<F: FnOnce(KA) -> KA>(f: F) -> impl FnOnce(KA) -> KA
{
    move |x| f(x)
}

fn eachr<F: FnOnce(KA) -> KA>(f: F) -> impl FnOnce(KA) -> KA
{
    move |x| f(x)
}

fn eachb<F: FnOnce(KA, KA) -> KA>(f: F) -> impl FnOnce(KA, KA) -> KA
{
    move |x, y| f(x, y)
}

fn scan<F: FnOnce(KA, KA) -> KA>(f: F) -> impl FnOnce(KA, KA) -> KA
{
    move |x, y| f(x, y)
}

fn fold<F: FnOnce(KA, KA) -> KA>(f: F) -> impl FnOnce(KA, KA) -> KA
{
    move |x, y| f(x, y)
}

// verbs:+!*(/:)#,-(\:)/\@$=
fn lex<'a>(s: &'a str) -> Vec<Tok<'a>>
{
    enum LM
    {
        V,
        N,
        U,
    }
    struct L<'a>
    {
        m: LM,
        s: &'a str,
        i: S,
    }
    impl<'a> Iterator for L<'a>
    {
        type Item = Tok<'a>;
        fn next(&mut self) -> Option<Tok<'a>>
        {
            let mut i = self.i;
            while i < self.s.len()
            {
                println!("{}", &self.s[..i]);
                match self.s[..i].as_bytes()
                {
                    b"/:" | b"\\:" | b"'" =>
                    {
                        self.i = 1;
                        let (h, t) = self.s.split_at(i);
                        self.s = t;
                        return Some(Tok::A(h));
                    },

                    b"/" | b"\\" if self.s.len() == 1 =>
                    {
                        self.i = 1;
                        let(h,t)=self.s.split_at(i);
                        self.s=t;
                        return Some(Tok::A(h))
                    },

                    b"/" | b"\\" =>
                    {
                        i += 1;
                        continue
                    }

                    &[47u8, x] | &[92, x] if x != (':' as u8) => // 47 = /,92 = \
                    {
                        self.i = 1;
                        let (h, t) = self.s.split_at(i-1);
                        self.s=t;
                        return Some(Tok::A(h))
                    },

                    &[x] if b"+!*#,-@$=".contains(&x) =>
                    {
                        self.i = 1;
                        let (h, t) = self.s.split_at(i);
                        self.s = t;
                        return Some(Tok::V(h))
                    },

                    &[ref cs.., x] if b"+!*#,-@$=".contains(&x) =>
                    {
                        self.i = 1;
                        let (h, t) = self.s.split_at(i-1);
                        self.s = t;
                        return Some(Tok::N(h))
                    },

                    //&[ref cs..] if cs.len() == self.s.len() =>
                    //{
                    //    self.i = 1;
                    //    let (h, t) = self.s.split_at(i);
                    //    self.s = t;
                    //    return Some(Tok::N(h))
                    //},

                    &[] => return None,
                    _ => (),
                }
                i += 1;
            }

            None
        }
    }

    (L { m: LM::U, s: s, i: 1 }).collect()
}

#[derive(Clone,PartialEq,PartialOrd,Debug)]
enum T<'a>
{
    N(&'a str),
    V(&'a str, Vec<&'a str>),
    K(KA),
}
// just coalesces adverbs
fn pa<'a>(s: &'a str) -> Vec<T<'a>>
{
    let mut adv = Vec::new();
    let mut ter = Vec::new();
    let mut tok = lex(s);
    while let Some(t) = tok.pop()
    {
        match t
        {
            Tok::N(n) => ter.push(T::N(n)),
            Tok::V(v) =>
            {
                adv.reverse();
                ter.push(T::V(v, adv));
                adv = Vec::new();
            },
            Tok::A(a) => adv.push(a),
        }
    }

    ter.reverse();
    ter
}

fn pr(k: KA)
{
    println!("{:?}", k)
}

fn ca(s: &str, p: S) -> char
{
    s.as_bytes()[p] as char
}

fn nn(n: &str, v: &HashMap<String, KA>) -> KA
{
    if n.is_empty()
    {
        KA::KE("noun?")
    }
    else if ca(n, 0).is_digit(10)
    {
        if n.contains(".")
        {
            match F::from_str(n)
            {
                Ok(x) => KA::KF(mk(0, x)),
                Err(_) => KA::KE("float?"),
            }
        }
        else
        {
            match I::from_str(n)
            {
                Ok(x) => KA::KI(mk(0, x)),
                Err(_) => KA::KE("int?"),
            }
        }
    }
    else
    {
        match v.get(n)
        {
            Some(k) => k.clone(),
            None => KA::KE("var?"),
        }
    }
}

// verbs:+!*(/:)#,-(\:)/\@$=
fn mon<'a>(verb: &'a str, advs: &[&'a str], r: KA) -> KA
{
    match verb
    {
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

fn dya<'a>(verb: &'a str, advs: &[&'a str], l: KA, r: KA) -> KA
{
    match verb
    {
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

// recursion err here
// replace with destructuring of k
// for each adv instead
// can maybe remove recursion as well
// fn madv<'a, F: FnOnce(KA) -> KA>(f: F, advs: &[&'a str], k: KA) -> KA
//
//    if advs.is_empty()
//    {
//        return f(k);
//    }
//
//    let (adv, rest) = advs.split_at(0);
//
//    match adv
//    {
//        // todo: try to avoid recursion?
//        &["/:"] => madv(eachr(f), rest, k),
//        &[r"\:"] => madv(eachl(f), rest, k),
//        &["/"] | &["\\"] | &["'"] => return KA::KE("type"),
//        _ => return KA::KE("adv?"),
//    }
//

fn madv<'a, F: Fn(KA) -> KA>(f: &F, advs: &[&'a str], k: KA) -> KA
{
    match advs
    {
        &["'", ref advs..] => match k
        {
            KA::KK(ks) => rz(KA::KK(wr(ks.i().map(|k| madv(f, advs, k.clone())).collect()))),
            KA::KI(ki) => rz(KA::KK(wr(ki.i().map(|i| madv(f, advs, f(KA::KI(mk(0, *i))))).collect()))),
            KA::KC(kc) => rz(KA::KK(wr(kc.i().map(|i| madv(f, advs, f(KA::KC(mk(0, *i))))).collect()))),
            KA::KF(kf) => rz(KA::KK(wr(kf.i().map(|i| madv(f, advs, f(KA::KF(mk(0, *i))))).collect()))),
            KA::KB(kb) => rz(KA::KK(wr(kb.i().map(|i| madv(f, advs, f(KA::KB(mk(0, *i))))).collect()))),
            _ => k, // KE,KZ
        },
        &[] => f(k),
        _ => KA::KE("adv?"),
    }
}

// if / or \ are at the top of adv, then ignore l, and just f(/|\)r
fn dadv<'a, F: Fn(KA, KA) -> KA>(f: &F, advs: &[&'a str], l: KA, r: KA) -> KA
{
    KA::KE("dadv nyi")
}

fn rz(k: KA) -> KA
{
    #[derive(Clone)]
    enum ST
    {
        B(Vec<bool>),
        C(Vec<char>),
        I(Vec<I>),
        K(Vec<KA>),
        F(Vec<F>),
        Z,
    }
    let mut st = ST::Z;

    match k.clone()
    {
        KA::KK(ks) =>
        {
            for kk in ks.i().cloned()
            {
                st = match (kk, st)
                {
                    (KA::KK(k0), ST::K(mut v)) =>
                    {
                        v.extend(k0.i().cloned());
                        ST::K(v)
                    },
                    (KA::KK(k0), ST::Z) =>
                    {
                        ST::K(k0.i().cloned().collect())
                    },
                    (KA::KB(k0), ST::B(mut v)) =>
                    {
                        v.extend(k0.i().cloned());
                        ST::B(v)
                    },
                    (KA::KB(k0), ST::Z) =>
                    {
                        ST::B(k0.i().cloned().collect())
                    },
                    (KA::KC(k0), ST::C(mut v)) =>
                    {
                        v.extend(k0.i().cloned());
                        ST::C(v)
                    },
                    (KA::KC(k0), ST::Z) =>
                    {
                        ST::C(k0.i().cloned().collect())
                    },
                    (KA::KF(k0), ST::F(mut v)) =>
                    {
                        v.extend(k0.i().cloned());
                        ST::F(v)
                    },
                    (KA::KF(k0), ST::Z) =>
                    {
                        ST::F(k0.i().cloned().collect())
                    },
                    (KA::KI(k0), ST::I(mut v)) =>
                    {
                        v.extend(k0.i().cloned());
                        ST::I(v)
                    },
                    (KA::KI(k0), ST::Z) =>
                    {
                        ST::I(k0.i().cloned().collect())
                    },
                    _ => return k,
                }
            }

            match st
            {
                ST::B(v) => KA::KB(wr(v)),
                ST::C(v) => KA::KC(wr(v)),
                ST::I(v) => KA::KI(wr(v)),
                ST::F(v) => KA::KF(wr(v)),
                ST::K(v) => KA::KK(wr(v)),
                ST::Z => KA::KZ,
            }
        },
        _ => k,
    }
}

fn ev(s: &str, v: &mut HashMap<String, KA>)
{
    let mut ter = pa(s);
    println!("{:?}", &ter);
    loop
    {
        match (ter.pop(), ter.pop(), ter.pop())
        {
            // empty stack!
            (None, _, _) => return pr(KA::KE("empty?")),

            // stop on err
            (Some(T::K(KA::KE(e))), _, _) => return pr(KA::KE(e)),

            // last res
            (Some(T::K(k)), None, _) => return pr(k),
            (Some(T::N(n)), None, _) => return pr(nn(n, v)),
            (Some(T::V(vb, a)), None, _) => return println!("{}{}", vb, a.join("")),

            // set
            (Some(T::N(n)), Some(T::V(":", _)), Some(T::K(k))) =>
            {
                v.insert(n.to_owned(), k.clone());
                ter.push(T::K(k))
            },

            // monad
            // forced monads
            (Some(T::K(k2)), Some(T::K(k1)), None) => ter.push(T::K(dat(k1, k2))),
            (Some(T::N(n2)), Some(T::N(n1)), None) => ter.push(T::K(dat(nn(n1, v), nn(n2, v)))),
            // possibly dyads based on adverbs
            // if a contains / or \ then it is a dyad in a monadic position
            // and can be called with KV
            (Some(T::K(k2)), Some(T::V(vb, a)), None) =>
            {
                if a.contains(&"\\") || a.contains(&"/")
                {
                    ter.push(T::K(dya(vb, &a[..], k2, KA::KZ)))
                }
                else
                {
                    ter.push(T::K(mon(vb, &a[..], k2)))
                }
            },
            (Some(T::N(n2)), Some(T::V(vb, a)), None) =>
            {
                if a.contains(&"\\") || a.contains(&"/")
                {
                    ter.push(T::K(dya(vb, &a[..], nn(n2, v), KA::KZ)))
                }
                else
                {
                    ter.push(T::K(mon(vb, &a[..], nn(n2, v))))
                }
            },
            (Some(T::N(n2)), Some(T::V(v1, a)), Some(T::V(v0, a0))) =>
            {
                ter.push(T::V(v0, a0));
                if a.contains(&"\\") || a.contains(&"/")
                {
                    ter.push(T::K(dya(v1, &a[..], nn(n2, v), KA::KZ)))
                }
                else
                {
                    ter.push(T::K(mon(v1, &a[..], nn(n2, v))))
                }
            },
            (Some(T::K(k2)), Some(T::V(v1, a)), Some(T::V(v0, a0))) =>
            {
                ter.push(T::V(v0, a0));
                if a.contains(&"\\") || a.contains(&"/")
                {
                    ter.push(T::K(dya(v1, &a[..], k2, KA::KZ)))
                }
                else
                {
                    ter.push(T::K(mon(v1, &a[..], k2)))
                }
            },

            // dyad
            (Some(T::K(k2)), Some(T::V(vb, a)), Some(T::K(k0))) => ter.push(T::K(dya(vb, &a[..], k0, k2))),
            (Some(T::N(n2)), Some(T::V(vb, a)), Some(T::K(k0))) => ter.push(T::K(dya(vb, &a[..], nn(n2, v), k0))),
            (Some(T::N(n2)), Some(T::V(vb, a)), Some(T::N(n0))) => ter.push(T::K(dya(vb, &a[..], nn(n2, v), nn(n0, v)))),

            // errs
            (Some(T::V(_, _)), Some(T::V(_, _)), _) => return pr(KA::KE("type")),

            _ => return pr(KA::KE("ev nyi")),
        }
    }
}

fn main()
{
    let mut vars: HashMap<String, KA> = HashMap::new();
    let stdin = io::stdin();
    let mut s = String::new();
    loop
    {
        s.clear();
        match stdin.read_line(&mut s)
        {
            Ok(0) => continue,
            Ok(_) =>
            {
                match s.as_str()
                {
                    r"\\" => return,
                    _ => (),
                }

                ev(&s, &mut vars)
            },
            _ => return,
        }
    }
}
