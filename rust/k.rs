use std::rc::Rc;
use std::io::{self, Read, Write, BufRead, BufWrite};
use std::collections::HashMap;
type I = i64;
type F = f64;
type S = usize;
#[derive(Clone,PartialOrd,PartialEq,Debug)]
enum K0<T> {
    A(T),
    L(Vec<T>),
}
type K<T> = Rc<K0<T>>;
#[derive(Clone,PartialOrd,Ord,PartialEq,Eq,Hash,Debug)]
enum Tok<'a> {
    V(&'a str),
    N(&'a str),
    A(&'a str),
}
trait KT: PartialEq + PartialOrd + Clone {}
impl KT for I {}
impl KT for char {}
impl KT for F {}
impl<T: KT> KT for K0<T> {}
impl KT for KA {}
//int char mix err
#[derive(Clone,PartialOrd,PartialEq,Debug)]
enum KA {
    KI(K<I>),
    KC(K<char>),
    KF(K<F>),
    KK(K<KA>),
    KE(&'static str),
}
impl<T: KT> K0<T> {
    fn len(&self) -> S {
        match *self {
            K0::A(_) => 0,
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
                if *ix > v.len() {
                    None
                } else {
                    *ix += 1;
                    Some(&v[*ix - 1])
                }
            }
        }
    }
}

impl KA {
    fn len(&self) -> S {
        match *self {
            KA::KI(ref ki) => ki.len(),
            KA::KC(ref kc) => kc.len(),
            KA::KF(ref kf) => kf.len(),
            KA::KK(ref kk) => kk.len(),
            KA::KE(_) => 0,
        }
    }

    fn a(&self) -> bool {
        match *self {
            KA::KI(ref k) => k.a(),
            KA::KC(ref k) => k.a(),
            KA::KF(ref k) => k.a(),
            KA::KK(ref k) => k.a(),
            KA::KE(_) => false,
        }
    }
}
fn wr<T: KT>(v: Vec<T>) -> K<T> {
    Rc::new(K0::L(v))
}
fn mk<T: KT>(n: S, d: T) -> K<T> {
    Rc::new(if n == 0 { K0::A(d) } else { K0::L(vec![d;n]) })
}
/*add*/
fn dplus(l: KA, r: KA) -> KA {
    match (l, r) {
        (KA::KI(li), KA::KI(ri)) => {
            if li.len() == ri.len() {
                KA::KI(wr(li.i()
                    .zip(ri.i())
                    .map(|(x, y)| x + y)
                    .collect()))
            } else {
                KA::KE("rank")
            }
        }
        (KA::KF(lf), KA::KF(rf)) => {
            if lf.len() == rf.len() {
                KA::KF(wr(lf.i()
                    .zip(rf.i())
                    .map(|(x, y)| x + y)
                    .collect()))
            } else {
                KA::KE("rank")
            }
        }
        (KA::KI(_), KA::KI(_)) |
        (KA::KF(_), KA::KF(_)) => KA::KE("rank"),
        (KA::KK(lk), KA::KK(rk)) => eachb(lk, dplus, rk),
        _ => KA::KE("type"),
    }
}
/*verbs*/
/*flip*/
fn mplus(r: KA) -> KA {
    KA::KE("nyi")
}
/*sub*/
fn dmin(l: KA, r: KA) -> KA {
    match (l, r) {
        (KA::KI(li), KA::KI(ri)) => {
            if li.len() == ri.len() {
                KA::KI(wr(li.i()
                    .zip(ri.i())
                    .map(|(x, y)| x - y)
                    .collect()))
            } else {
                KA::KE("rank")
            }
        }
        (KA::KF(lf), KA::KF(rf)) => {
            if lf.len() == rf.len() {
                KA::KF(wr(lf.i()
                    .zip(rf.i())
                    .map(|(x, y)| x - y)
                    .collect()))
            } else {
                KA::KE("rank")
            }
        }
        (KA::KI(_), KA::KI(_)) |
        (KA::KF(_), KA::KF(_)) => KA::KE("rank"),
        (KA::KK(lk), KA::KK(rk)) => eachb(lk, dplus, rk),
        _ => KA::KE("type"),
    }
}
/*neg*/
fn mmin(r: KA) -> KA {
    match &r {
        &KA::KI(_) => dstar(KA::KI(mk(0, -1)), r),
        &KA::KF(_) => dstar(KA::KF(mk(0, -1f64)), r),
        &KA::KK(_) => eachr(mmin, r),
        _ => KA::KE("type"),
    }
}

/*first*/
fn mstar(r: KA) -> KA {
    match r {
        KA::KI(ki) => {
            let kp: &K0<I> = &ki;
            match *kp {
                K0::A(ref a) => KA::KI(mk(0, a.clone())),
                K0::L(ref v) => {
                    if let Some(a) = v.get(0) {
                        KA::KI(mk(0, a.clone()))
                    } else {
                        KA::KE("empty")
                    }
                }
            }
        }

        KA::KC(ki) => {
            let kp: &K0<char> = &ki;
            match *kp {
                K0::A(ref a) => KA::KC(mk(0, a.clone())),
                K0::L(ref v) => {
                    if let Some(a) = v.get(0) {
                        KA::KC(mk(0, a.clone()))
                    } else {
                        KA::KE("empty")
                    }
                }
            }
        }

        KA::KF(ki) => {
            let kp: &K0<F> = &ki;
            match *kp {
                K0::A(ref a) => KA::KF(mk(0, a.clone())),
                K0::L(ref v) => {
                    if let Some(a) = v.get(0) {
                        KA::KF(mk(0, a.clone()))
                    } else {
                        KA::KE("empty")
                    }
                }
            }
        }

        KA::KK(ki) => {
            let kp: &K0<KA> = &ki;
            match *kp {
                K0::A(ref a) => KA::KK(mk(0, a.clone())),
                K0::L(ref v) => {
                    if let Some(a) = v.get(0) {
                        KA::KK(mk(0, a.clone()))
                    } else {
                        KA::KE("empty")
                    }
                }
            }
        }

        _ => KA::KE("nyi"),
    }
}

/*mul*/
fn dstar(l: KA, r: KA) -> KA {
    match (l, r) {
        (KA::KI(li), KA::KI(ri)) => {
            if li.len() == ri.len() {
                KA::KI(wr(li.i()
                    .zip(ri.i())
                    .map(|(x, y)| x * y)
                    .collect()))
            } else {
                KA::KE("rank")
            }
        }
        (KA::KF(lf), KA::KF(rf)) => {
            if lf.len() == rf.len() {
                KA::KF(wr(lf.i()
                    .zip(rf.i())
                    .map(|(x, y)| x * y)
                    .collect()))
            } else {
                KA::KE("rank")
            }
        }
        (KA::KI(_), KA::KI(_)) |
        (KA::KF(_), KA::KF(_)) => KA::KE("rank"),
        (KA::KK(lk), KA::KK(rk)) => eachb(lk, dplus, rk),
        _ => KA::KE("type"),
    }

}

/*til*/
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
        eachr(mexcl, r)
    }
}
/*map*/
fn dexcl(l: KA, r: KA) -> KA {
    KA::KE("nyi")
}
/*shape*/
fn dhash(l: KA, r: KA) -> KA {
    KA::KE("nyi")
}
/*first*/
fn mhash(r: KA) -> KA {
    KA::KE("nyi")
}
/*join*/
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
/*enlist*/
fn mcomm(r: KA) -> KA {
    KA::KK(mk(0, r))
}
/*eq*/
fn deq(l: KA, r: KA) -> KA {
    KA::KE("nyi")
}
/*group*/
fn meq(r: KA) -> KA {
    KA::KE("nyi")
}

fn eachr<F: Fn(KA) -> KA>(f: F, r: KA) -> KA {
    match r {
        KA::KI(ki) => KA::KK(wr(ki.i().map(|x| f(KA::KI(mk(0, x.clone())))).collect())),
        KA::KC(ki) => KA::KK(wr(ki.i().map(|x| f(KA::KC(mk(0, x.clone())))).collect())),
        KA::KF(ki) => KA::KK(wr(ki.i().map(|x| f(KA::KF(mk(0, x.clone())))).collect())),
        KA::KK(kk) => KA::KK(wr(kk.i().map(|x| f(KA::KK(mk(0, x.clone())))).collect())),
        r => r, // KE
    }
}
fn eachl<F: Fn(KA) -> KA>(f: F, r: KA) -> KA {
    KA::KE("nyi")
}
fn eachb<F: Fn(KA, KA) -> KA>(l: K<KA>, f: F, r: K<KA>) -> KA {
    KA::KE("nyi")
}

fn lex<'a>(s: &'a str) -> Vec<Tok<'a>> {
    enum LTy {
        N,
        V,
        U,
    }
    struct L<'a> {
        t: &'a str,
        m: LTy,
    }
    impl<'a> Iterator for L<'a> {}
}
fn pr(k: KA) {}
fn main() {
    let mut vars: HashMap<String, KA> = HashMap::new();
}
