use std::ops::{Add, Mul, Sub, Div, Index, IndexMut};
use lib;
macro_rules! numeric_impl 
{
    ($t:ident, 1) => {
        impl Add<$t> for $t
        {
            type Output = $t;
            fn add(self, y : $t) -> Self::Output
            {
                $t(self.0 + y.0)
            }
        }
        impl Sub<$t> for $t
        {
            type Output = $t;
            fn sub(self, y : $t) -> Self::Output
            {
                $t(self.0 - y.0)
            }
        }
        impl Mul<$t> for $t
        {
            type Output = $t;
            fn mul(self, y : $t) -> Self::Output
            {
                $t(self.0 * y.0)
            }
        }
    };
    ($t:ident, 2) => {
        impl Add<$t> for $t
        {
            type Output = $t;
            fn add(self, y : $t) -> Self::Output
            {
                $t(self.0 + y.0, self.1 + y.1)
            }
        }
        impl Sub<$t> for $t
        {
            type Output = $t;
            fn sub(self, y : $t) -> Self::Output
            {
                $t(self.0 - y.0, self.1 - y.1)
            }
        }
        impl Mul<$t> for $t
        {
            type Output = $t;
            fn mul(self, y : $t) -> Self::Output
            {
                $t(self.0 * y.0, self.1 * y.1)
            }
        }
    }
}

macro_rules! scalar_impl
{
    ($s:ident, $t:ty, 1) =>
    {
        impl Add<$t> for $s
        {
            type Output = $s;
            fn add(self, x : $t) -> Self::Output {$s(self.0 + x)}
        }
        impl Sub<$t> for $s
        {
            type Output = $s;
            fn sub(self, x : $t) -> Self::Output {$s(self.0 - x)}
        }
        impl Mul<$t> for $s
        {
            type Output = $s;
            fn mul(self, x : $t) -> Self::Output {$s(self.0 * x)}
        }
        impl Div<$t> for $s
        {
            type Output = $s;
            fn div(self, x : $t) -> Self::Output {$s(self.0 / x)}
        }
    };
    ($s:ident, $t:ty, 2) =>
    {
        impl Add<$t> for $s
        {
            type Output = $s;
            fn add(self, x : $t) -> Self::Output {$s(self.0 + x, self.0 + x)}
        }
        impl Sub<$t> for $s
        {
            type Output = $s;
            fn sub(self, x : $t) -> Self::Output {$s(self.0 - x, self.1 - x)}
        }
        impl Mul<$t> for $s
        {
            type Output = $s;
            fn mul(self, x : $t) -> Self::Output {$s(self.0 * x, self.1 * x)}
        }
        impl Div<$t> for $s
        {
            type Output = $s;
            fn div(self, x : $t) -> Self::Output {$s(self.0 / x, self.1 / x)}
        }
    };
}

macro_rules! index_impl
{
    ($x:ty, $y:ty, $o:ty) =>
    {
        impl Index<&'a $y> for $x
        {
            type Output = $o;
            fn index(&'a self, idx : &'a $x) -> Self::Output
            {
                self[idx.0]
            }
        }
    }
}

#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Debug, Ord)]
pub struct Product(usize);
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Debug, Ord)]
pub struct Poptype(usize); 
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Debug, Ord)]
pub struct Terrain(usize); 
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Debug, Ord)]
pub struct Weather(usize);
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Debug, Ord)]
pub struct Hex(usize, usize); 
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Debug, Ord)]
pub struct Elevation(usize);
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Debug, Ord)]
pub struct Attack(usize); 
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Debug, Ord)]
pub struct Defence(usize);
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Debug, Ord)]
pub struct Width(usize);
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Debug, Ord)]
pub struct Size(usize);
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Debug, Ord)]
pub struct Id(usize);
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Debug, Ord)]
pub struct Speed(usize);
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Debug, Ord)]
pub struct Priority(usize);

numeric_impl!{Attack, 1}
scalar_impl!{Attack, usize, 1}
numeric_impl!{Defence, 1}
scalar_impl!{Defence, usize, 1}
numeric_impl!{Width, 1}   
scalar_impl!{Width, usize, 1}
numeric_impl!{Size, 1}
scalar_impl!{Size, usize, 1}
numeric_impl!{Hex, 2}
scalar_impl!{Hex, usize, 2}
numeric_impl!{Speed, 1}
scalar_impl!{Speed, usize, 1}
numeric_impl!{Id,1}
scalar_impl!{Id,usize,1}
index_impl!{Vec<lib::Unit>,Id,lib::Unit}
numeric_impl!{Priority, 1}
scalar_impl!{Priority, usize, 1}

// Error struct

pub struct HexError
{
    pub kind : &'static str,
    pub errmsg : &'static str,
}

