/*
--------------------------------------------------------------------------------
v ← +-*%<>!$^&_=|~# <- -> ← → arrows are equivalent
c ← [](){};
a ← /\' /: \: ': 
n ← ".*" [a-zA-Z]+[0-9a-zA-Z]* [0-9]+
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
T : T /type - T:T is inconsistent
B : T /bool
I : T /i64
F : T /f64
S : T /string - unicode array
O : T → T /option
E : T → T → T /disjoint union
V : I → T → T /typed vector
H : [n : I] → V[n;T] → T /mixed vector (vector with associated vector of types)
M : T → T → T /hashmap
Ptr : T → T /foreign ptr
Handle : T 
Σ : (a : T) → (a → T) → T

optional : T → T
optional ← {
 some ← [x : t] → option t
 none ← option t
};

/ can oneline types + definition with ; separation
either : T → T → T ← {left ← [x : s] → either s t; right ← [y : t] → either s t}

map_either : [a → c] [b → d] [either a b] → either c d 
map_either ← {[f;g;left x] → left f x;[f;g;right x] → right g y}

to_optional : either a b → optional a
to_optional ← {[left x] → some x; ... → none}

/ extend verbs with patterns and types
*either a b : optional a ← to_optional

(optional a)*optional a : optional a
(optional a)*optional a ← { [some x] [some y] → some x * y ; ... → none }
/ *left 2        ~> some 2
/ *right "seven" ~> none

/ omit the naming if no matching is required
matrix : I → I → T → T
matrix ← { mkmatrix ← [V[n;V[m;t]]] → matrix[n;m;t]}

(matrix[a;b;t])*matrix[b;c;t] : matrix[a;c;t] ← {
 [mkmatrix x] [mkmatrix y] → mkmatrix x {+/x*'y}\: +y 
}

filter : (t → B) → V[n;t] → Σ[[m:I];V[m;t]]
filter ← {n ← #x←&x'y; Σ[n;y@x]}

A ← mkmatrix 3 3 # !9 /(0 1 2;3 4 5;6 7 8)
B ← A * A /(15 18 21;42 54 66;69 90 111)
v ← !10
0@0@A ← 3 /(3 1 2;3 4 5;6 7 8)
C ← +B ← A*A / multiple assignments in an expression
w ← v@3 ← 100 / w = 100
*/

#![feature(slice_patterns,advanced_slice_patterns,custom_attribute)]
#![feature(conservative_impl_trait,box_syntax,box_patterns,non_ascii_idents)]
#![allow(unused_variables,dead_code,unused_imports,non_camel_case_types)]
#![deny(unreachable_patterns)]
use std::{cmp,mem};use std::collections::HashMap;use std::io::{self,Write};
use std::str::FromStr;
fn main(){}
