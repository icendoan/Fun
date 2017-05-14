%default total
data NValley : (f : Nat -> Nat) -> (n : Nat) -> (l : Nat) -> Type where
  Single : (f n = f n) -> NValley f n 0
  Range  : (f n = f (n + (S l))) -> NValley f n l -> NValley f n (S l)

Decreasing : (Nat -> Nat) -> Type
Decreasing f = (n, k : Nat) -> n `LTE` k -> (f k) `LTE` (f n)
  
lteZero_x_Z_is_Z : LTE x Z -> x = Z
lteZero_x_Z_is_Z LTEZero = Refl

lemma0 : x `LTE` y -> Either (x = y) ((S x) `LTE` y)
lemma0 {x = Z} {y = Z} l = Left Refl
lemma0 {x = Z} {y = (S k)} LTEZero = Right (LTESucc LTEZero)
lemma0 {x = (S k)} {y = (S j)} (LTESucc l) =
  case lemma0 l of
    (Left l) => Left (cong l)
    (Right r) => Right (LTESucc r)

nvalley_at_zero_lemma : (f : Nat -> Nat) -> 
                        (dec : Decreasing f) -> 
                        (start, posn : Nat) -> 
                        (isZero : 0 = f start) -> 
                        (lte : start `LTE` posn) -> 0 = f posn
nvalley_at_zero_lemma f dec k x isZero lte = sym $ lteZero_x_Z_is_Z $ (sym isZero) `replace` (dec k x lte)
  
nvalley_at_zero : (f : Nat -> Nat) ->
                  (dec : Decreasing f) ->
                  (start, len : Nat) ->
                  (isZero : 0 = f start) ->
                  NValley f start len
nvalley_at_zero f dec start Z isZero = Single Refl
nvalley_at_zero f dec start (S k) isZero = 
  let rec = nvalley_at_zero f dec start k isZero  in
  let rhs = nvalley_at_zero_lemma f dec start (start + (S k)) isZero (lteAddRight start) in
  Range ((sym isZero) `trans` rhs) rec
  
nvalley_test_range : (f : Nat -> Nat) -> 
                     (dec : Decreasing f) -> 
                     (k, n : Nat) -> Either (NValley f k n) (new_start : Nat ** (f new_start) `LT` f k)
nvalley_test_range f dec k Z = Left (Single Refl)
nvalley_test_range f dec k (S j) = 
  case nvalley_test_range f dec k j of
    (Left l) => case (decEq (f k) (f (k + (S j)))) of
      (Yes prf) => Left (Range prf l)
      (No contra) => 
        let lte = dec k (k + S j) (lteAddRight k) in
        case lemma0 lte of
          (Left l0) => void (contra (sym l0))
          (Right r0) => Right ((k + (S j)) ** r0)
    (Right (new_start ** ltPrf)) => Right (new_start ** ltPrf)

nvalley_step : (f : Nat -> Nat) -> 
               (prf : Decreasing f) -> 
               (k, n : Nat) ->
               (spine : Nat) -> 
               (spine = f k) -> 
               (start : Nat ** NValley f start n)
nvalley_step f prf k n (f k) Refl = case (decEq 0 (f k)) of
  Yes prf0 => (k ** nvalley_at_zero f prf k n prf0)
  No _ => case nvalley_test_range f prf k n of
    (Left l) => (k ** l)
    (Right (new_start ** ltPrf)) => nvalley_step f prf new_start n ((f k) `assert_smaller` f new_start) Refl
 
nvalley : (f : Nat -> Nat) -> (prf : Decreasing f) -> (n : Nat) -> (k : Nat ** NValley f k n)
nvalley f prf n = nvalley_step f prf 0 n (f 0) Refl
