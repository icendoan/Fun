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
lemma0 {x = (S k)} {y = Z} l impossible
lemma0 {x = (S k)} {y = (S j)} (LTESucc l) =
  case lemma0 l of
    (Left l) => Left (cong l)
    (Right r) => Right (LTESucc r)

nvalley_at_zero_lemma : (f : Nat -> Nat) -> 
                        (dec : Decreasing f) -> 
                        (start : Nat) -> 
                        (isZero : 0 = f start) -> 
                        (posn : Nat) -> 
                        (lte : start `LTE` posn) -> 0 = f posn
nvalley_at_zero_lemma f dec k isZero x lte = 
  let prfLTE = dec k x lte in 
  let prfX0 = (sym isZero) `replace` prfLTE in
  let prfX1 = lteZero_x_Z_is_Z prfX0 in
  sym prfX1
  
nvalley_at_zero : (f : Nat -> Nat) ->
                  (dec : Decreasing f) ->
                  (start : Nat) ->
                  (isZero : 0 = f start) ->
                  (len : Nat) ->
                  NValley f start len
nvalley_at_zero f dec start isZero Z = Single Refl
nvalley_at_zero f dec start isZero (S k) = 
  let rec = nvalley_at_zero f dec start isZero k in
  let lhs = sym isZero in
  let rhs = nvalley_at_zero_lemma f dec start isZero (start + (S k)) (lteAddRight start) in
  let prf = lhs `trans` rhs in
  Range prf rec
  
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
               (spine = f k) -> (start : Nat ** NValley f start n )
nvalley_step f prf k n (f k) Refl = case (decEq 0 (f k)) of
  Yes prf0 => (k ** nvalley_at_zero f prf k prf0 n)
  No _ => case nvalley_test_range f prf k n of
    (Left l) => (k ** l)
    (Right (new_start ** ltePrf)) => nvalley_step f prf new_start n (f new_start) Refl
  
nvalley : (f : Nat -> Nat) ->
          (prf : Decreasing f) ->
          (n : Nat) -> (k : Nat ** NValley f k n)
nvalley f prf n = nvalley_step f prf 0 n (f 0) Refl

constNDecreasing : (result : Nat) -> (Decreasing (const result))
constNDecreasing result = \n, k, _ => lteRefl 
