module Quotient 
%access public export  -- replace this at some point

data Equivalence : (t -> t -> Type) -> Type where
  MkEqui : (rel : t -> t -> Type) ->
            (refl : (x : t) -> rel x x) ->
            (symm : (x, y : t) -> rel x y -> rel y x) ->
            (trans : (x, y, z : t) -> rel x y -> rel y z -> rel x z) ->
            Equivalence rel

prf : Equivalence rel -> (t -> t -> Type)
prf {t} (MkEqui rel {t = t} _ _ _) = rel 

data Quotient : {rel : t -> t -> Type} -> Equivalence rel -> (t : Type) -> Type where
  MkQuotient : (x : t) -> (y ** (prf rel) x y) -> Quotient rel t

infixl 5 //, .//
(//) : {rel : t -> t -> Type} -> (t : Type) -> Equivalence rel -> Type
t // rel = Quotient rel t

data IntMod : Int -> Type where
  MkIntMod  : {n : Int} -> Int -> IntMod n

(.//) : {rel : t -> t -> Type} -> (f : t -> t) -> (e : Equivalence rel) -> Quotient e t -> Quotient e t
(.//) f (MkEqui rel refl symm trans) (MkQuotient x (repr ** prf)) = 
  let y = f repr in
  MkQuotient y (y ** refl y)

Num (IntMod n) where
  (MkIntMod x) + (MkIntMod y) = assert_total $ MkIntMod $ (x + y) `mod` n
  (MkIntMod x) * (MkIntMod y) = assert_total $ MkIntMod $ (x * y) `mod` n
  fromInteger a = assert_total $ MkIntMod ((fromInteger a) `mod` n)

Neg (IntMod n) where
  (MkIntMod x) - (MkIntMod y) = assert_total $ MkIntMod $ (x - y) `mod` n
  abs (MkIntMod x) = assert_total $ MkIntMod $ (abs x) `mod` n
  negate (MkIntMod x) = assert_total $ MkIntMod $ (n - x) `mod` n

Eq (IntMod n) where
  (MkIntMod x) == (MkIntMod y) = assert_total $ let xn = x `mod` n in
                                let yn = x `mod` n in
                                let xn' = if xn < 0 then xn + n else xn in
                                let yn' = if yn < 0 then yn + n else yn in
                                xn' == yn'
Ord (IntMod n) where
  compare (MkIntMod x) (MkIntMod y) = assert_total $ let xn = x `mod` n in
                                    let yn = x `mod` n in
                                    let xn' = if xn < 0 then xn + n else xn in
                                    let yn' = if yn < 0 then yn + n else yn in
                                    compare xn' yn'

MaxBound (IntMod n) where
  maxBound = MkIntMod (n - 1)

MinBound (IntMod n) where
  minBound = MkIntMod 0

Enum (IntMod n) where
  pred (MkIntMod x) = MkIntMod $ if x == n - 1 then 0 else x + 1
  succ (MkIntMod x) = MkIntMod $ if x == 0 then n - 1 else x - 1
  toNat (MkIntMod x) = toNat $ if x < 0 then x + n else x
  fromNat k = MkIntMod $ assert_total (fromNat $ k `mod` (cast n))

intModEq : IntMod j -> IntMod k -> Bool
intModEq {j} {k} (MkIntMod x) (MkIntMod y) = j == k && x == y

intModCmp : IntMod j -> IntMod k -> Ordering
intModCmp {j} {k} (MkIntMod x) (MkIntMod y)  = case compare j k of
  EQ => compare x y
  r => r

||| Expands or contracts the modulus of an IntMod
intModExt : IntMod j -> IntMod k
intModExt {k} (MkIntMod x) = MkIntMod (assert_total $ x `mod` k)

