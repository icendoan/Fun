import Data.Vect

%default total
partial fix : Eq a => (a -> a) -> a -> a
fix f x = let s = iterate f x in go s
  where
    partial go : Eq a => Stream a -> a
    go (x::y::xs) = if x == y then x else go xs

namespace Untyped
  data Lambda : Type where
    L : Lambda -> Lambda
    V : Int -> Lambda
    A : Lambda -> Lambda -> Lambda
  
  -- current depth, and then terms and the depth they were added
  Env : Type
  Env = (Int, List (Int, Lambda))
  
  -- this has an indexing problem

  get : Env -> Int -> Lambda
  get (_, []) n = V n -- just return the variable term
  get (d, (m, t) :: e) n = if m == (d - n) then t else get ((d,(m,t)::e)`assert_smaller` (d, e)) n
  
  s : Int -> Int
  s x = x + 1

  beta : Env -> Lambda -> Lambda
  beta (x, e) (L l) = L $ beta (s x, e) l
  beta e (V v) = get e v
  beta (x, e) (A (L l) r) = beta (x, (x, r) :: e) l
  beta e (A l (V n)) = A l (get e n)
  beta (x, e) (A l r) = A (beta (x, e) l) (beta (x, e) r) -- need to substitute existing context in params
  
  β : Lambda -> Lambda
  β = beta (0, [])
  
  Show Lambda where
    show (L l) = "λ" ++ show l 
    show (V n) = show n
    show (A l r) = "(" ++ show l ++ ", " ++ show r ++ ")"

  Eq Lambda where
    (L x) == (L y) = x == y
    (V x) == (V y) = x == y
    (A x y) == (A z w) = (x == z) && (y == w)
    _ == _ = False

  partial eval : Lambda -> Lambda
  eval = fix β

data BasicType = BInt | BFloat | BChar | BArr Nat BasicType
  
value : BasicType -> Type
value BInt = Int
value BFloat = Double
value BChar = Char
value (BArr n t) = Vect n (value t)
  
namespace Simple
  data SimpleType : Type where
    BaseType : SimpleType
    FuncType : SimpleType -> SimpleType -> SimpleType
  
  data Lambda : SimpleType -> Type where
    V : Int -> Lambda τ
    L : (s : SimpleType) -> Lambda t -> Lambda (FuncType s t)
    A : Lambda (FuncType x y) -> Lambda x -> Lambda y
  
  Env : Type
  Env = List (Int, (τ : SimpleType ** Lambda τ))
  
  varIndexExtract : (Simple.V x = V y) -> x = y
  varIndexExtract Refl = Refl
  
  funcParamExtract : (Simple.FuncType x y) = (FuncType x' y') -> y = y'
  funcParamExtract Refl = Refl
  
  baseNotFunc : (BaseType = FuncType _ _) -> Void
  baseNotFunc Refl impossible
  
  varNotFunc : V x = L a b -> Void
  varNotFunc Refl impossible
  varNotApp : Simple.V x = A l r -> Void
  varNotApp Refl impossible
  funcNotApp : Simple.A l r = L a b -> Void
  funcNotApp Refl impossible
  
  DecEq (Lambda τ) where
    decEq (V x) (V y) with (decEq x y)
      | (Yes prf) = Yes $ cong {f=V} prf
      | (No contra) = No $ \prf : (Simple.V x = V y) => contra (varIndexExtract prf)
    decEq (L α β) (L α β') with (decEq β β')
      | (Yes prf) = Yes $ cong {f=L α} prf
      | (No contra) = No $ \prf => contra ((\Refl => Refl) prf)
    decEq (A {x} l r) (A {x} l' r') with (decEq r r')
      | (Yes prf) = case decEq l l' of
             Yes Refl => Yes $ cong {f=A l} prf
             No contr => No $ \prf => contr ((\Refl => Refl) prf)
      | (No contra) = No $ \prf => contra ((\Refl => Refl) prf)
    decEq (V x) (L a b) = No varNotFunc
    decEq (V x) (A l r) = No varNotApp
    decEq (L a b) (A l r) = No $ negEqSym funcNotApp
    decEq (L a b) (V x) = No $ negEqSym varNotFunc
    decEq (A l r) (V x) = No $ negEqSym varNotApp
    decEq (A l r) (L a b) = No funcNotApp
  
  DecEq (SimpleType) where
    decEq (BaseType) (BaseType) = Yes Refl
    decEq (FuncType x y) (FuncType x' y') = 
      case decEq y y' of
        Yes prf => case decEq x x' of
           Yes Refl => Yes $ cong {f=FuncType x} prf
           No contra => No $ \prf => contra ((\Refl => Refl) prf)
        No contra => No $ \prf => contra (funcParamExtract prf)
    decEq (FuncType a b) (BaseType) = No $ negEqSym baseNotFunc
    decEq (BaseType) (FuncType a b) = No baseNotFunc
  
  replace : Lambda (FuncType α β) -> Lambda α -> Lambda β
  replace f a = go 0 f a
    where
      go : Nat -> Simple.Lambda (FuncType α β) -> Lambda α -> Lambda β
      go k (L s λ) a = L s $ go (S k) λ a
      go k (A λ ρ) a = A (go k λ α) (go k ρ α)
      go k (V x) a = V x
  
  beta : Lambda τ -> Lambda τ
  beta (V x) = V x
  beta (L s x) = L s $ beta x
  beta (A y z) = replace y z
