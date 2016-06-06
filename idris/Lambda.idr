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
    VarType : SimpleType
    FuncType : SimpleType -> SimpleType -> SimpleType
  
  data Lambda : SimpleType -> Type where
    V : Int -> Lambda VarType
    L : (s : SimpleType) -> Lambda t -> Lambda (FuncType s t)
    A : Lambda (FuncType x y) -> Lambda x -> Lambda y
    
namespace Extended
  data ExtendedType : Type where
    VarType : BasicType -> ExtendedType
    FuncType : ExtendedType -> ExtendedType -> ExtendedType
    DisjType : ExtendedType -> ExtendedType -> ExtendedType
    ConjType : ExtendedType -> ExtendedType -> ExtendedType
  
  data Lambda : ExtendedType -> Type where
    C : (τ : BasicType) -> value τ -> Lambda (VarType τ)
    V : (τ : BasicType) -> Int -> Lambda (VarType τ)
    L : (α : ExtendedType) -> Lambda β -> Lambda (FuncType α β)
    A : Extended.Lambda (FuncType α β) -> Extended.Lambda α -> Lambda β
  
namespace Extensible
  data ExtensibleType : Type where
    VarType : BasicType -> ExtensibleType
    FuncType : ExtensibleType -> ExtensibleType -> ExtensibleType
    ConjType : ExtensibleType -> ExtensibleType -> ExtensibleType
    DisjType : ExtensibleType -> ExtensibleType -> ExtensibleType
    DefnType : String -> ExtensibleType -> ExtensibleType
  
  data Lambda : ExtensibleType -> Type where
    C : (τ : BasicType) -> value τ -> Extensible.Lambda (VarType τ)
    V : (τ : BasicType) -> Int -> Extensible.Lambda (VarType τ)
    L : (α : ExtensibleType) -> Lambda β -> Lambda (FuncType α β)
    A : Extensible.Lambda (FuncType α β) -> Lambda α -> Lambda β
