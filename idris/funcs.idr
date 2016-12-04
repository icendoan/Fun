-- need the inverse to be b->Maybe a as f could be inclusion
data Injection : (a, b : Type) -> Type where
  MkInj : (f : a -> b) -> (g : b -> a) -> ((x : a) -> (g . f) x = x) -> Injection a b
  
data Surjection : (a, b : Type) -> Type where
  MkSur : (f : a -> b) -> (g : b -> a) -> ((x : b) -> (f . g) x = x) -> Surjection a b
  
data Bijection : (a, b : Type) -> Type where
   MkBij : (f : a -> b) -> (g : b -> a) -> ((x : a) -> (g . f) x = x) -> ((x : b) -> (f . g) x = x) -> Bijection a b
  
getF : Bijection a b -> a -> b
getF (MkBij f g x z) = f
getG : Bijection a b -> b -> a
getG (MkBij f g x z) = g
getL : (f : Bijection a b) -> (x : a) -> ((getG f) . (getF f) $ x) = x
getL (MkBij f g y z) = y
getR : (f : Bijection a b) -> (x : b) -> ((getF f) . (getG f) $ x) = x
getR (MkBij f g y z) = z
 
implicit bijAp : Bijection a b -> (a -> b)
bijAp (MkBij f _ _ _) = f
  
ap : Injection a b -> a -> b
ap (MkInj f _ _ _) = f
ap : Surjection a b -> a -> b
ap (MkSur f _ _ _) = f

compAssoc : (f : a -> b) -> (g : b -> c) -> (h : c -> d) ->
            ((h . g) . f) = (h . (g . f))
compAssoc f g h = Refl
  
compAssoc' : (f : a -> b) -> (g : b -> c) -> (h : c -> d) ->
             (x : a) -> h (g (f x)) = (h . g . f $ x)
compAssoc' f g h x = Refl
  
idNullFuncL : (f : a -> b) -> ((x : a) -> f x = id (f x))
idNullFuncL f = \x => Refl

idNullFuncR : (f : a -> b) -> ((x : a) -> f x = f (id x))
idNullFuncR f = \x => Refl
  
leftRightInverseEqual : (f : a -> b) -> (g : b -> a) -> (g' : b -> a) ->
                        (prfL : (x : a) -> (g' (f x) = x)) ->
                        (prfR : (x : b) -> f (g x) = x) ->
                        (x : b) -> g x = g' x

leftRightInverseEqual f g g' prfL prfR x = 
  let step0 = the (g x = g x) Refl in
  let step1 = (replace (sym $ prfL (g x)) step0) in
  let step2 = prfR x in
  let step3 = cong {f = g'} step2 in
  let step4 = step1 `trans` step3 in
  step4
  
idBij : (a : Type) -> Bijection a a
idBij _ = MkBij Basics.id Basics.id (\x => Refl) (\x => Refl)

idInj : (a : Type) -> Injection a a
idInj _ = MkInj Basics.id Basics.id (\x => Refl)
  
idSur : (a : Type) -> Surjection a a  
idSur _ = MkSur Basics.id Basics.id (\x => Refl)

namespace Injection
  ap : Injection a b -> a -> b
  ap (MkInj f g x) y = f y
namespace Surjection
  ap : Surjection a b -> a -> b
  ap (MkSur f g x) y = f y
namespace Bijection
  ap : Bijection a b -> a -> b
  ap (MkBij f g x z) y = f y

infix 5 <> 
infix 4 <~>
  
injSurBij : (f : Injection a b) -> (g : Injection a b) -> {auto ap f = ap g} -> Bijection a b
injSurjBij (MkInj f g x) (MkSur f g' y) = ?something

interface Semigroup t where
  (<>) : t -> t -> t
  sg_assoc : (x, y, z : t) -> (x <> y) <> z = x <> (y <> z)

interface (Main.Semigroup t) => Group t where
  e : t
  e_identityL : (x : t) -> (e <> x) = x
  e_identityR : (x : t) -> (x <> e) = x
  inv : t -> t
  inv_inv_id : (x : t) -> (x = inv (inv x))
  inv_op : (x : t) -> (x <> (inv x)) = e

interface Group t => Ring t where
  em : t
  (<~>) : t -> t -> t
  em_identityL : (x : t) -> (em <~> x) = x
  em_identityR : (x : t) -> (x <~> em) = x
  rng_assoc : (x, y, z : t) -> (x <~> y) <~> z = x <~> (y <~> z)
  rng_distrib : (x, y, z : t) -> x <~> (y <> z) = x <> y <~> x <> z
  
compBijL : (f : a -> b) -> 
           (g : b -> a) -> 
           (l : (x : a) -> (g . f) x = x) -> 
           (f' : b -> c) -> 
           (g' : c -> b) -> 
           (l' : (x : b) -> (g' . f') x = x) -> 
           (x : a) -> 
           ((g . g') . (f' . f)) x = x
compBijL f g l f' g' l' x = 
  let step0 = the (x = x) Refl in
  let step1 = cong {f = g'} $ cong {f = f'} $ cong {f = f} step0 in
  let step2 = l' (f x) in
  let step3 = step1 `trans` step2 in
  let step4 = cong {f = g} step3 in
  step4 `trans` (l x)

compBijR : (f : a -> b) -> 
           (g : b -> a) -> 
           (r : (x : b) -> (f . g) x = x) -> 
           (f' : b -> c) -> 
           (g' : c -> b) -> 
           (r' : (x : c) -> (f' . g') x = x) -> 
           (x : c) -> 
           ((f' . f) . (g . g')) x = x
compBijR f g r f' g' r' x = 
  let step0 = the (x = x) Refl in
  let step1 = cong {f = f} $ cong {f = g} $ cong {f = g'} step0 in
  let step2 = r (g' x) in
  let step3 = step1 `trans` step2 in
  let step4 = cong {f = f'} step3 in
  step4 `trans` (r' x)

compBij : Bijection b c -> Bijection a b -> Bijection a c
compBij (MkBij f' g' l' r') (MkBij f g l r) = 
  MkBij (f' . f) (g . g') (compBijL f g l f' g' l') (compBijR f g r f' g' r')

compBijAssoc : (f : Bijection a b) -> (g : Bijection b c) -> (h : Bijection c d) ->
               (compBij h (compBij g f)) = (compBij (compBij h g) f)
compBijAssoc (MkBij f0 g0 l0 r0) 
             (MkBij f1 g1 l1 r1) 
             (MkBij f2 g2 l2 r2) = 
  let l0 = compBijL f0 g0 l0 f1 g1 l1 in
  let l1 = compBijL f1 g1 l1 f2 g2 l2 in
  ?compBijAssoc_rhs_2
