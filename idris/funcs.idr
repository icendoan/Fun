-- need the inverse to be b->Maybe a as f could be inclusion
data Injection : (a, b : Type) -> Type where
  MkInj : (f : a -> b) -> (g : b -> a) -> ((x : a) -> (g . f) x = x) -> Injection a b
  
data Surjection : (a, b : Type) -> Type where
  MkSur : (f : a -> b) -> (g : b -> a) -> ((x : b) -> (f . g) x = x) -> Surjection a b
  
data Bijection : (a, b : Type) -> Type where
  MkBij : (f : a -> b) -> (g : b -> a) -> ((x : a) -> (g . f) x = x) -> ((x : b) -> (f . g) x = x) -> Bijection a b
  
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

Main.Semigroup (a -> a) where
  (<>) = (.)
  sg_assoc x y z = compAssoc z y x 
