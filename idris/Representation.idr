
  ||| A pair of an abstract, erased type and a core representation
  ||| along with an application function that lifts any change to the 
  ||| abstract type to the representation
  data Representation : Type -> Type -> Type where
    Repr : .(abst : a) -> 
           (concrete : b) -> 
           (ap : (a -> a) -> (b -> b)) -> Representation a b
  
  applyRepr : (a -> a) -> Representation a b -> Representation a b
  applyRepr f (Repr abst conc ap) = Repr (f abst) (ap f $ conc) ap
  
  infixr 5 |>
  (|>) : (a -> a) -> Representation a b -> Representation a b
  (|>) = applyRepr
 
