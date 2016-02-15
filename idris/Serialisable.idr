import Data.Vect

||| An interface for data types that can be serialised as a binary list
||| sizeof should be the length of the list
public export interface Serialisable t where
  sizeof      : Int -- if the size is bigger than an int, we're in trouble
  serialise   : t -> Vect (cast sizeof) Bits8
  deserialise : Vect (cast sizeof) Bits8 -> t
  
export Serialisable Bits8 where
  sizeof = 1
  serialise x = [x]
  deserialise [x] = x
  
export Serialisable Bits16 where
  sizeof = 2
  serialise x = [prim__truncB16_B8 $ prim__lshrB16 x 8, prim__truncB16_B8 x]
  deserialise [a, b] = prim__addB16 (prim__shlB16 (prim__sextB8_B16 a) 8) (prim__sextB8_B16 b)

