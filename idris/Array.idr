import Data.Vect
import Serialisable

namespace Array
          -- this is really ugly
          -- should probably done with the Memory EFFECT instead?
          -- or just not bother, and just serialise directly?

   
  ||| A wrapper around a pointer to a heap-allocated array of ```sz``` bytes
  data RawArray : Int -> Type -> Type where
    MkRawArray : {t : Type} -> {sz : Int} -> Serialisable t => Ptr -> RawArray sz t

  ||| Unsafely writes the bytes as given to the ptr
  ||| Does absolutely no bounds/null checking
  __write_bytes : Vect n Bits8 -> Ptr -> Ptr

  ||| Serialises and writes the t to the array, starting at idx
  ||| Returns Nothing if the write would go out of bounds
  ||| Terribly unsafe otherwise
  __write : Serialisable t 
            => RawArray sz t 
            -> t 
            -> (idx : Int)
            -> Maybe $ RawArray sz t
  
  ||| Writes as many copies of the serialised t to the array as possible
  ||| Returns Nothing if the write would go out of bounds
  __write_all : {sz : Int} -> Serialisable t => RawArray sz t -> t -> Maybe $ RawArray sz t
  __write_all (MkRawArray ptr {sz}) init = 
    case (go (serialise init) ptr 0 sz) of
      Nothing => Nothing
      (Just x) => Just (MkRawArray x)
    where
      go : Vect n Bits8 -> Ptr -> Int -> Int -> Maybe Ptr
      go {n} ser ptr offset bound = 
        case (bound == offset, bound > offset) of
          (True, _) => Just ptr
          (False, True) => go ser (__write_bytes ser ptr) (offset + (cast n)) bound
          (False, False) => Nothing
  
  __alloc : Serialisable t => (n, sz : Int) 
                           -> t
                           -> IO' FFI_C (Maybe $ RawArray (sz * n) t)
  __alloc {t} count size init = 
    let malloc = foreign FFI_C "calloc" (Int -> Int -> IO' FFI_C Ptr) in
    do
      ptr <- malloc size count
      case ptr == prim__null of
        True  => return Nothing
        False => return $ __write_all (the (RawArray (size * count) t) (MkRawArray ptr)) init 
  
  __free : {t : Type} -> {n : Int} -> Serialisable t => RawArray n t -> IO ()
  __free (MkRawArray ptr) = foreign FFI_C "free" (Ptr -> IO ()) ptr

  ||| A continuous block of memory, with values of type t, of length n
  ||| starting at ptr
  export data Array : Nat -> Type -> Type where
    MkArray : Serialisable t => (n : Nat) -> RawArray (sizeof * (cast n)) t -> Array n t
  
  export allocate : Serialisable t => (n : Nat) -> Maybe $ Array n t
  --allocate {t} n = let sz = (sizeof {t = t}) in
  --  case (unsafePerformIO $ __alloc {t = t} (cast n) sz) of
  --    Nothing => Nothing
  --    (Just x) => Just $ MkArray n x
  
  export free : Serialisable t => Array n t -> ()
  free (MkArray _ ra) = unsafePerformIO $ __free ra
  
  export write : Serialisable t => Array n t 
                                -> (idx : Nat) 
                                -> {auto in_bounds : LTE idx n}
                                -> t
                                -> Array n t
  
  export read : Serialisable t => Array n t
                               -> (idx : Nat)
                               -> {auto in_bounds : LTE idx n}
                               -> t
