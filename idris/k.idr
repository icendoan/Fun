import Data.Vect

%lib C "kdb"
%language TypeProviders
  
namespace Array
  
  ||| An interface for data types that can be serialised as a binary list
  ||| sizeof should be the length of the list
  public export interface Serialisable t where
    sizeof      : Int -- if the size is bigger than an int, we're in trouble
    serialise   : t -> Vect (cast sizeof) Bits8
    deserialise : Vect (cast sizeof) Bits8 -> t
  
  getSizeOf : Serialisable serty => Int
  getSizeOf {serty} = sizeof {t = serty}
  
  Serialisable Bits8 where
    sizeof = 1
    serialise x = [x]
    deserialise [x] = x
  
  data RawArray : Int -> Type -> Type where
    MkRawArray : Serialisable t => Ptr -> RawArray sz t
  
  __alloc : Serialisable t => (n : Int) -> IO (Maybe $ RawArray (n * sizeof) t)
  __alloc {t} count = 
    let malloc = foreign FFI_C "calloc" (Int -> Int -> IO Ptr) in
    let size = sizeof {t = t} in
    do
      ptr <- malloc size count
      ?rest
  
  __free : Serialisable t => RawArray n t -> IO ()
  __free (MkRawArray ptr) = foreign FFI_C "free" (Ptr -> IO ()) ptr

  ||| A continuous block of memory, with values of type t, of length s
  ||| starting at ptr
  export data Array : Nat -> Type -> Type where
    MkArray : Serialisable t => (n : Nat) -> RawArray (sizeof * (cast n)) t -> Array n t
  
  export allocate : {t : Type} -> Serialisable t => (n : Nat) -> Maybe $ Array n t
  allocate {t} n = 
    case (unsafePerformIO $ __alloc {t = t} (cast $ n))  of
     Just ra => Just $ MkArray n ra
     Nothing => Nothing
  
  export free : Serialisable t => Array n t -> ()
  free (MkArray _ ra) = unsafePerformIO $ __free ra

namespace Raw
  
  data Signed : (t : Type) -> (w : t) -> Type -> Type where
    MkSigned : (x : t) -> Signed t w r
  
  implicit __signed_val : Signed t w r -> r
  
  SBits8 : Type
  SBits16 : Type
  SBits32 : Type
  SBits64 : Type
  SBits8 = Signed Int 8 Bits8
  SBits16 = Signed Int 16 Bits16
  SBits32 = Signed Int 32 Bits16
  SBits64 = Signed Int 64 Bits16
  
  S : Type 
  C : Type 
  G : Type 
  H : Type 
  I : Type 
  J : Type 
  F : Type 
  V : Type
  S = String
  C = SBits8
  G = Bits8
  H = SBits16
  I = SBits32
  J = SBits64
  F = Double
  V = ()
  
  data KType = KBool
             | KGuid
             | KByte
             | KShort
             | KInt
             | KLong
             | KReal  -- float - generally unused, idris cannot handle
             | KFloat -- double
             | KChar
             | KSym
             | KTimestamp
             | KMonth
             | KDate
             | KDateTime
             | KTimespan
             | KMinute
             | KSecond
             | KTime
             | KDict
             | KTable
             | KFunc
             | KUnknown
             
  __k_type : Bits8 -> KType
  
  __k_repr : KType -> Bits8
  
  __k_val  : KType -> Type

  data KUnion : (ty : Bits8) -> Type where
    MkG : Bits8 -> KUnion  $ prim__complB8 $ __k_repr KByte
    MkH : Bits16 -> KUnion $ prim__complB8 $ __k_repr KShort
    MkI : Bits32 -> KUnion $ prim__complB8 $ __k_repr KInt
    MkJ : Bits64 -> KUnion $ prim__complB8 $ __k_repr KLong
    --MkE : Float -> KUnion -- never use floats, idris does not support them
    MkF : Double -> KUnion $ prim__complB8 $ __k_repr KFloat
    MkS : String -> KUnion $ __k_repr KChar
    MkKPtr : Ptr -> KUnion ty
    MkV : (len : Bits64) -> Vect (cast . prim__sextB64_BigInt $ len) (__k_val . k_type $ ty) -> KUnion ty
  
  data K : Type where
    MkK : (m, a, t, u : Bits8) -> (r : Int) -> (union : KUnion t) -> K
  
  __read_kptr : Ptr -> IO' FFI_C (Maybe K)
  
  __write_kptr : Ptr -> K -> IO' FFI_C ()
