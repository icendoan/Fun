import Data.Vect
import Data.Bits

namespace Bits

  ||| This is to be put into Data.Bits at some point
  data Endian = BigEndian | LittleEndian
  
  readBytesShort : Endian -> Vect 2 Bits8 -> Bits16
  readBytesShort BigEndian bytes = 
    let [a, b] = map (prim__zextB8_B16) bytes in
    prim__addB16 (prim__shlB16 a 8) b
  readBytesShort LittleEndian bytes = 
    let [a, b] = map (prim__zextB8_B16) bytes in
    prim__addB16 (prim__shlB16 b 8) a

  readBytesInt   : Endian -> Vect 4 Bits8 -> Bits32
  readBytesInt BigEndian bytes = 
    let [d, c, b, a] = map (prim__zextB8_B32) bytes in
    (prim__shlB32 d 24) + (prim__shlB32 c 16) + (prim__shlB32 b 8) + a
  readBytesInt LittleEndian bytes = 
    let [a, b, c, d] = map (prim__zextB8_B32) bytes in
    (prim__shlB32 d 24) + (prim__shlB32 c 16) + (prim__shlB32 b 8) + a

  readBytesLong  : Endian -> Vect 8 Bits8 -> Bits64
  readBytesLong BigEndian bytes = 
    let bytes = the (Vect 8 Bits64) $ map (prim__zextB8_B64) bytes in
    sum . map (uncurry prim__shlB64) . zip (map (*8) [7,6,5,4,3,2,1,0]) $ bytes

  readBytesLong LittleEndian bytes = 
    let bytes = the (Vect 8 Bits64) $ map (prim__zextB8_B64) bytes in
    sum . map (uncurry prim__shlB64) . zip (map (*8) [0,1,2,3,4,5,6,7]) $ bytes
  
%lib C "kdb"
namespace Raw
  
  S : Type 
  C : Type 
  G : Type 
  H : Type 
  I : Type 
  J : Type 
  F : Type 
  V : Type
  S = String
  C = Bits8
  G = Bits8
  H = Bits16
  I = Bits32
  J = Bits64
  F = Double
  V = ()
  
  data KPrimTy = KBool 
              | KGuid 
              | KByte 
              | KShort 
              | KInt 
              | KLong 
              | KReal
              | KFloat 
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

  data KRawTy = KAtom KPrimTy | KList KPrimTy
  
  data KTy = MkKTy KRawTy
           | KMixed
           | KDict
           | KTable
           | KFunc
           | KUnknown
             
  __k_type : Bits8 -> KTy
  __k_type x = case x of
    0 => KMixed
    x => KUnknown
  
  __k_repr : KRawTy -> Bits8
  
  data KUnion : (ty : KTy) -> Type where
    --MkE : Float -> KUnion -- never use floats, idris does not support them
    MkG : Bits8 -> KUnion   $ MkKTy $ KAtom KByte 
    MkH : Bits16 -> KUnion  $ MkKTy $ KAtom KShort 
    MkI : Bits32 -> KUnion  $ MkKTy $ KAtom KInt 
    MkJ : Bits64 -> KUnion  $ MkKTy $ KAtom KLong 
    MkF : Double -> KUnion  $ MkKTy $ KAtom KFloat 
    MkS : String -> KUnion  $ MkKTy $ KList KChar 
    MkKPtr : Ptr -> KUnion ty
    MkV : (len : Bits64) -> Vect (cast . prim__sextB64_BigInt $ len) (__k_val . k_type $ ty) -> KUnion ty
  
  data K : Type where
    MkK : (m, a, t, u : Bits8) -> (r : Bits32) -> (union : KUnion (__k_type t)) -> K
  
  __read_n_bytes : (n : Nat) -> Ptr -> Int -> Vect n Bits8
  __read_n_bytes Z ptr offset = []
  __read_n_bytes (S k) ptr offset = 
    (prim__peek8 prim__TheWorld ptr offset) :: __read_n_bytes k ptr (offset + 1)
  
  __write_bytes : Ptr -> Int -> Vect n Bits8 -> Int
  __write_bytes ptr offset [] = -1
  __write_bytes ptr offset (x :: []) = prim__poke8 prim__TheWorld ptr offset x
  __write_bytes ptr offset (x :: (y :: xs)) = 
    let r = (prim__poke8 prim__TheWorld ptr offset x) in
    min r (__write_bytes ptr (offset + 1) (y :: xs))
  
  __interpret_bytes : (t : KPrimTy) -> Vect n Bits8 -> Maybe (KUnion (MkKTy (KList t))) 
  
  __read_union : (ty : KTy) -> Ptr -> Int -> Maybe (KUnion ty)
  __read_union (MkKTy (KAtom x)) ptr offset =  ?__read_union_rhs_7
  __read_union (MkKTy (KList x)) ptr offset =  ?__read_union_rhs_8
  __read_union KMixed ptr offset =  ?__read_union_rhs_2
  __read_union KDict ptr offset =  ?__read_union_rhs_3
  __read_union KTable ptr offset =  ?__read_union_rhs_4
  __read_union KFunc ptr offset =  ?__read_union_rhs_5
  __read_union KUnknown ptr offset =  ?__read_union_rhs_6 

  __read_kptr : Ptr -> Maybe K
  __read_kptr ptr = 
    let [m, a, t, u] = __read_n_bytes 4 ptr 0 in -- 4 bytes
    let r = Bits.readBytesInt LittleEndian $ __read_n_bytes 4 ptr 4 in -- 4 bytes, fills the word
    let u' = __read_union (__k_type t) ptr 8 in
    case u' of
      Just union =>  Just (MkK m a t u r union)
      Nothing    =>  Nothing
    

  __write_kptr : Ptr -> K -> ()
