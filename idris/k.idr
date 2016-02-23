import Data.Vect
import Data.Bits
  
namespace Data.Quotient
 -- data Quotient : (t : Type) -> (relation : t -> t) -> (x : t) -> Type where
 --   MkQuotient : (x : t) -> Quotient t relation (relation x)
  


  NatMod : Nat -> Type
  NatMod n = Subset Nat (\x => LTE x n)
  
  data Equi : Type -> Type where
    MkEqui : (equi : t -> t -> Type) ->
             (reflexive : (x : t) -> equi x x) ->
             (symmetric : (x, y : t) -> equi x y -> equi y x) ->
             (transitive : (x, y, z : t) -> equi x y -> equi y z -> equi x z) ->
             Equi t 

  rel : Equi t -> (t -> t -> Type)
  rel (MkEqui rel _ _ _) = rel
  
  data Quotient : Equi t -> (t : Type)-> Type where
    MkQuotient : (x : t) -> (equi : Equi t) -> (repr : Subset t ((rel equi) x)) -> Quotient equi t
 
  withQuotient : Quotient rel t -> (t -> t) -> t
  withQuotient (MkQuotient x rel repr) f = f (getWitness repr)
  
namespace Data.DateTime
  data Month = January
             | February
             | March
             | April
             | May
             | June
             | July
             | August
             | September
             | October
             | November
             | December
  
  Year : Type
  Year = Int
  
  Hour : Int -> Type
  Hour h = IntMod 24 h
  
  Minute : Int -> Type
  Minute h = IntMod 60 h
  
  numDays : Year -> Month -> Int
  
  record DateTime where
    constructor MkDateTime
    year : Year
    month : Month
    day : IntMod (numDays year month) n
    hours : Hour h
    minutes : Minute m
    nanoseconds : Integer
  
  record Date where
    constructor MkDate
    year : Year
    month : Month
    day : IntMod (numDays year month) n
  
  record Time where
    constructor MkTime
    hours : Hour h
    minutes : Minute m
    nanoseconds : Integer
  
namespace Util
  chunks : Vect (n * m) t -> Vect n (Vect m t)
  chunks {n = n} {m = Z} xs = replicate n []
  chunks {n = Z} {m = (S k)} xs = []
  chunks {n = (S j)} {m = (S k)} (x :: xs) = 
    let head = take k xs in
    let tail = drop k xs in
    (x :: head) :: chunks tail
  
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
  
  __k_prim_ty : KPrimTy -> Type
  __k_prim_ty KBool = Bool
  __k_prim_ty KGuid = Vect 4 Bits32
  __k_prim_ty KByte = Bits8
  __k_prim_ty KShort = Bits16
  __k_prim_ty KInt = Bits32
  __k_prim_ty KLong = Bits64
  __k_prim_ty KReal = Double
  __k_prim_ty KFloat = Double
  __k_prim_ty KChar = Char
  __k_prim_ty KSym = String
  __k_prim_ty KTimestamp = ?__k_prim_ty_rhs_11 -- could use ints?
  __k_prim_ty KMonth = ?__k_prim_ty_rhs_12
  __k_prim_ty KDate = Date
  __k_prim_ty KDateTime = DateTime
  __k_prim_ty KTimespan = ?__k_prim_ty_rhs_15
  __k_prim_ty KMinute = ?__k_prim_ty_rhs_16
  __k_prim_ty KSecond = ?__k_prim_ty_rhs_17
  __k_prim_ty KTime = ?__k_prim_ty_rhs_18
  
  __k_prim_sizeof : KPrimTy -> Nat

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
  
  __k_raw_repr : KPrimTy -> Type
  __k_raw_repr KBool = Bits8
  __k_raw_repr KGuid = Vect 16 Bits8
  __k_raw_repr KByte = Bits8
  __k_raw_repr KShort = Bits16
  __k_raw_repr KInt = Bits32
  __k_raw_repr KLong = Bits64
  __k_raw_repr KReal = Double
  __k_raw_repr KFloat = Double
  __k_raw_repr KChar = Bits8
  __k_raw_repr KSym = String
  __k_raw_repr KTimestamp = Bits64
  __k_raw_repr KMonth = Bits32
  __k_raw_repr KDate = Bits32
  __k_raw_repr KDateTime = Double
  __k_raw_repr KTimespan = Bits64
  __k_raw_repr KMinute = Bits32
  __k_raw_repr KSecond = Bits32
  __k_raw_repr KTime = Bits32
  
  data KUnion : (ty : KTy) -> Type where
    --MkE : Float -> KUnion -- never use floats, idris does not support them
    -- todo: replace free type variable with size based predicate
    MkG : Bits8 -> KUnion ty
    MkH : Bits16 -> KUnion ty
    MkI : Bits32 -> KUnion ty
    MkJ : Bits64 -> KUnion ty
    MkF : Double -> KUnion ty
    MkS : String -> KUnion ty
    MkKPtr : Ptr -> KUnion ty
    MkV : (len : Nat) -> Vect len Bits8 -> KUnion ty
  
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
  
  __read_k_prim : (e : Endian) -> (t : KPrimTy) -> Vect n Bits8 -> Maybe $ __k_raw_repr t
  __read_k_prim e KBool [x] = Just x
  __read_k_prim e KGuid x {n = S$S$S$S$S$S$S$S$S$S$S$S$S$S$S$S$Z} = Just x
  __read_k_prim e KByte [x] = Just x
  __read_k_prim e KShort x@[a,b] = Just $ readBytesShort e x
  __read_k_prim e KInt x@[a,b,c,d] = Just $ readBytesInt e x
  __read_k_prim e KLong x@[a,b,c,d,e',f,g,h] = Just $ readBytesLong e x
  __read_k_prim e KReal x@[a,b,c,d,e',f,g,h] = Just $ believe_me $ readBytesLong e x
  __read_k_prim e KFloat x@[a,b,c,d,e',f,g,h] = Just $ believe_me $ readBytesLong e x
  __read_k_prim e KChar [x] = Just x
  __read_k_prim e KSym xs = Just $ pack $ map (chr . prim__zextB8_Int) xs
  __read_k_prim e KTimestamp x@[a,b,c,d,e',f,g,h] = Just $ readBytesLong e x
  __read_k_prim e KTimespan x@[a,b,c,d,e',f,g,h] = Just $ readBytesLong e x
  __read_k_prim e KMonth x@[a,b,c,d] = Just $ readBytesInt e x
  __read_k_prim e KDate x@[a,b,c,d] = Just $ readBytesInt e x
  __read_k_prim e KDateTime x@[a,b,c,d,e',f,g,h] = Just $ believe_me $ readBytesLong e x
  __read_k_prim e KMinute x@[a,b,c,d] = Just $ readBytesInt e x
  __read_k_prim e KSecond x@[a,b,c,d] = Just $ readBytesInt e x
  __read_k_prim e KTime x@[a,b,c,d] = Just $ readBytesInt e x
  __read_k_prim _ _ _ = Nothing
  
  __interpret_bytes : (t : KPrimTy) -> Vect n Bits8 -> Maybe (KUnion (MkKTy (KList t))) 
  
  __read_union : (e : Endian) -> (ty : KTy) -> Ptr -> Int -> Maybe (KUnion ty)
  __read_union e (MkKTy (KAtom x)) ptr offset = do
    let sz = __k_prim_sizeof x
    let bytes = __read_n_bytes sz ptr offset
    val <- __read_k_prim e x bytes
    case x of
      KBool => Just $ MkG val
      KByte => Just $ MkG val
      KChar => Just $ MkG val
      KShort => Just $ MkH val
      KInt => Just $ MkI val
      KMonth => Just $ MkI val
      KDate => Just $ MkI val
      KMinute => Just $ MkI val
      KSecond => Just $ MkI val
      KTime => Just $ MkI val
      KLong => Just $ MkJ val
      KTimestamp => Just $ MkJ val
      KTimespan => Just $ MkJ val
      KReal => Just $ MkF val
      KFloat => Just $ MkF val
      KDateTime => Just $ MkF val
      KSym => Just $ MkS val

  __read_union e (MkKTy (KList x)) ptr offset = do
    let len = cast . prim__zextB64_BigInt $ prim__peek64 prim__TheWorld ptr offset
    let bytes = __read_n_bytes len ptr (offset + 8)
    return $ MkV len bytes
    
  __read_union e KMixed ptr offset =  ?__read_union_rhs_2
  __read_union e KDict ptr offset =  ?__read_union_rhs_3
  __read_union e KTable ptr offset =  ?__read_union_rhs_4
  __read_union e KFunc ptr offset =  ?__read_union_rhs_5
  __read_union e KUnknown ptr offset =  ?__read_union_rhs_6 

  __read_kptr : Endian -> Ptr -> Maybe K
  __read_kptr e ptr = 
    let [m, a, t, u] = __read_n_bytes 4 ptr 0 in -- 4 bytes
    let r = Bits.readBytesInt e $ __read_n_bytes 4 ptr 4 in -- 4 bytes, fills the word
    let u' = __read_union e (__k_type t) ptr 8 in
    case u' of
      Just union =>  Just (MkK m a t u r union)
      Nothing    =>  Nothing
    
  __write_kptr : Endian -> Ptr -> K -> ()
