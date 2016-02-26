import Data.Vect
import Data.Bits
import Data.Morphisms
  
namespace Data.ZZ -- until this is moved to base  
  ||| An int is either a positive nat, or the negated successor of a nat
  ||| Zero is positive
  data ZZ : Type where
    Pos  : Nat -> ZZ
    NegS : Nat -> ZZ
  
  Eq ZZ where
    (Pos n) == (Pos m) = n == m
    (NegS n) == (NegS m) = n == m
    _ == _ = False
  
  Ord ZZ where
    compare (Pos n) (Pos m) = compare n m
    compare (NegS n) (NegS m) = compare m n
    compare (Pos _) (NegS _) = GT
    compare (NegS _) (Pos _) = LT
  
  Num ZZ where
    a + b = ?plus_zz
    a * b = ?mult_zz
    fromInteger x = ?fromInt_zz
  
  Neg ZZ where
    negate a = ?negate_zz
    a - b = ?sub_zz
    abs a = ?abs_zz
  
  
  
namespace Data.Representation

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
  
namespace Data.Quotient
  data Equivalence : (t -> t -> Type) -> Type where
    MkEqui : (rel : t -> t -> Type) ->
             (refl : (x : t) -> rel x x) ->
             (symm : (x, y : t) -> rel x y -> rel y x) ->
             (trans : (x, y, z : t) -> rel x y -> rel y z -> rel x z) ->
             Equivalence rel
 
  prf : Equivalence rel -> (t -> t -> Type)
  prf {t} (MkEqui rel {t = t} _ _ _) = rel 

  data Quotient : {rel : t -> t -> Type} -> Equivalence rel -> (t : Type) -> Type where
    MkQuotient : (x : t) -> (y ** (prf rel) x y) -> Quotient rel t
  
  infixl 5 //, .//
  (//) : {rel : t -> t -> Type} -> (t : Type) -> Equivalence rel -> Type
  t // rel = Quotient rel t
  
  data IntMod : Int -> Type where
    MkIntMod  : {n : Int} -> Int -> IntMod n
  
  (.//) : {rel : t -> t -> Type} -> (f : t -> t) -> (e : Equivalence rel) -> Quotient e t -> Quotient e t
  (.//) f (MkEqui rel refl symm trans) (MkQuotient x (repr ** prf)) = 
    let y = f repr in
    MkQuotient y (y ** refl y)
  
  Num (IntMod n) where
    (MkIntMod x) + (MkIntMod y) = assert_total $ MkIntMod $ (x + y) `mod` n
    (MkIntMod x) * (MkIntMod y) = assert_total $ MkIntMod $ (x * y) `mod` n
    fromInteger a = assert_total $ MkIntMod ((fromInteger a) `mod` n)
  
  Neg (IntMod n) where
    (MkIntMod x) - (MkIntMod y) = assert_total $ MkIntMod $ (x - y) `mod` n
    abs (MkIntMod x) = assert_total $ MkIntMod $ (abs x) `mod` n
    negate (MkIntMod x) = assert_total $ MkIntMod $ (n - x) `mod` n
  
  Eq (IntMod n) where
    (MkIntMod x) == (MkIntMod y) = assert_total $ let xn = x `mod` n in
                                   let yn = x `mod` n in
                                   let xn' = if xn < 0 then xn + n else xn in
                                   let yn' = if yn < 0 then yn + n else yn in
                                   xn' == yn'
  Ord (IntMod n) where
    compare (MkIntMod x) (MkIntMod y) = assert_total $ let xn = x `mod` n in
                                        let yn = x `mod` n in
                                        let xn' = if xn < 0 then xn + n else xn in
                                        let yn' = if yn < 0 then yn + n else yn in
                                        compare xn' yn'
  
  MaxBound (IntMod n) where
    maxBound = MkIntMod (n - 1)
  
  MinBound (IntMod n) where
    minBound = MkIntMod 0

  Enum (IntMod n) where
    pred (MkIntMod x) = MkIntMod $ if x == n - 1 then 0 else x + 1
    succ (MkIntMod x) = MkIntMod $ if x == 0 then n - 1 else x - 1
    toNat (MkIntMod x) = toNat $ if x < 0 then x + n else x
    fromNat k = MkIntMod $ assert_total (fromNat $ k `mod` (cast n))
  
  intModEq : IntMod j -> IntMod k -> Bool
  intModEq {j} {k} (MkIntMod x) (MkIntMod y) = j == k && x == y
  
  intModCmp : IntMod j -> IntMod k -> Ordering
  intModCmp {j} {k} (MkIntMod x) (MkIntMod y)  = case compare j k of
    EQ => compare x y
    r => r
  
  ||| Expands or contracts the modulus of an IntMod
  intModExt : IntMod j -> IntMod k
  intModExt {k} (MkIntMod x) = MkIntMod (assert_total $ x `mod` k)

namespace Data.DateTime
  Year : Type
  Year = Int

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
  
  numDays : Year -> Month -> Int
  
  Day : Year -> Month -> Type
  Day y m = IntMod (numDays y m)

  Hour : Type
  Hour = IntMod 24
  
  Minute : Type
  Minute = IntMod 60
  
  record DateTime where
    constructor MkDateTime
    year : Year
    month : Month
    day : Day year month
    hour : Hour
    minute : Minute
    nanosecond : Integer
  
  record Date where
    constructor MkDate
    year : Year
    month : Month
    day : Day year month
  
  record Time where
    constructor MkTime
    hours : Hour
    minute : Minute
    nanosecond : Integer
  
  Eq Month where
    January == January = True
    February == February = True
    March == March = True
    April == April = True
    May == May = True
    June == June = True
    July == July = True
    August == August = True
    September == September = True
    October == October = True
    November == November = True
    December == December = True
    _ == _ = False
  
  
  Enum Month where
    pred January = February
    pred February = March
    pred March = April
    pred April = May
    pred May = June
    pred June = July
    pred July = August
    pred August = September
    pred September = October
    pred October = November
    pred November = December
    pred December = January
    succ January = December
    succ February = January
    succ March = February
    succ April = March
    succ May = April
    succ June = May
    succ July = June
    succ August = July
    succ September = August
    succ October = September
    succ November = October
    succ December = November
    toNat January = 1
    toNat February = 2
    toNat March = 3
    toNat April = 4
    toNat May = 5
    toNat June = 6
    toNat July = 7
    toNat August = 8
    toNat September = 9
    toNat October = 10
    toNat November = 11
    toNat December = 12
    fromNat Z = January
    fromNat (S Z) = January
    fromNat (S (S Z)) = February
    fromNat (S (S (S Z))) = March
    fromNat (S (S (S (S Z)))) = April
    fromNat (S (S (S (S (S Z))))) = May
    fromNat (S (S (S (S (S (S Z)))))) = June
    fromNat (S (S (S (S (S (S (S Z))))))) = July
    fromNat (S (S (S (S (S (S (S (S Z)))))))) = August
    fromNat (S (S (S (S (S (S (S (S (S Z))))))))) = September
    fromNat (S (S (S (S (S (S (S (S (S (S Z)))))))))) = October
    fromNat (S (S (S (S (S (S (S (S (S (S (S Z))))))))))) = November
    fromNat (S (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))) = December
    fromNat _ = December
  
  Ord Month where
    compare a b = compare (toNat a) (toNat b)
  
  Num Month where
    x + y = fromNat $ assert_total $ ((toNat x) + (toNat y)) `mod` 12
    x * y = fromNat $ assert_total $ ((toNat x) * (toNat y)) `mod` 12 
    fromInteger x = fromNat (cast x)
  
  Neg Month where
    x - y = fromInteger $ (cast $ toNat x) - (cast $ toNat y)
    negate x = fromInteger $ 13 - (cast $ toNat x)
    abs = id
  
  Eq Date where
    (MkDate year_x month_x day_x) == (MkDate year_y month_y day_y) = 
      case (year_x == year_y, month_x == month_y) of
        (True, True) => intModEq day_x day_y
        _ => False

  Enum Date where
  Ord Date where
    compare a b = case compare (year a) (year b) of
      EQ => case compare (month a) (month b) of
        EQ => intModCmp (day a) (day b)
        r => r
      r => r

  Num Date where
    a + b = MkDate (year a + year b) (month a + month b) ((intModExt $ day a) + (intModExt $ day b))
    a * b = MkDate (year a * year b) (month a * month b) ((intModExt $ day a) * (intModExt $ day b))
    -- number of days since some epoch? Maybe 2000-01-01?
    fromInteger a = assert_total $ let y = a `div` 365 in
                    let d' = a - y in
                    let leaps = 1 + (y `div` 4) in -- 2000 is a leap year
                    let (m ** d) = go (fromInteger y) (d' - leaps) 1 in
                    MkDate (fromInteger y) m d
      where
        go : (y : Year) -> Integer -> Nat -> (m : Month ** IntMod (numDays y m))
        go y d m = if (fromInteger d) < (numDays y (fromNat m))
                   then (fromNat m ** MkIntMod (fromInteger d))
                   else let d' = (fromInteger d) - numDays y (fromNat m) in
                        go y (cast d') (S m)

  Neg Date where
    negate (MkDate year month (MkIntMod x)) = 
      let y = negate year in
      let m = negate month in
      let d = the (IntMod (numDays y m)) $ fromInteger (negate (cast x)) in
      MkDate y m d
    a - b = a + (negate b)
    abs (MkDate year month day) = ?absdate_1
  
  Eq Time where
  Enum Time where
  Ord Time where
  Num Time where
  Neg Time where

  Eq DateTime where
  Enum DateTime where
  Ord DateTime where
  Num DateTime where
  Neg DateTime where
  
  interface Temporal t where
    years : t -> t -> Integer
    months : t -> t -> Integer
    days : t -> t -> Integer
    hours : t -> t -> Integer
    minutes : t -> t -> Integer
    
  seconds : Temporal t => t -> t -> Integer
  seconds x y = (minutes x y) * 60
  millis  : Temporal t => t -> t -> Integer
  millis x y = (seconds x y) * 1000
  micros  : Temporal t => t -> t -> Integer
  micros x y = (millis x y) * 10
  nanos   : Temporal t => t -> t -> Integer
  nanos x y = (micros x y) * 10

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
