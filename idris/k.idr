module K

import Data.Vect
import Data.Bits
import Data.Morphisms
import Representation
import Quotient
import DateTime


namespace Util
  chunks : Vect (n * m) t -> Vect n (Vect m t)
  chunks {n = n} {m = Z} xs = replicate n []
  chunks {n = Z} {m = (S k)} xs = []
  chunks {n = (S j)} {m = (S k)} (x :: xs) = 
    let head = take k xs in
    let tail = drop k xs in
    (x :: head) :: chunks tail
  
  --split : (m : Nat) -> Vect (m + n) t -> (Vect m t, Vect n t)
  --split m v = (take m v, drop m v)
  
  split : (tk : Nat) -> Vect k t -> Maybe (x : Nat ** (Vect tk t, Vect x t))
  split (S k) [] = Nothing
  split (S k) (x::xs) = do
    (c ** (head, tail)) <- split k xs
    ?something
  split Z v = Just (_ ** ([], v))
  
  -- if using pointers, all type safety goes out the window anyway
  data TypedPtr : Type -> Type where
    WrapPtr : Ptr -> TypedPtr t
  
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
  
  ||| Interpret a list of Bits8 as a string
  asciiStr : List Bits8 -> String
  asciiStr = pack . map (prim__intToChar . prim__zextB8_Int)
  
  ||| Read a pointer as a character array for a number of (ascii!) characters
  readCStar : Ptr -> (len : Int) -> String
  readCStar ptr len = asciiStr $ map (prim__peek8 prim__TheWorld ptr) [0, 8 .. 8*len]
  
  -- literally the same as for(char c=*ptr;c!=0;ptr++)
  ||| Read an (ascii!) C string until the null terminator
  readCStr : Ptr -> String
  readCStr ptr = asciiStr (go ptr 0)
    where
      go : Ptr -> Int -> List Bits8
      go ptr offset = 
        case (prim__peek8 prim__TheWorld ptr offset) of
          0 => []
          b => b :: go ptr (offset + 1)
  
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
  
  public export data KPrimTy = KBool 
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
  __k_prim_ty KGuid = Vect 16 Bits8
  __k_prim_ty KByte = Bits8
  __k_prim_ty KShort = Bits16
  __k_prim_ty KInt = Bits32
  __k_prim_ty KLong = Bits64
  __k_prim_ty KReal = Double
  __k_prim_ty KFloat = Double
  __k_prim_ty KChar = Char
  __k_prim_ty KSym = String
  __k_prim_ty KTimestamp = Bits64
  __k_prim_ty KMonth = Bits32
  __k_prim_ty KDate = Bits32
  __k_prim_ty KDateTime = Double
  __k_prim_ty KTimespan = Bits64
  __k_prim_ty KMinute = Bits32
  __k_prim_ty KSecond = Bits32
  __k_prim_ty KTime = Bits32
  
  -- no signed bits8 type, so just use abs and compl later
  __k_prim_repr : KPrimTy -> Bits8
  __k_prim_repr KBool = 1
  __k_prim_repr KGuid = 2
  __k_prim_repr KByte = 4
  __k_prim_repr KShort = 5
  __k_prim_repr KInt = 6
  __k_prim_repr KLong = 7
  __k_prim_repr KReal = 8 -- unused
  __k_prim_repr KFloat = 9
  __k_prim_repr KChar = 10
  __k_prim_repr KSym = 11
  __k_prim_repr KTimestamp = 12
  __k_prim_repr KMonth = 13
  __k_prim_repr KDate = 14
  __k_prim_repr KDateTime = 15
  __k_prim_repr KTimespan = 16
  __k_prim_repr KMinute = 17
  __k_prim_repr KSecond = 18
  __k_prim_repr KTime = 19
  
  __k_prim_sizeof : KPrimTy -> Nat
  __k_prim_sizeof KBool = 1
  __k_prim_sizeof KGuid = 16
  __k_prim_sizeof KByte = 1
  __k_prim_sizeof KShort = 2
  __k_prim_sizeof KInt = 4
  __k_prim_sizeof KLong = 8
  __k_prim_sizeof KReal = 4
  __k_prim_sizeof KFloat = 8
  __k_prim_sizeof KChar = 1
  __k_prim_sizeof KSym = 8 -- sizeof (char*)
  __k_prim_sizeof KTimestamp = 8
  __k_prim_sizeof KMonth = 4
  __k_prim_sizeof KDate = 4
  __k_prim_sizeof KDateTime = 8
  __k_prim_sizeof KTimespan = 8
  __k_prim_sizeof KMinute = 4
  __k_prim_sizeof KSecond = 4
  __k_prim_sizeof KTime = 4

  public export data KRawTy = KAtom KPrimTy | KList KPrimTy
  
  public export data KTy = MkKTy KRawTy
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
  __k_repr (KAtom x) = prim__complB8 $ __k_prim_repr x
  __k_repr (KList x) = __k_prim_repr x
  
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
    --MkV : (len : Nat) -> Vect len Bits8 -> KUnion ty -- deprecated in favour of a shallow copy
                                                       -- since vectors can be very large
    MkV : (len : Nat) -> Ptr -> (offset : Int) -> KUnion ty -- use __read_bytes to access the union
  
  data K : Type where
    MkK : (m, a, t, u : Bits8) -> (r : Bits32) -> (union : KUnion (__k_type t)) -> K
  
  KPtr : Type 
  KPtr = TypedPtr K
  
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
  
  ||| Desperately unsafe, uses lots of unsafe casts
  __interpret_bytes : (t : KPrimTy) -> Vect n Bits8 -> Maybe (List (__k_prim_ty t))
  __interpret_bytes t v = do
      let size = __k_prim_sizeof t
      (_ ** (chunk, rest)) <- split size v
      results <- __interpret_bytes t (assert_smaller v rest)
      let val = case size of
        S$Z => really_believe_me chunk
        S$S$Z => really_believe_me . readBytesShort LittleEndian $ chunk
        S$S$S$S$Z => really_believe_me . readBytesInt LittleEndian $ chunk
        S$S$S$S$S$S$S$S$Z => really_believe_me . readBytesLong LittleEndian $ chunk
        S$S$S$S$S$S$S$S$S$S$S$S$S$S$S$S$Z => really_believe_me chunk -- this is actually just id
        _ => really_believe_me chunk
      return $ val :: results
      

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
    --let bytes = __read_n_bytes len ptr (offset + 8)
    --return $ MkV len bytes
    return $ MkV len ptr (offset + 8)
    
  -- a mixed list a list of kptrs
  __read_union e KMixed ptr offset = do
    let len = cast . prim__zextB64_BigInt $ prim__peek64 prim__TheWorld ptr offset
    -- let bytes = __read_n_bytes len ptr (offset + 8)
    -- return $ MkV len bytes
    return $ MkV len ptr (offset + 8)
  
  -- a dictionary is a list of two kptrs - keys and values
  __read_union e KDict ptr offset = do
    -- guaranteed to be 2 long
    -- let bytes = __read_n_bytes 2 ptr (offset + 8)
    -- return $ MkV 2 bytes
    return $ MkV 2 ptr (offset + 8)
  
  -- a table is a ptr to a dict
  __read_union e KTable ptr offset = 
    let kptr = prim__peekPtr prim__TheWorld ptr offset in
    Just $ MkKPtr kptr 
  -- a lambda is a char-array (type 100)
  -- others are unknown
  __read_union e KFunc ptr offset =  ?__read_union_rhs_5

  -- no clue here
  __read_union e KUnknown ptr offset = Nothing

  -- use this to just check the details of a given k struct
  -- shallowly reads the kptr into a K type
  -- does not inspect any sub-ptrs
  -- does not check for correctness
  __read_kptr : Endian -> KPtr -> Maybe K
  __read_kptr e (WrapPtr ptr) = 
    let [m, a, t, u] = __read_n_bytes 4 ptr 0 in -- 4 bytes
    let r = Bits.readBytesInt e $ __read_n_bytes 4 ptr 4 in -- 4 bytes, fills the word
    let u' = __read_union e (__k_type t) ptr 8 in
    case u' of
      Just union =>  Just (MkK m a t u r union)
      Nothing    =>  Nothing
    
  -- shallowly writes the union to the ptr1
  __write_union : Endian -> KPtr -> Int -> KUnion t -> ()
  -- shallowly writes the K to the ptr
  __write_kptr : Endian -> KPtr -> Int -> K -> ()
  
  -- these are idris-land bindings 
  -- but they are shallow, and do no copying
  
  -- utility function to unwrap kptrs from ffi calls
  kextr : IO Ptr -> KPtr
  kextr = WrapPtr . unsafePerformIO
  
  -- gc functions
  ||| call the kdb garbage collector
  __k_m9 : () -> IO ()
  __k_m9 () = foreign FFI_C "m9" (() -> IO ()) ()
  
  ||| decrement refcount
  __k_r0 : KPtr -> IO V
  __k_r0 (WrapPtr ptr) = (foreign FFI_C "r0" (Ptr -> IO ())) ptr

  ||| increment refcount
  __k_r1 : KPtr -> IO KPtr
  __k_r1 (WrapPtr ptr) = map WrapPtr $ foreign FFI_C "r1" (Ptr -> IO Ptr) ptr

  -- remote connection functions
  
  ||| connect with authentication and timeout
  __k_khpun : (addr : S) -> (port : I) -> (auth : S) -> (timeout : I) -> IO I
  __k_khpun = foreign FFI_C "khpun" (S -> I -> S -> I -> IO I) 
  
  ||| connect with authentication
  __k_khpu : (addr : S) -> (port : I) -> (auth : S) -> IO I
  __k_khpu = foreign FFI_C "khpu" (S -> I -> S -> IO I) 
  
  ||| just connect with an address and port
  __k_khp : (addr : S) -> (port : I) -> IO I
  __k_khp = foreign FFI_C "khp" (S -> I -> IO I)
  
  ||| close the socket obtained by the previous functions
  __k_kclose : (handle : I) -> IO V
  __k_kclose = foreign FFI_C "kclose" (I -> IO ())
  
  -- move to a kptr some how
  -- expand the params as well
  ||| send a string to the remote k server
  ||| and wait for a response
  ||| Actually a variadic function, this has been expanded to match on up to 8 arguments,
  ||| since that is all that may be used in a q function anyway
  __k_k : (handle : I) -> (src : S) -> (args : Vect n KPtr) -> {sm : LTE n 8} -> KPtr
  __k_k handle src [] = kextr $  foreign FFI_C "k" (I -> S -> IO Ptr) 
    {fty = FFun (C_IntT C_IntBits32) $ FFun (C_Str) $ FRet (C_Ptr)} handle src
  __k_k handle src [(WrapPtr a)] = kextr $  foreign FFI_C "k" (I -> S -> Ptr -> IO Ptr) handle src a
  __k_k handle src [(WrapPtr a), (WrapPtr b)] = kextr $ foreign FFI_C "k" (I -> S -> Ptr -> Ptr -> IO Ptr) handle src a b
  __k_k handle src [(WrapPtr a), (WrapPtr b), (WrapPtr c)] = kextr $ foreign FFI_C "k" (I -> S -> Ptr -> Ptr -> Ptr -> IO Ptr) handle src a b c
  __k_k handle src [(WrapPtr a), (WrapPtr b), (WrapPtr c), (WrapPtr d)] = kextr $ foreign FFI_C "k" (I -> S -> Ptr -> Ptr -> Ptr -> Ptr -> IO Ptr) handle src a b c d
  __k_k handle src [(WrapPtr a), (WrapPtr b), (WrapPtr c), (WrapPtr d), (WrapPtr e)] = kextr $ foreign FFI_C "k" (I -> S -> Ptr -> Ptr -> Ptr -> Ptr -> Ptr -> IO Ptr) handle src a b c d e
  __k_k handle src [(WrapPtr a), (WrapPtr b), (WrapPtr c), (WrapPtr d), (WrapPtr e), (WrapPtr f)] = kextr $ foreign FFI_C "k" (I -> S -> Ptr -> Ptr -> Ptr -> Ptr -> Ptr -> Ptr -> IO Ptr) handle src a b c d e f
  __k_k handle src [(WrapPtr a), (WrapPtr b), (WrapPtr c), (WrapPtr d), (WrapPtr e), (WrapPtr f), (WrapPtr g)] = kextr $ foreign FFI_C "k" (I -> S -> Ptr -> Ptr -> Ptr -> Ptr -> Ptr -> Ptr -> Ptr -> IO Ptr) handle src a b c d e f g
  __k_k handle src [(WrapPtr a), (WrapPtr b), (WrapPtr c), (WrapPtr d), (WrapPtr e), (WrapPtr f), (WrapPtr g), (WrapPtr h)] = kextr $ foreign FFI_C "k" (I -> S -> Ptr -> Ptr -> Ptr -> Ptr -> Ptr -> Ptr -> Ptr -> Ptr -> IO Ptr) handle src a b c d e f g h
  
  
  ||| checks if a byte stream (type: 4) is a valid IPC stream or not
  __k_okx : (stream : KPtr) -> I
  __k_okx (WrapPtr ptr) = unsafePerformIO $ foreign FFI_C "okx" (Ptr -> IO I) ptr

  -- usable only from a library loaded into kdb
  -- ||| Remove callback
  -- __k_sd0 : I -> IO V
  -- __k_sd0 = foreign FFI_C "sd0" (I -> IO V)

  -- ||| Add callback
  -- __k_sd1 : I -> (I -> KPtr) -> IO KPtr
  -- __k_sd1 = foreign FFI_C "sd1" (I -> (I -> KPtr) -> IO KPtr)
 
  -- Variadic functions are not supported
  -- ||| Dynamically link a function.
  -- |||
  -- ||| Takes a function taking n K objects, returning another K object, and builds it into a Q function
  -- __k_dl : fty -> I -> KPtr

  -- data conversion functions

  ||| Year/month/day -> Int
  __k_ymd : (years : I) -> (months : I) -> (days : I) -> I
  __k_ymd years months days = unsafePerformIO $ foreign FFI_C "ymd" (I -> I -> I -> IO I) years months days

  
  -- data creation functions

  ||| Create a date from an int
  __k_dj : (days : I) -> I
  __k_dj days = unsafePerformIO $ foreign FFI_C "dj" (I -> IO I) days

 
  ||| Intern n chars from a string
  __k_sn : S -> I -> S
  __k_sn str count = unsafePerformIO $ foreign FFI_C "sn" (S -> I -> IO S) str count
  
  ||| Intern a string
  __k_ss : S -> S
  __k_ss str = unsafePerformIO $ foreign FFI_C "ss" (S -> IO S) str

  ||| Create a timestamp
  __k_ktj : (ty : I) -> (val : J) -> KPtr
  __k_ktj ty val = WrapPtr . unsafePerformIO $ foreign FFI_C "ktj" (I -> J -> IO Ptr) ty val

  ||| Create an atom of type ```ty```
  __k_ka : (ty : I) -> KPtr
  __k_ka ty = kextr $ foreign FFI_C "ka" (I -> IO Ptr) ty
  
  ||| Create a bool, C style
  __k_kb : (val : I) -> KPtr
  __k_kb val = kextr $ foreign FFI_C "kb" (I -> IO Ptr) val
  
  ||| Create a byte
  __k_kg : I -> KPtr
  __k_kg val = kextr $ foreign FFI_C "kg" (I -> IO Ptr) val

  ||| Create a short
  __k_kh : I -> KPtr
  __k_kh val = kextr $ foreign FFI_C "kh" (I -> IO Ptr) val
  
  ||| Create an int
  __k_ki : I -> KPtr
  __k_ki val = kextr $ foreign FFI_C "ki" (I -> IO Ptr) val
  
  ||| Create a long
  __k_kj : J -> KPtr
  __k_kj val = kextr $ foreign FFI_C "kj" (J -> IO Ptr) val
  
  ||| Create a real
  __k_ke : F -> KPtr
  __k_ke val = kextr $ foreign FFI_C "ke" (F -> IO Ptr) val

  ||| Create a float
  __k_kf : F -> KPtr
  __k_kf val = kextr $ foreign FFI_C "kf" (F -> IO Ptr) val
  
  ||| Create a char
  __k_kc : I -> KPtr
  __k_kc val = kextr $ foreign FFI_C "kc" (I -> IO Ptr) val
  
  ||| Create a symbol
  __k_ks : S -> KPtr
  __k_ks val = kextr $ foreign FFI_C "ks" (S -> IO Ptr) val
  
  ||| Create a date
  __k_kd : I -> KPtr
  __k_kd val = kextr $ foreign FFI_C "kd" (I -> IO Ptr) val
  
  ||| Create a datetime
  __k_kz : F -> KPtr
  __k_kz val = kextr $ foreign FFI_C "kz" (F -> IO Ptr) val
  
  ||| Create a time
  __k_kt : I -> KPtr
  __k_kt val = kextr $ foreign FFI_C "kt" (I -> IO Ptr) val

  ||| Create a mixed list
  ||| Actually a variadic function, this has been expanded to take up to 8 arguments
  ||| Try to just create a mixed list of required length and join elements to it instead
  -- what a monstrosity
  __k_knk : (length : I) -> (values : Vect n KPtr) -> {sm : LTE n 8} -> KPtr
  __k_knk n [] = kextr $ foreign FFI_C "knk" (I -> IO Ptr) n
  __k_knk n [(WrapPtr a), (WrapPtr b)] = kextr $ foreign FFI_C "knk" (I -> Ptr -> IO Ptr) n a
  __k_knk n [(WrapPtr a), (WrapPtr b), (WrapPtr c)] = kextr $ foreign FFI_C "knk" (I -> Ptr -> Ptr -> Ptr -> IO Ptr) n a b c
  __k_knk n [(WrapPtr a), (WrapPtr b), (WrapPtr c), (WrapPtr d)] = kextr $ foreign FFI_C "knk" (I -> Ptr -> Ptr -> Ptr -> Ptr -> IO Ptr) n a b c d
  __k_knk n [(WrapPtr a), (WrapPtr b), (WrapPtr c), (WrapPtr d), (WrapPtr e)] = kextr $ foreign FFI_C "knk" (I -> Ptr -> Ptr -> Ptr -> Ptr -> Ptr -> IO Ptr) n a b c d e
  __k_knk n [(WrapPtr a), (WrapPtr b), (WrapPtr c), (WrapPtr d), (WrapPtr e), (WrapPtr f)] = kextr $ foreign FFI_C "knk" (I -> Ptr -> Ptr -> Ptr -> Ptr -> Ptr -> Ptr -> IO Ptr) n a b c d e f
  __k_knk n [(WrapPtr a), (WrapPtr b), (WrapPtr c), (WrapPtr d), (WrapPtr e), (WrapPtr f), (WrapPtr g)] = kextr $ foreign FFI_C "knk" (I -> Ptr -> Ptr ->  Ptr -> Ptr -> Ptr -> Ptr ->Ptr -> IO Ptr) n a b c d e f g
  __k_knk n [(WrapPtr a), (WrapPtr b), (WrapPtr c), (WrapPtr d), (WrapPtr e), (WrapPtr f), (WrapPtr g), (WrapPtr h)] = kextr $ foreign FFI_C "knk" (I -> Ptr ->  Ptr ->Ptr -> Ptr -> Ptr -> Ptr -> Ptr -> Ptr -> IO Ptr) n a b c d e f g h
  __k_knk n [(WrapPtr a), (WrapPtr b), (WrapPtr c), (WrapPtr d), (WrapPtr e), (WrapPtr f), (WrapPtr g), (WrapPtr h), (WrapPtr i)] = kextr $ foreign FFI_C "knk" (I -> Ptr -> Ptr -> Ptr -> Ptr -> Ptr -> Ptr -> Ptr -> Ptr -> Ptr -> IO Ptr) n a b c d e f g h i

  ||| Create a string
  __k_kp : S -> KPtr
  __k_kp val = kextr $ foreign FFI_C "kp" (S -> IO Ptr) val
  
  ||| Create a string with length n
  __k_kpn : S -> J -> KPtr
  __k_kpn val len = kextr $ foreign FFI_C "kpn" (S -> J -> IO Ptr) val len
  
  ||| Join an atom to a list
  ||| First parameter is the list, second is a ptr to a primitive C type to add.
  ||| This is unlikely to be used.
  __k_ja : KPtr -> Ptr -> KPtr 
  __k_ja (WrapPtr list) val = kextr $ foreign FFI_C "ja" (Ptr -> Ptr -> IO Ptr) list val
  
  ||| Join a string to a list
  __k_js : KPtr -> S -> KPtr
  __k_js (WrapPtr list) str = kextr $ foreign FFI_C "js" (Ptr -> S -> IO Ptr) list str
  
  ||| Join an arbitrary k object to a list
  __k_jk : (list : KPtr) -> (obj : KPtr) -> KPtr
  __k_jk (WrapPtr list) (WrapPtr obj) = kextr $ foreign FFI_C "jk" (Ptr -> Ptr -> IO Ptr) list obj
  
  ||| Join two lists
  __k_jv : KPtr -> KPtr -> KPtr
  __k_jv (WrapPtr head) (WrapPtr tail) = kextr $ foreign FFI_C "jv" (Ptr -> Ptr -> IO Ptr) head tail
  
  ||| Create a table from a dictionary (flip)
  __k_xT : KPtr -> KPtr
  __k_xT (WrapPtr dict) = kextr $ foreign FFI_C "xT" (Ptr -> IO Ptr) dict
  
  ||| Build a dictionary from two klists
  __k_xD : KPtr -> KPtr -> KPtr
  __k_xD (WrapPtr keys) (WrapPtr vals) = kextr $ foreign FFI_C "xD" (Ptr -> Ptr -> IO Ptr) keys vals
  
  ||| Create a simple table from a keyed table
  __k_ktd : KPtr -> KPtr
  __k_ktd (WrapPtr table) = kextr $ foreign FFI_C "ktd" (Ptr -> IO Ptr) table
  
  ||| Raise an error
  __k_krr : S -> KPtr
  __k_krr err = kextr $ foreign FFI_C "krr" (S -> IO Ptr) err
  
  ||| Raise a system error
  __k_orr : S -> KPtr
  __k_orr err = kextr $ foreign FFI_C "orr" (S -> IO Ptr) err
  
  ||| Apply the first parameter to the second parameter as a . function
  ||| May not be present unless loaded as a library
  __k_dot : KPtr -> KPtr -> KPtr
  __k_dot (WrapPtr fn) (WrapPtr args) = kextr $ foreign FFI_C "dot" (Ptr -> Ptr -> IO Ptr) fn args
  
  ||| Serialise an object
  ||| First parameter is capability
  __k_b9 : I -> KPtr -> KPtr
  __k_b9 cap (WrapPtr val) = kextr $ foreign FFI_C "b9" (I -> Ptr -> IO Ptr) cap val
  
  ||| Deserialise an object
  __k_d9 : KPtr -> KPtr
  __k_d9 (WrapPtr stream) = kextr $ foreign FFI_C "d9" (Ptr -> IO Ptr) stream

-- Representations of q datatypes
-- They are all just wrappers around k ptrs
-- and should therefore hide their constructors

public export data KAttrs = AttrS
                          | AttrU
                          | AttrP
                          | AttrG
  
||| A K table is parametrised by its length, and the name and type of each of its columns, and 
||| then by any attributes.
export data KTable : List KAttrs -> Nat -> Vect n (String, KTy) -> Type where
  MkKTable  : KPtr -> KTable attrs len cols
  
||| A K dictionary is a list of lists, each associated with a particular key.
||| The key does not have to be any particular type.
export data KDict : List KAttrs -> Vect n String -> Type where
  MkKDict : KPtr -> KDict attrs keys
  
||| A K list is assumed to be a heterogeneous list; the actual kdb type of the list is determined at runtime.
||| The type vector indexing the list must contain a type for each element in the list.
||| Use [t, t, t, t, ..., t] to model a homogeneous list.
export data KList : List KAttrs -> (cap : Nat) -> Either KTy (Vect n KTy) -> Type where
  MkKList : KPtr -> KList attrs cap types
  
||| A K atom is a single value of the specified K primitive type.
export data KAtom : List KAttrs -> KPrimTy -> Type where
  MkKAtom : KPtr -> KAtom attrs ty
  
||| A K function is parametrised by the names and possible types of its arguments, and then its
||| possible return types.
export data KFunc : List KAttrs -> Vect n (String, List KTy) -> List KTy -> Type where
  MkKFunc : KPtr -> KFunc attrs params res

||| A predicate indicating that a given Idris type has a K representation.
public export data KData : KTy -> Type where
  
||| Interpret a primitive K type as an Idris type.
public export kPrimTy : KPrimTy -> Type  
  
||| Interpret a K type as an Idris type.
public export kTy : KTy -> Type
  
||| Associate a K type with its representative Idris definition (wrapper).
public export kRep : KTy -> Type
kRep (MkKTy (KList t)) = Vect n (Raw.__k_prim_ty t)

||| Extract an Idris value from a K value.
||| Warning: this could be very slow.
public export fromKVal : {ty : KTy} -> (kval : kRep ty) -> kTy ty
  
||| Builds an Idris value into its representative K type.
||| This always involves a full copy of the data.
export toKVal : KData kty -> kRep kty

||| Build a new KAtom from an existing representative type.
export mkAtom : (ty : KPrimTy) -> (kPrimTy ty) -> KAtom [] ty
  
export mkList : (listTy : Either KTy (Vect n KTy)) -> (len : Nat) -> KList [] len listTy
  
export mkDict : (keys : KList a n (Left (MkKTy (KAtom KSym)))) -> 
                (vals : KList a' n tys) -> KDict [] (fromKVal keys)
