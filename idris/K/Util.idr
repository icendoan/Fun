module K.Util
  import Data.Vect
  public export chunks : Vect (n * m) t -> Vect n (Vect m t)
  chunks {n = n} {m = Z} xs = replicate n []
  chunks {n = Z} {m = (S k)} xs = []
  chunks {n = (S j)} {m = (S k)} (x :: xs) = 
    let head = take k xs in
    let tail = drop k xs in
    (x :: head) :: chunks tail
  
  public export split : (m : Nat) -> Vect (m + n) t -> (Vect m t, Vect n t)
  split m v = (take m v, drop m v)
  
  --split : (tk : Nat) -> Vect k t -> Maybe (x : Nat ** (Vect tk t, Vect x t))
  --split (S k) [] = Nothing
  --split (S k) (x::xs) = do
  --  (c ** (head, tail)) <- split k xs
  --  ?something
  --split Z v = Just (_ ** ([], v))
  
  -- if using pointers, all type safety goes out the window anyway
  public export data TypedPtr : Type -> Type where
    WrapPtr : Ptr -> TypedPtr t

  export readBytesShort : Vect 2 Bits8 -> Bits16
  readBytesShort bytes = 
    let [a, b] = map (prim__zextB8_B16) bytes in
    prim__addB16 (prim__shlB16 a 8) b

  export readBytesInt : Vect 4 Bits8 -> Bits32
  readBytesInt bytes = 
    let [d, c, b, a] = map (prim__zextB8_B32) bytes in
    (prim__shlB32 d 24) + (prim__shlB32 c 16) + (prim__shlB32 b 8) + a

  export readBytesLong : Vect 8 Bits8 -> Bits64
  readBytesLong bytes = 
    let bytes = the (Vect 8 Bits64) $ map (prim__zextB8_B64) bytes in
    sum . map (uncurry prim__shlB64) . zip (map (*8) [7,6,5,4,3,2,1,0]) $ bytes

  ||| Interpret a list of Bits8 as a string
  export asciiStr : List Bits8 -> String
  asciiStr = pack . map (prim__intToChar . prim__zextB8_Int)
  
  ||| Read a pointer as a character array for a number of (ascii!) characters
  export readCStar : Ptr -> (len : Int) -> String
  readCStar ptr len = asciiStr $ map (prim__peek8 prim__TheWorld ptr) [0, 8 .. 8*len]
  
  -- literally the same as for(;*ptr!=0;ptr++)
  ||| Read an (ascii!) C string until the null terminator
  export readCStr : Ptr -> String
  readCStr ptr = asciiStr (go ptr 0)
    where
      go : Ptr -> Int -> List Bits8
      go ptr offset = 
        case (prim__peek8 prim__TheWorld ptr offset) of
          0 => []
          b => b :: go ptr (offset + 1)
