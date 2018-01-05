import Data.Map as M
import Data.Char
data R=RAX|RBX|RDX|RSP|RBP|R10 deriving Show
type E=Either
type I=Int
data V=VI I|VR R|DR R I
type S=String
data ASM=Push V
        |Pop R
        |Mov V V
        |Cqto
        |Imul V R
        |Sub V R
        |Mul V R
        |Add V R
        |Call S
        |Ret
        |Label S
        |NOOP
        deriving Show
data Op = Plus
        | Minus
        | Times
        | Til
        | At
data Verb = V Op [Adverb] 
data Adverb = Each | Fold | Scan | EachR | EachL
data Noun = Int I
data A = A_P [A]
       | A_V Verb 
       | A_N Noun
       | A_M Verb A
       | A_D A Verb A

parse   :: S -> A
parse = undefined
compile__cc_head :: [ASM]
compile__cc_head = [Push (VR RBP), Mov (VR RSP) (VR RBP)]
compile__cc_tail :: [ASM]
compile__cc_tail = [Pop RBP]
compile :: A -> [ASM]
compile _ = compile__cc_head ++ [Mov (VI 0) (VR RAX),Ret]
codegen :: [ASM] -> S
codegen [] = ""
codegen (x : rest) = (cinstr x) ++ "\n" ++ codegen rest
  where
    cinstr :: ASM -> S
    cinstr NOOP = ""
    cinstr (Label s) = s ++ ":"
    cinstr i =
      let (t:ext) = show i in '\t' : (toLower t) : ext
   
instance Show V where
  show (VI i) = show i
  show (VR r) = show r
  show (DR r i) = (show i) ++ "(" ++ show r ++ ")"
