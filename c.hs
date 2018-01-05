import Data.Char
import Data.List
import System.Environment
import System.IO
import System.Process
import System.Exit
data R_=RAX|RBX|RDX|RSP|RBP|R10 deriving Show
data R=R_ R_
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
data Noun = N_Int I
data A = A_P [A]
       | A_V Verb 
       | A_N Noun
       | A_M Verb A
       | A_D A Verb A

parse   :: S -> A
parse _ = (A_N (N_Int 0))
compile__cc_head :: [ASM]
compile__cc_head = [Push (VR $ R_ RBP), Mov (VR $ R_ RSP) (VR $ R_ RBP)]
compile__cc_tail :: [ASM]
compile__cc_tail = [Pop $ R_ RBP, Ret]
compile :: A -> [ASM]
compile _ = (Label "main") : compile__cc_head ++ [Mov (VI 0) (VR $ R_ RAX)] ++ compile__cc_tail
codegen :: [ASM] -> S
codegen asm = (intercalate "\n" (["\t.globl main", "\t.section .text"] ++ (map cinstr asm))) ++ "\n"
  where
    cinstr :: ASM -> S
    cinstr NOOP = ""
    cinstr (Label s) = s ++ ":"
    cinstr i =
      let (t:ext) = show i in '\t' : (toLower t) : ext
   
instance Show V where
  show (VI i) = '$' : show i
  show (VR r) = show r
  show (DR r i) = (show i) ++ "(" ++ show r ++ ")"

instance Show R where
  show (R_ r) = showR r

showR :: R_ -> S
showR r = '%' : map toLower (show r)

main :: IO ()
main = do
  args <- getArgs
  input <- readFile (args !! 0)
  let name = args !! 1
  writeFile (name ++ ".s") (codegen . compile . parse $ input)
  code <- system ("gcc " ++ name ++ ".s" ++ " -o " ++ name)
  exitWith code
