import Data.Char
import Data.List
import System.Environment
import System.IO
import System.Process
import System.Exit
data R=RAX|RBX|RDX|RSP|RBP|R10|RCX
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
        |Jmp S
        |Je S
        |Cmp V V
        |NOOP
data Op = Plus
        | Minus
        | Times
        | Til
        | At
        | Show
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
compile__cc_head = [Push (VR RBP), Mov (VR RSP) (VR RBP)]
compile__cc_tail :: [ASM]
compile__cc_tail = [Pop RBP, Ret]

compile :: (Int, A) -> [ASM]
compile (n, (A_M v (A_N (N_Int i)))) =
  case v of
    (V Til []) ->
      [
        Mov (VI (8 * i)) (VR RBX),
        Call "malloc",
        -- todo: handle malloc failing
        Mov (VR RAX) (VR RDX),
        Mov (VI i) (VR RCX),
        Label ((show n) ++ "_til_loop_start"),
        Cmp (VR RCX) (VI n),
        Je ((show n) ++ "_til_loop_end"),
        Add (VI 1) RCX,
        Add (VI 4) RDX,
        Mov (VR RCX) (DR RDX 0),
        Jmp ((show n) ++ "_til_loop_start"),
        Label (show n ++ "_til_loop_end")
      ]
compile _ = (Label "main") : compile__cc_head ++ [Mov (VI 0) (VR RAX)] ++ compile__cc_tail

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
  show RAX = "%rax"
  show RBX = "%rbx"
  show RDX = "%rdx"
  show RSP = "%rsp"
  show RBP = "%rbp"
  show R10 = "%r10"
  show RCX = "%rcx"
instance Show ASM where
  show (Push v) = "pushq " ++ show v
  show (Pop r) = "popq " ++ show r
  show (Mov x y) = "movq " ++ show x ++ ", " ++ show y
  show Cqto = "cqto"
  show (Imul x y) = "imulq " ++ show x ++ ", " ++ show y
  show (Sub x y) = "subq " ++ show x ++ ", " ++ show y
  show (Mul x y) = "mulq " ++ show x ++ ", " ++ show y
  show (Add x y) = "addq " ++ show x ++ ", " ++ show y
  show (Call s) = "call " ++ s
  show Ret = "ret"
  show (Label s) = s ++ ":"
  show NOOP = ""
  show (Jmp s) = "jmp " ++ s
  show (Je s) = "je " ++ s
  show (Cmp x y) = "cmp " ++ show x ++ ", " ++ show y

main :: IO ()
main = do
  args <- getArgs
  input <- readFile (args !! 0)
  let name = args !! 1
  writeFile (name ++ ".s") (codegen . compile . parse $ input)
  code <- system ("gcc " ++ name ++ ".s" ++ " -o " ++ name)
  exitWith code
