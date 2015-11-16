-- Impossibly slow on loops
-- No idea why
import Data.List
import Data.Vect
  
record BFState where
  constructor BFS
  left : Vect m Int
  val : Int
  right : Vect n Int
  
data Action = L | R | P | M | Out | In | W (Vect k Action) | E Char
  
emptyState : BFState
emptyState = BFS [] 0 [] 
 
takeCorresponding : Eq a => Nat -> a -> a -> (xs : Vect n a) -> (ys : Vect m a) -> ((x ** Vect x a), (y ** Vect y a))
takeCorresponding k s t xs (y :: ys) = case ((y == s), (y == t), k) of
  (True, False, k) => takeCorresponding (S k) s t xs ys
  (False, True, (S k)) => takeCorresponding k s t (y::xs) (ys)
  (False, True, Z) => ((_ ** xs), (_ ** ys))
  _ => takeCorresponding k s t (y::xs) (ys)
takeCorresponding k s t xs [] = ((_ ** xs), (_ ** []))

fromString : (s : String) -> Vect (length s) Char
fromString s = Vect.take (length s) . unpack $ s 

parse_ : Vect n Char -> (k ** Vect k Action)
parse_ [] = (_ ** [])
parse_ ('>' :: cs) = let (x ** xs) = parse_ cs in (S x ** L :: xs)
parse_ ('<' :: cs) = let (x ** xs) = parse_ cs in (S x ** R :: xs)
parse_ ('+' :: cs) = let (x ** xs) = parse_ cs in (S x ** P :: xs)
parse_ ('-' :: cs) = let (x ** xs) = parse_ cs in (S x ** M :: xs)
parse_ ('.' :: cs) = let (x ** xs) = parse_ cs in (S x ** Out :: xs)
parse_ (',' :: cs) = let (x ** xs) = parse_ cs in (S x ** In :: xs)
parse_ ('[' :: cs) = let ((_ ** xs), (_ ** ys)) = (takeCorresponding 0 '[' ']' [] cs) in 
                     let (_ ** xxs) = parse_ xs in
                     let (y ** yys) = parse_ ys in (S y ** (W xxs) :: yys)
  
parse : String -> (n ** Vect n Action)
parse = parse_ . fromString

mleft : BFState -> IO BFState
mleft (BFS [] val right) = return $ BFS [] 0 (val :: right)
mleft (BFS (x :: xs) val right) = return $ BFS xs x (val :: right)
  
mright : BFState -> IO BFState
mright (BFS left val []) = return $ BFS (val :: left) 0 []
mright (BFS left val (x :: xs)) = return $ BFS (val :: left) x xs 

inc : BFState -> IO BFState
inc (BFS left val right) = return $ BFS left (val + 1) right
  
dec : BFState -> IO BFState
dec (BFS left val right) = return $ BFS right (val - 1) right
  
out : BFState -> IO BFState
out b@(BFS left val right) = do { putStrLn $ show val; return b } 

inn : BFState -> IO BFState
inn (BFS left val right) = do { int <- getLine ; return $ BFS left (cast int) right }
  
mutual
    while : Vect n Action -> BFState -> IO BFState
    while accs b@(BFS left val right) = if (val == 0) then (return b) else (run accs b) >>= while accs

    run : Vect n Action -> BFState -> IO BFState
    run [] initState = return initState
    run (L :: xs) initState = mleft initState >>= run xs
    run (R :: xs) initState = mright initState >>= run xs
    run (P :: xs) initState = inc initState >>= run xs
    run (M :: xs) initState = dec initState >>= run xs
    run (Out :: xs) initState = out initState >>= run xs
    run (In :: xs) initState = inn initState >>= run xs
    run ((W ys) :: xs) initState = while ys initState >>= run xs
 

main : IO ()
main = do
  source <- getLine
  let (n ** actions) = parse source
  endState <- run actions emptyState
  putStrLn $ show $ val endState
