{-
 - A simple interpreter for a Turing machine
 - This is all the machinery for actually simulating the running of such a machine
 - It can move the tape, write to the tape, read from the tape
 - When run is supplied with a transition function and initial tape, it should output the final result of the TM
 - Each TM is assumed to transition to a special state Q "END", where the transition function will not be applied again, but the value of the tape at the current head and right will be returned
 - Run assumes that the TM starts on a special Q "START".
 - -}
-- Imports

--import Data.List

main :: IO ()
main = return ()

-- Type definitions.
data Alphabet = Zero | One | Blank deriving (Eq, Ord, Show)
data Move = L | R | S
data Tape = Tape { left :: [Alphabet], current :: Alphabet, right :: [Alphabet]}
data InternalState = Q { label :: String } 
type TransitionFunc = (InternalState, Alphabet) -> (InternalState, Alphabet, Move)

-- Moves the tape
shift :: Tape -> Move -> Tape
shift t S = t
shift t L = Tape (extendingTail (left t)) (extendingHead (left t)) (current t : right t)
shift t R = Tape (current t : left t) (extendingHead (right t)) (extendingTail (right t))

-- Because partial functions are _bad_
extendingHead :: [Alphabet] -> Alphabet
extendingHead [] = Blank
extendingHead (x:_) = x

extendingTail :: [Alphabet] -> [Alphabet]
extendingTail [] = [Blank]
extendingTail (_:x) = x

-- Writes a character to the current head of the tape
write :: Tape -> Alphabet -> Tape
write t s = Tape (left t) s (right t)

-- Convenience functions
toList :: Tape -> [Alphabet]
toList t = (reverse (left t)) ++ [current t] ++ (right t)

fromList :: [Alphabet] -> Tape
fromList l = Tape [] (extendingHead l) (extendingTail l)

encodeNumber :: Integer -> [Alphabet]
encodeNumber n = toBinary $ n
	where
		toBinary 0 = [Zero]
		toBinary 1 = [One]
		toBinary n
			| n `mod` 2 == 0 = Zero : toBinary (n `div` 2)
			| otherwise = One : toBinary ((n-1) `div` 2)

-- So we can see things
instance Show Tape where
	show = show . toList

-- Applies the transition function
runTM :: TransitionFunc -> (InternalState, Tape) -> (InternalState, Tape)
runTM f (q,t) = (q', writeAndMove (s, m) t)
	where
		(q', s, m) = f (q, current t)

-- Writes the character to the tape, and then moves the tape
writeAndMove :: (Alphabet, Move) -> Tape -> Tape
writeAndMove (s, m) t = shift t' m
	where t' = write t s

-- Runs the TM, and outputs the final result as a list

run :: TransitionFunc -> Tape -> (Integer, [Alphabet])
run f t = run' 0 f (Q "START", t)

run' :: Integer -> TransitionFunc -> (InternalState, Tape) -> (Integer, [Alphabet])
run' n f (q, t)
	| label q == "END" = (n, current t : right t)
	| otherwise = run' (n + 1) f $ runTM f (q, t)

-- Runs the TM, showing the tape and state label at each stage
showTM :: TransitionFunc -> Tape -> IO ()
showTM f t = showTM' f (Q "START") t 0

showTM' :: TransitionFunc -> InternalState -> Tape -> Integer -> IO ()
showTM' f q t n
	| label q == "END" = putStrLn $ "STEP " ++ show n ++ ": FINAL RESULT: " ++ show (current t : right t)
	| otherwise = do
		putStrLn $ "STEP " ++ show n ++ ": STATE: " ++ label q ++ " => " ++ show t
		let (q', t') = runTM f (q, t)
		showTM' f q' t' (n + 1)


-- the output tape from one TM is written as the input to the next, counting the total number of steps and returning the eventual result
-- TODO: replace the output of run with (Integer, Tape) so that actual composition will work.
compose :: TransitionFunc -> TransitionFunc -> Tape -> (Integer, [Alphabet])
compose f g t = (count, result)
	where
		count = m + n
		(n, result) = run f (fromList intermediate)
		(m, intermediate) = run g t

{- Definitions of TMs follow -}

trivial :: TransitionFunc
trivial (q, s) = (Q "END", s, S)

oddOnes :: TransitionFunc
oddOnes (Q "START", Zero) = (Q "START", Zero, R)
oddOnes (Q "START", One) = (Q "ODD", One, R)
oddOnes (Q "START", Blank) = (Q "END", Zero, S)
oddOnes (Q "ODD", Zero) = (Q "ODD", Zero, R)
oddOnes (Q "ODD", One) = (Q "START", One, R)
oddOnes (Q "ODD", Blank) = (Q "END", One, S)

increment :: TransitionFunc
increment (Q "START", Zero) = (Q "DONE", One, L)
increment (Q "START", One) = (Q "START", Zero, R)
increment (Q "START", Blank) = (Q "DONE", One, L)
increment (Q "DONE", Blank) = (Q "END", Blank, R)
increment (Q "DONE", a) = (Q "DONE", a, L)

palindrome :: TransitionFunc
palindrome (Q "START", Zero) = (Q "ZERO", Blank, R)
palindrome (Q "START", One) = (Q "ONE", Blank, R)
palindrome (Q "START", Blank) = (Q "END", One, S)
palindrome (Q "ZERO", Blank) = (Q "CHECKZERO", Blank, L)
palindrome (Q "ZERO", s) = (Q "ZERO", s, R)
palindrome (Q "ONE", Blank) = (Q "CHECKONE", Blank, L)
palindrome (Q "ONE", s) = (Q "ONE", s, R)
palindrome (Q "CHECKONE", Zero) = (Q "END", Zero, S)
palindrome (Q "CHECKONE", One) = (Q "GOBACK", Blank, L)
palindrome (Q "CHECKONE", Blank) = (Q "END", One, S)
palindrome (Q "CHECKZERO", Blank) = (Q "END", One, S)
palindrome (Q "CHECKZERO", Zero) = (Q "GOBACK", Blank, L)
palindrome (Q "CHECKZERO", One) = (Q "END", Zero, S)
palindrome (Q "GOBACK", Blank) = (Q "START", Blank, R)
palindrome (Q "GOBACK", s) = (Q "GOBACK", s, L)

decrement :: TransitionFunc
decrement (Q "START", Zero) = (Q "START", One, R)
decrement (Q "START", One) = (Q "GOBACK", Zero, L)
decrement (Q "START", Blank) = (Q "GOBACK", Blank, L)
decrement (Q "GOBACK", Blank) = (Q "END", Blank, R)
decrement (Q "GOBACK", s) = (Q "GOBACK", s, L)

moveRightThree :: TransitionFunc
moveRightThree (Q "START", s) = (Q "TWO", s, R)
moveRightThree (Q "TWO", s) = (Q "ONE", s, R)
moveRightThree (Q "ONE", s) = (Q "END", s, R)

moveUntilZero :: TransitionFunc
moveUntilZero (Q "START", One) = (Q "START", One, R)
moveUntilZero (Q "START", s) = (Q "END", s, S)

insert :: Alphabet -> TransitionFunc
insert x (q, s)
	| label q == "START" = case s of
		Zero -> (Q "COPYZERO", x, R)
		One -> (Q "COPYONE", x, R)
		Blank -> (Q "END", x, S)
	| label q == "COPYONE" = case s of
		Zero -> (Q "COPYZERO", One, R)
		One -> (q, One, R)
		Blank -> (Q "GOBACK", One, L)
	| label q == "COPYZERO" = case s of
		Zero -> (q, Zero, R)
		One -> (Q "COPYONE", Zero, R)
		Blank -> (Q "GOBACK", Zero, L)
	| label q == "GOBACK" = case s of
		Blank -> (Q "END", Blank, R)
		s -> (q, s, L)

delete :: TransitionFunc
delete (Q "START", s) = (Q "GOTOEND", Blank, R) -- starts at the cell to delete, replaces it with a Blank and then tracks until the end
delete (Q "GOTOEND", Blank) = (Q "COPYBACK", Blank, L) -- moves to the end of the input
delete (Q "GOTOEND", s) = (Q "GOTOEND", s, R)
delete (Q "COPYBACK", One) = (Q "COPYONE", Blank, L) -- the results are now shifted Left by one cell, until we hit the blank we wrote (we can't hit a blank before!)
delete (Q "COPYBACK", Zero) = (Q "COPYZERO", Blank, L)
delete (Q "COPYBACK", Blank) = (Q "DONE", Blank, L)
delete (Q "COPYONE", Zero) = (Q "COPYZERO", One, L)
delete (Q "COPYONE", One) = (Q "COPYONE", One, L)
delete (Q "COPYONE", Blank) = (Q "DONE", One, L)
delete (Q "COPYZERO", Zero) = (Q "COPYZERO", Zero, L)
delete (Q "COPYZERO", One) = (Q "COPYONE", Zero, L)
delete (Q "COPYZERO", Blank) = (Q "DONE", Zero, L)
delete (Q "DONE", Blank) = (Q "END", Blank, R) -- Once we've hit the blank, we stop copying, and we track to the start of the input. Once we hit Blank for a second time, we are done
delete (Q "DONE", s) = (Q "DONE", s, L)
