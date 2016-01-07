import Data.Vect

--plusReduces : (n : Nat) -> (m : Nat) -> S (plus n m) = plus (S n) m
--plusReduces n Z = Refl
--plusReduces n (S m) = cong {f= \x => S x} (plusReduces n m)
 
separate : Vect n a -> Vect n (Vect 1 a)
separate [] = []
separate (x :: xs) = [x] :: separate xs

readNat : Num n => Char -> Maybe n
readNat '0' = Just 0
readNat '1' = Just 1
readNat '2' = Just 2
readNat '3' = Just 3
readNat '4' = Just 4
readNat '5' = Just 5
readNat '6' = Just 6
readNat '7' = Just 7
readNat '8' = Just 8
readNat '9' = Just 9
readNat _   = Nothing
  
readNum_ : Num n => Maybe n -> Char -> Maybe n
readNum_ n c = do
  x <- n
  y <- readNat c
  return $ 10 * x + y
  
readNum : Num n => String -> Maybe n
readNum s = Foldable.foldl (\mn,c => readNum_ mn c) (Just 0) $ unpack s
  
second : (a ** b) -> b
second (MkSigma a b) = b
  
fibonacci : Stream Nat
fibonacci = map fst . iterate f $ (0,1)
  where
    f : (Nat, Nat) -> (Nat, Nat)
    f (a, b) = (b, a + b)

filterS : (a -> Bool) -> Stream a -> Stream a
filterS p (x :: y) = if p x then x :: filterS p y else filterS p y

takeWhileSV : (a -> Bool) -> Stream a -> (n ** Vect n a)
takeWhileSV p (x :: y) = if p x then let (MkSigma k v) = takeWhileSV p y in (S k ** x :: v)
                                else (0 ** [])

takeWhileSL : (a -> Bool) -> Stream a -> List a
takeWhileSL p (x :: y) = if p x then x :: takeWhileSL p y else []

sieve : Stream Nat -> Stream Nat
sieve (p :: rest) = p :: (sieve $ filterS (\x : Nat => (x `mod` p) /= 0) rest)  

primes : Stream Nat
primes = sieve [2..]

largest : (Foldable f, MinBound a) => f a -> a
largest x = Foldable.foldr max minBound x

factorise : Nat -> List Nat
factorise n = let p1 = Stream.head (filterS (\x : Nat => (n `mod` x) == 0) primes) in p1 :: (factorise (n `div` p1))

prob1 : Nat
prob1 = sum . (filter (\x : Nat => ((x `mod` 3) /= 0) || ((x `mod` 5) /= 0))) $ [1..1001]

prob2 : Nat
prob2 = sum . (filter (\n : Nat => (n `mod` 2) == 0)) . (takeWhileSL (\n : Nat => n < 4000000)) $ fibonacci

prob3 : Nat
prob3 = largest . factorise $ 600851475143

main : IO ()
main = print prob1 
