import Data.Vect

interface Neuron (t : Nat -> Type) where
  eval : t n -> Vect n Double -> Double

data Perceptron : Nat -> Type where
  MkP : Double -> Vect n Double -> Perceptron n

Neuron Perceptron where
  eval (MkP t w) v = 
    let a = sum (zipWith (*) v w) in
    if t > a then 0 else 1

data Sigmoid : Nat -> Type where
  MkS : Vect n Double -> Sigmoid n
  
Neuron Sigmoid where
  eval (MkS w) v =
    let a = sum (zipWith (*) v w) in
    1 / (1 + exp (negate a))
  
infixr 5 ##
data NNet : (t : Nat -> Type) -> Vect k Nat -> Type where
  Output : Neuron t => NNet t [k]
  (##)   : Neuron t => Vect k (t n) -> NNet t (k::ns) -> NNet t (n::k::ns)

runNN : NNet p (n::ns) -> Vect n Double -> Vect (last (n::ns)) Double
runNN Output xs = xs
runNN (ys ## x) xs = runNN x (map (flip eval xs) ys)

rsq : Vect k Double -> Vect k Double -> Double
rsq v w = (sum $ map (\x => x * x) (zipWith (-) v w)) / (cast $ length v)
  
split_paired_list : List (a,b) -> (k : Nat ** (Vect k a, Vect k b))
split_paired_list [] = (0 ** ([], []))
split_paired_list ((a, b) :: xs) = 
  let (k ** (as, bs)) = split_paired_list xs in 
  (S k ** (a::as, b::bs))

error : Neuron p => p n -> List (Vect n Double, Double) -> Double
error p test = 
  let (_ ** (test_inputs, test_outputs)) = split_paired_list test in
  rsq (map (eval p) test_inputs) test_outputs
  
  
trP : List (Vect n Double, Double) -> Double -> Perceptron n -> Perceptron n
trP [] _ p = p
trP ((train_input, train_output) :: trains) delta p@(MkP b w) = 
  let output = eval p train_input in
  trP trains delta $ MkP b (zipWith (\weight, input => weight + (train_output - output) * input * delta) w train_input)
 
  
trainPerceptron : List (Vect n Double, Double) -> List (Vect n Double, Double) -> Double -> Double -> Perceptron n -> Perceptron n
trainPerceptron train test delta err p = assert_total $ 
  let trained = trP train delta p in
  let (_ ** (test_inputs, test_outputs)) = split_paired_list test in 
  if err < rsq (map (eval trained) test_inputs) test_outputs
  then trainPerceptron train test delta err trained
  else trained
          
and_training : List (Vect 2 Double, Double)
and_training = [([0, 0], 0),([1, 0], 0), ([0, 1], 0), ([1, 1], 1)]

xor_training : List (Vect 2 Double, Double)  
xor_training = [([0, 0], 0),
               ([1, 0], 1),
               ([0, 1], 0),
               ([1, 1], 1)]

  
--trainSigmoid : List (Vect n Double, Double) -> List (Vect n Double, Double) -> Double -> Double -> Sigmoid n -> Sigmoid n
--  trS : Vect m (Vect n Double, Double) -> Double -> Sigmoid n -> Sigmoid n

