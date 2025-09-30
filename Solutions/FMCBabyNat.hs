module FMCBabyNat where

-- Do not alter this import!
import Prelude ( Show(..) , Eq(..) , undefined )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

-- define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

-- some sugar
zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

-- addition
(+) :: Nat -> Nat -> Nat
n + O   = n
n + S m = S (n + m)

 -- syntactic associativity: L
 -- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True
isZero :: Nat -> Nat
isZero O = S O
isZero (S _ ) = O

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O 
pred (S m ) = m 

-- Output: O means False, S O means True
even :: Nat -> Nat
even O = S O
even (S O) = O   
even (S (S m)) = even m



odd :: Nat -> Nat
odd n = isZero (even n)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus m O = m
monus O n = O
monus (S n) (S m) = monus n m


(-*) :: Nat -> Nat -> Nat
(-*) = monus

-- multiplication
(*) :: Nat -> Nat -> Nat
_ * O = O
n * (S m) = n + (n * m)


infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
_ ^ O     = S O -- condição: qualquer número elevado a 0 é 1
n ^ (S m) = n * (n ^ m)
infixr 8 ^  

-- acossiei pela direita o operador ^


-- quotient
(/) :: Nat -> Nat -> Nat
_ / O = undefined
n / m =
  case n -* m of
    O ->
      case m -* n of
        O -> S O
        _ -> O
    S k ->
      S (S k / m) -- aqui consegui fazer a recursão
infixl 7 / 


-- remainder
(%) :: Nat -> Nat -> Nat
n % m =
  case n -* m of
    O ->
      case m -* n of
        O -> O
        _ -> n
    k -> k % m 

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
O ||| O = S O 
O ||| (S _ ) = O   
a ||| b = isZero (b % a)

infix 3 ||| 

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat
absDiff x y = (x -* y) + (y -* x)

(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff

factorial :: Nat -> Nat
factorial O = S O
factorial (S n) = S n * factorial n 

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg (S _ ) = S O 


-- lo b a is the floor of the logarithm base b of a
maiorOuIgual :: Nat -> Nat -> Nat -- fiz uma função auxiliar para comparar dois Nats
maiorOuIgual _ O = S O
maiorOuIgual O (S _) = O
maiorOuIgual (S n) (S m) = maiorOuIgual n m

lo :: Nat -> Nat -> Nat
lo O _ = undefined
lo (S O) _ = undefined
lo _ O     = O 
lo b a =
  case maiorOuIgual a b of
    O -> O
    S O -> S (lo b (a / b))

