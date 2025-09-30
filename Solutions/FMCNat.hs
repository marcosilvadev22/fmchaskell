{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where

    (==) :: Nat -> Nat -> Bool
    O == O = True
    (S m) == (S n) = m == n
    _ == _ = False

instance Ord Nat where

    (<=) :: Nat -> Nat -> Bool
    O <= _ = True
    (S _) <= O = False
    (S m) <= (S n) = m <= n

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min :: Nat -> Nat -> Nat
    min O n = O
    min m O = O
    min (S m) (S n) = S (min m n)

    max :: Nat -> Nat -> Nat
    max O n = n 
    max m O = m
    max (S m) (S n) = S (max m n)


----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

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

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Nat
isZero O = S O
isZero (S _ ) = O

pred :: Nat -> Nat
pred O = O 
pred (S m ) = m 

even :: Nat -> Nat
even O = S O
even (S O) = O   
even (S (S m)) = even m

odd :: Nat -> Nat
odd n = isZero (even n)

----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> O   = n
n <+> (S m) = S (n <+> m) 

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
times :: Nat -> Nat -> Nat
times n O = O
times n (S m) = n <+> (n `times` m)

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow n O = S O
pow n (S m) = n <*> (n `pow` m)

exp :: Nat -> Nat -> Nat
exp = pow

(<^>) :: Nat -> Nat -> Nat
(<^>) = exp

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) = (//)
_ // O = undefined
n // m =
  case n -* m of
    O ->
      case m -* n of
        O -> S O
        _ -> O
    S k ->
      S (S k // m)


-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) = (%%)
n %% m =
  case n -* m of
    O ->
      case m -* n of
        O -> O
        _ -> n
    k -> k %% m 

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv (n, m) = (n // m, n %% m)

-- divides
divides :: Nat -> Nat -> Bool
divides O (S _) = False
divides _ O     = True
divides a b = (b %% a) == O

(<|>) :: Nat -> Nat -> Bool
(<|>) = divides


-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist = absDiff
absDiff x y = (x -* y) <+> (y -* x)

(|-|) = dist

factorial :: Nat -> Nat
factorial O = S O
factorial (S n) = S n <*> factorial n 

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
    S O -> S (lo b (a // b))



----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat n 
    | n < 0     = error "não pode converter negativos para Nat"
    | n == 0    = O
    | otherwise = S (toNat (n - 1))

fromNat :: Integral a => Nat -> a
fromNat O = 0
fromNat (S n) = 1 + fromNat n


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (-*)
    abs n = n
    signum = sg
    fromInteger x
        | x < 0     = error "não pode ser negativo"
        | x == 0    = O
        | otherwise = S (fromInteger (x - 1))

