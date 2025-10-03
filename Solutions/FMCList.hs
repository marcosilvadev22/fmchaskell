{-# LANGUAGE GADTs #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head (x:_) = x
head [] = error "corpo vazio"

tail :: [a] -> [a]
tail [_] = []
tail (_:xs) = xs
tail [] = error "corpo vazio"

null :: [a] -> Bool
null [] = True
null (_:_) = False

length :: Integral i => [a] -> i
length [] = 0
length (_:xs) = 1 + length xs

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * product xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x : (xs ++ ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]
infixr 5 `snoc`

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

{- minimum :: Ord a => [a] -> a
minimum [] = error "corpo vazio"
minimum [x] = x
minimum (x:xs) = min x (minimum xs)

maximum :: Ord a => [a] -> a
maximum [] = error "corpo vazio"
maximum [x] = x
maximum (x:xs) = max x (maximum xs)

take :: Integral i => i -> [a] -> [a]
take n _ | n <= 0 = []
take _ [] = []
take n (x:xs) = x : take (n-1) xs

drop :: Integral i => i -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ [] = []
drop n (_:xs) = drop (n-1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) | p x       = x : takeWhile p xs
                    | otherwise = []

-- dropWhile

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:xs') = xs : tails xs'

init :: [a] -> [a]
init [] = error "corpo vazio"
init [_] = []
init (x:xs) = x : init xs

inits :: [a] -> [[a]]
inits [] = [[]]
inits xs@(_:xs') = [] : map (head xs :) (inits (init xs)) -- não gostei do xs@..


subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = let subs = subsequences xs in subs ++ map (x:) subs -- não gostei

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all p (x:xs) = p x && all p xs
any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x:xs) = p x || any p xs 

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs
or :: [Bool] -> Bool
or [] = False
or (x:xs) = x || or xs

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs -}


-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

(!!) :: [a] -> Int -> a
(!!) xs n | n < 0 = error "índice negativo"
(!!) [] _ = error "corpo vazio"
(!!) (x:_) 0 = x
(!!) (_:xs) n = xs !! (n-1)

{- filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) | p x       = x : filter p xs
                | otherwise = filter p xs -- não gostei muito dessa solução
map:: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs -- vou olhar depois 


cycle :: [a] -> [a
cycle [] = error "corpo vazio"
cycle xs = xs ++ cycle xs --  conferir depois -}

repeat :: a -> [a]
repeat x = x : repeat x
replicate :: Integral i => i -> a -> [a]
replicate n x | n <= 0 = []
              | otherwise = x : replicate (n-1) x 



-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

splitAt :: Integral i => i -> [a] -> ([a], [a])
splitAt n xs  =  (take n xs, drop n xs)
infix 5 `splitAt`


-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
normalize :: String -> String
normalize s = [C.toLower c | c <- s, C.isAlphaNum c]

palindrome :: String -> Bool
palindrome s = normalize s == reverse (normalize s)


{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

