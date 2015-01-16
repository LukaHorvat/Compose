{-# LANGUAGE FunctionalDependencies
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
           , DeriveDataTypeable
           , OverlappingInstances
           , TypeFamilies 
           , AllowAmbiguousTypes
           , KindSignatures #-}
module TypeLevel where

import Data.Typeable
import Prelude hiding (Eq, Bool, Ordering, mod, filter)

data True  deriving Typeable
data False deriving Typeable

class Bool' a
class Bool' a => Bool a
instance Bool' True
instance Bool' False
instance Bool' a => Bool a

class (Bool x, Bool y, Bool b) => Is x y b | x y -> b
instance Is True  True  True
instance Is True  False False
instance Is False False True
instance Is False True  False

class (Bool x, Bool y, Bool b) => And x y b | x y -> b
instance And True  True  True
instance And True  False False
instance And False True  False
instance And False False False

class (Bool x, Bool y, Bool b) => Or x y b | x y -> b
instance Or True  False True
instance Or False True  True
instance Or True  True  True
instance Or False False False

class (Bool x, Bool b) => Not x b | x -> b
instance Not True  False
instance Not False True

class (Bool x, Bool y, Bool b) => Isnt x y b | x y -> b
instance (Is x y b1, Not b1 b2) => Isnt x y b2

class (Bool x, Bool y, Bool b) => Xor x y b | x y -> b
instance Isnt x y b => Xor x y b

is :: (Is x y b) => x -> y -> b
is = undefined

and :: (And x y b) => x -> y -> b
and = undefined

or :: (Or x y b) => x -> y -> b
or = undefined

not :: (Not x b) => x -> b
not = undefined

xor :: (Xor x y b) => x -> y -> b
xor = undefined

isnt :: (Isnt x y b) => x -> y -> b
isnt = undefined

data Zero   = Zero   deriving Typeable
data Succ a = Succ a deriving Typeable

class Nat' a
class Nat' a => Nat a
instance Nat' Zero
instance Nat' (Succ a)
instance Nat' a => Nat a

type One   = Succ Zero
type Two   = Succ One
type Three = Succ Two
type Four  = Succ Three
type Five  = Succ Four
type Six   = Succ Five
type Seven = Succ Six
type Eight = Succ Seven
type Nine  = Succ Eight
type Ten   = Succ Nine

one   = Succ Zero
two   = Succ one
three = Succ two
four  = Succ three
five  = Succ four
six   = Succ five
seven = Succ six
eight = Succ seven
nine  = Succ eight
ten   = Succ nine

class (Nat x, Nat y, Nat z) => Plus x y z | x y -> z
instance Nat x => Plus Zero x x
instance Plus x y z => Plus (Succ x) y (Succ z)

class (Nat x, Nat y, Nat z) => Times x y z | x y -> z
instance Nat x => Times Zero x Zero
instance (Times x y z1, Plus y z1 z2) => Times (Succ x) y z2

class (Nat x, Nat y, Nat acc, Nat z) => Minus' x y acc z | x y acc -> z
instance (Nat x, Nat acc) => Minus' x x acc acc
instance (Nat acc, Gt x y True, Minus' x (Succ y) (Succ acc) z) => Minus' x y acc z

class (Nat x, Nat y, Nat z) => UnsafeMinus x y z | x y -> z
instance Minus' x y Zero z => UnsafeMinus x y z 

class (Nat x, Nat y, Nat z) => Minus x y z | y z -> x
instance Plus x y z => Minus z y x 

plus :: (Plus x y z) => x -> y -> z
plus = undefined

times :: (Times x y z) => x -> y -> z
times = undefined

minus :: (Minus x y z) => x -> y -> z
minus = undefined

class Bool b => Equal x y b | x y -> b
instance Equal x x True
instance Not True b => Equal x y b

class Bool b => NotEqual x y b | x y -> b
instance (Equal x y b1, Not b1 b2) => NotEqual x y b2

equal :: (Equal x y b) => x -> y -> b
equal = undefined

notEqual :: (NotEqual x y b) => x -> y -> b
notEqual = undefined

data EQ deriving Typeable
data GT deriving Typeable
data LT deriving Typeable

class Ordering' a
class Ordering' a => Ordering a
instance Ordering' EQ
instance Ordering' GT
instance Ordering' LT
instance Ordering' a => Ordering a

class (Nat x, Nat y, Ordering c) => Compare x y c | x y -> c
instance Compare Zero     Zero     EQ
instance Compare (Succ x) Zero     GT
instance Compare Zero     (Succ x) LT
instance Compare x y c => Compare (Succ x) (Succ y) c

class (Nat x, Nat y, Bool b) => Eq x y b  | x y -> b
class (Nat x, Nat y, Bool b) => Lt x y b  | x y -> b
class (Nat x, Nat y, Bool b) => Gt x y b  | x y -> b
class (Nat x, Nat y, Bool b) => Leq x y b | x y -> b
class (Nat x, Nat y, Bool b) => Geq x y b | x y -> b
class (Nat x, Nat y, Bool b) => Neq x y b | x y -> b

instance (Compare x y c, Equal c EQ b) => Eq x y b
instance (Compare x y c, Equal c LT b) => Lt x y b
instance (Compare x y c, Equal c GT b) => Gt x y b
instance (Compare x y c, NotEqual c GT b) => Leq x y b
instance (Compare x y c, NotEqual c LT b) => Geq x y b
instance (Compare x y c, NotEqual c EQ b) => Neq x y b

compare :: (Compare x y c) => x -> y -> c
compare = undefined

eq :: (Eq x y b) => x -> y -> b
eq = undefined

lt :: (Lt x y b) => x -> y -> b
lt = undefined

gt :: (Gt x y b) => x -> y -> b
gt = undefined

leq :: (Leq x y b) => x -> y -> b
leq = undefined

geq :: (Geq x y b) => x -> y -> b
geq = undefined

neq :: (Neq x y b) => x -> y -> b
neq = undefined

class (Nat x, Nat y) => Inc x y | x -> y
instance Nat x => Inc x (Succ x)

class Bool b => Choose b x y z | b x y -> z
instance Choose True  x y x
instance Choose False x y y

class (Nat x, Nat y, Nat acc, Nat z) => Over' x y acc z | x y acc -> z
instance (Nat x, Nat y) => Over' x y x Zero
instance (Plus acc y acc2, Gt acc2 x b, Choose b x acc2 acc3, Over' x y acc3 z1, Inc z1 z2) => Over' x y acc z2

class (Nat x, Nat y, Nat z) => Over x y z | x y -> z
instance Over' x y Zero z => Over x y z

inc :: (Inc x y) => x -> y
inc = undefined

over :: (Over x y z) => x -> y -> z
over = undefined

data Nil      = Nil      deriving Typeable
data Cons a b = Cons a b deriving Typeable

class List' a
class List' a => List a
instance List' Nil
instance List' (Cons a b)
instance List' a => List a

class (List xs , List ys, List zs) => Append xs ys zs | xs ys -> zs
instance List xs => Append Nil xs xs
instance Append xs ys zs => Append (Cons x xs) ys (Cons x zs)

class (List xs, Bool b) => Elem x xs b | x xs -> b
instance Elem x Nil False
instance (Equal x y b1, Elem x ys b2, Or b1 b2 b3) => Elem x (Cons y ys) b3

append :: (Append xs ys zs) => xs -> ys -> zs
append = undefined

elem :: (Elem x xs b) => x -> xs -> b
elem = undefined

class GetElem x xs | xs -> x where
    getElem :: xs -> x

instance GetElem x (Cons x xs) where
    getElem (Cons x _) = x

instance GetElem x xs => GetElem x (Cons y xs) where
    getElem (Cons _ xs) = getElem xs

class (Nat x, Nat y, Bool f, Nat z) => Mod' x y f z | x y f -> z
instance (Nat x, Nat y) => Mod' x y True x
instance (UnsafeMinus x y z1, Lt z1 y b, Mod' z1 y b z2) => Mod' x y False z2

class (Nat x, Nat y, Nat z) => Mod x y z | x y -> z
instance (Lt x y b, Mod' x y b z) => Mod x y z

mod :: (Mod x y z) => x -> y -> z
mod = undefined

class (Nat x, Nat y, Bool b) => Prime' x y b | x y -> b
instance Nat x => Prime' x x True
instance (Mod x y z, NotEqual z Zero b1, Prime' x (Succ y) b2, And b1 b2 b3) => Prime' x y b3

class (Nat x, Bool b) => Prime x b | x -> b
instance Prime One True
instance Prime' x Two b => Prime x b

prime :: (Prime x b) => x -> b
prime = undefined

class ShowUnwrapped a where
    showUnwrapped :: a -> String
instance ShowUnwrapped Nil where
    showUnwrapped _ = ""
instance Show a => ShowUnwrapped (Cons a Nil) where
    showUnwrapped (Cons a _) = show a 
instance (Show a, ShowUnwrapped b) => ShowUnwrapped (Cons a b) where
    showUnwrapped (Cons a b) = show a ++ ", " ++ showUnwrapped b 

instance ShowUnwrapped (Cons x xs) => Show (Cons x xs) where
    show a = "<" ++ showUnwrapped a ++ ">"

class (List xs, Bool b) => Has (f :: * -> *) xs b | f xs -> b
instance Has f Nil False
instance Has f (Cons (f a) xs) True
instance Has f xs b => Has f (Cons x xs) b

class (List xs, List ys) => Filter (f :: * -> *) xs ys | f xs -> ys where
    filter :: f a -> xs -> ys
instance Filter f Nil Nil where
    filter _ Nil = Nil
instance Filter f xs ys => Filter f (Cons (f a) xs) (Cons (f a) ys) where
    filter f (Cons x xs) = Cons x (filter f xs)
instance Filter f xs ys => Filter f (Cons x xs) ys where
    filter f (Cons _ xs) = filter f xs

class (List xs, Nat i) => Index xs i x | xs i -> x where
    index :: xs -> i -> x
instance Index (Cons x xs) Zero x where
    index (Cons x _) Zero = x
instance Index xs i y => Index (Cons x xs) (Succ i) y where
    index (Cons _ xs) (Succ i) = index xs i

class Nat x => ToInt x where
    toInt :: x -> Int
instance ToInt Zero where
    toInt Zero = 0
instance ToInt x => ToInt (Succ x) where
    toInt (Succ x) = 1 + toInt x