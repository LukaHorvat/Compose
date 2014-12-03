{-# LANGUAGE FunctionalDependencies
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
           , DeriveDataTypeable
           , OverlappingInstances
           , TypeFamilies #-}
module TypeLevel where

import Data.Typeable
import Prelude hiding (Eq)

data True  deriving Typeable
data False deriving Typeable

class Is x y b | x y -> b
instance Is True  True  True
instance Is True  False False
instance Is False False True
instance Is False True  False

class And x y b | x y -> b
instance And True  True  True
instance And True  False False
instance And False True  False
instance And False False False

class Or x y b | x y -> b
instance Or True  False True
instance Or False True  True
instance Or True  True  True
instance Or False False False

class Not x b | x -> b
instance Not True  False
instance Not False True

class Isnt x y b | x y -> b
instance (Is x y b1, Not b1 b2) => Isnt x y b2

class Xor x y b | x y -> b
instance (Or x y b1, And x y b2, Not b2 b3, And b1 b3 b4) => Xor x y b4

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

data Zero   deriving Typeable
data Succ a deriving Typeable

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

class Plus x y z | x y -> z
instance Plus Zero x x
instance Plus x y z => Plus (Succ x) y (Succ z)

class Times x y z | x y -> z
instance Times Zero x Zero
instance (Times x y z1, Plus y z1 z2) => Times (Succ x) y z2

class Minus x y z | y z -> x
instance Plus x y z => Minus z x y 

plus :: (Plus x y z) => x -> y -> z
plus = undefined

times :: (Times x y z) => x -> y -> z
times = undefined

minus :: (Minus x y z) => x -> y -> z
minus = undefined

data EQ deriving Typeable
data GT deriving Typeable
data LT deriving Typeable

instance Is EQ EQ True
instance Is GT GT True
instance Is LT LT True
instance Is EQ GT False
instance Is EQ LT False
instance Is GT LT False
instance Is x y b => Is y x b 

class Compare x y c | x y -> c
instance Compare Zero     Zero     EQ
instance Compare (Succ x) Zero     GT
instance Compare Zero     (Succ x) LT
instance Compare x y c => Compare (Succ x) (Succ y) c

class Eq x y b  | x y -> b
class Lt x y b  | x y -> b
class Gt x y b  | x y -> b
class Leq x y b | x y -> b
class Geq x y b | x y -> b
class Neq x y b | x y -> b

instance (Compare x y c, Is c EQ b) => Eq x y b
instance (Compare x y c, Is c LT b) => Lt x y b
instance (Compare x y c, Is c GT b) => Gt x y b
instance (Compare x y c, Isnt c GT b) => Leq x y b
instance (Compare x y c, Isnt c LT b) => Geq x y b
instance (Compare x y c, Isnt c EQ b) => Neq x y b

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

class Inc x y | x -> y
instance Inc x (Succ x)

class Choose b x y z | b x y -> z
instance Choose True  x y x
instance Choose False x y y

class Over' x y acc z | x y acc -> z
instance Over' x y x Zero
instance (Plus acc y acc2, Gt acc2 x b, Choose b x acc2 acc3, Over' x y acc3 z1, Inc z1 z2) => Over' x y acc z2

class Over x y z | x y -> z
instance Over' x y Zero z => Over x y z

inc :: (Inc x y) => x -> y
inc = undefined

over :: (Over x y z) => x -> y -> z
over = undefined

data Nil      deriving Typeable
data Cons a b deriving Typeable

class Append xs ys zs | xs ys -> zs
instance Append Nil xs xs
instance Append xs ys zs => Append (Cons x xs) ys (Cons x zs)

class Elem x xs b | x xs -> b
instance Elem x Nil         False
instance Elem x (Cons x xs) True
instance Elem x ys b => Elem x (Cons y ys) b

append :: (Append xs ys zs) => xs -> ys -> zs
append = undefined

elem :: (Elem x xs b) => x -> xs -> b
elem = undefined