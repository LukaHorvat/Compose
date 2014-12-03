{-# LANGUAGE FunctionalDependencies
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
           , TypeFamilies #-}
module Binary where

import TypeLevel
import Prelude hiding (Bool)

{-
Type level binary numbers. Only addition implemented.
-}
class BoolList a
instance BoolList Nil
instance (Bool b, BoolList bs) => BoolList (Cons b bs)

class (Bool x, Bool y, Bool cin, Bool z, Bool cout) => BoolPlus x y cin z cout | x y cin -> z cout
instance Bool c => BoolPlus True True c c True
instance Not c nc => BoolPlus True False c nc c
instance Not c nc => BoolPlus False True c nc c
instance Bool c => BoolPlus False False c c False

class (BoolList xs, BoolList ys) => Increment xs ys | xs -> ys
instance Increment Nil (Cons True Nil)
instance BoolList xs => Increment (Cons False xs) (Cons True xs)
instance Increment xs ys => Increment (Cons True xs) (Cons False ys)

class (BoolList xs, BoolList ys, Bool carry, BoolList zs) => BinPlus' xs ys carry zs | xs ys carry -> zs
instance BoolList xs => BinPlus' Nil xs False xs
instance Increment xs ys => BinPlus' Nil xs True ys
instance (BoolPlus x y cin z cout, BinPlus' xs ys cout zs) => BinPlus' (Cons x xs) (Cons y ys) cin (Cons z zs)
instance BinPlus' xs ys c zs => BinPlus' ys xs c zs

class (BoolList xs, BoolList ys, BoolList zs) => BinPlus xs ys zs | xs ys -> zs
instance BinPlus' xs ys False zs => BinPlus xs ys zs

increment :: (Increment xs ys) => xs -> ys
increment = undefined

boolPlus :: (BoolPlus x y cin z cout) => x -> y -> cin -> z -> cout
boolPlus = undefined

binPlus :: (BinPlus xs ys zs) => xs -> ys -> zs
binPlus = undefined

binPlus' :: (BinPlus' xs ys carry zs) => xs -> ys -> carry -> zs
binPlus' = undefined