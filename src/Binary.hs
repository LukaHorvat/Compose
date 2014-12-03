{-# LANGUAGE FunctionalDependencies
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
           , TypeFamilies #-}
module Binary where

import TypeLevel

{-
Type level binary numbers. Only addition implemented.
-}

class BoolPlus x y cin z cout | x y cin -> z cout
instance BoolPlus True True c c True
instance Not c nc => BoolPlus True False c nc c
instance Not c nc => BoolPlus False True c nc c
instance BoolPlus False False c c False

class Increment xs ys | xs -> ys
instance Increment Nil (Cons True Nil)
instance Increment (Cons False xs) (Cons True xs)
instance Increment xs ys => Increment (Cons True xs) (Cons False ys)

class BinPlus' xs ys carry zs | xs ys carry -> zs
instance BinPlus' Nil xs False xs
instance Increment xs ys => BinPlus' Nil xs True ys
instance (BoolPlus x y cin z cout, BinPlus' xs ys cout zs) => BinPlus' (Cons x xs) (Cons y ys) cin (Cons z zs)
instance BinPlus' xs ys c zs => BinPlus' ys xs c zs

class BinPlus xs ys zs | xs ys -> zs
instance BinPlus' xs ys False zs => BinPlus xs ys zs

increment :: (Increment xs ys) => xs -> ys
increment = undefined

boolPlus :: (BoolPlus x y cin z cout) => x -> y -> cin -> z -> cout
boolPlus = undefined

binPlus :: (BinPlus xs ys zs) => xs -> ys -> zs
binPlus = undefined

binPlus' :: (BinPlus' xs ys carry zs) => xs -> ys -> carry -> zs
binPlus' = undefined