{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , UndecidableInstances
           , ScopedTypeVariables
           , TypeOperators
           , OverlappingInstances #-}
module Components where

import TypeLevel
import Data.Typeable

data Entity c = Entity c

instance GetElem x xs => GetElem x (Entity xs) where
    getElem (Entity c) = getElem c
instance Show c => Show (Entity c) where
    show (Entity c) = "Entity " ++ show c

infixr 7 :-:
type a :-: b = Cons a b

(-:-) :: a -> c -> (a :-: c)
a -:- c = Cons a c

with :: Entity c -> a -> Entity (a :-: c)
with (Entity c) a = Entity $ a -:- c

entity :: Entity Nil
entity = Entity Nil

data DisplayData a = DisplayData a
data Displayer a = Displayer (a -> IO ())

display :: forall a c . (GetElem (DisplayData a) c, GetElem (Displayer a) c) => Entity c -> IO ()
display e = f a
    where Displayer f =   getElem e :: Displayer a
          DisplayData a = getElem e :: DisplayData a

withDisplayData :: Entity c -> a -> Entity (DisplayData a :-: c)
withDisplayData e a = e `with` DisplayData a

withDisplayer :: Entity c -> (a -> IO ()) -> Entity (Displayer a :-: c)
withDisplayer e f = e `with` Displayer f

data Updater a = Updater (a -> a)
instance Typeable a => Show (Updater a) where
    show _ = "Updater " ++ show (typeOf (undefined :: a))

class List xs => Update a xs where
    update :: Updater a -> xs -> xs  
instance Update a Nil where 
    update _ Nil = Nil
instance Update x xs => Update x (Cons x xs) where
    update u@(Updater f) (Cons x xs) = Cons (f x) $ update u xs
instance Update x xs => Update x (Cons y xs) where
    update u (Cons x xs) = Cons x $ update u xs        

class (List xs, List ys) => UpdateAll' xs ys where
    updateAll' :: xs -> ys -> ys
instance List ys => UpdateAll' Nil ys where
    updateAll' Nil ys = ys
instance (UpdateAll' xs ys, Update u ys) => UpdateAll' (Cons (Updater u) xs) ys where
    updateAll' (Cons u xs) ys = update u $ updateAll' xs ys

class Has Updater xs True => UpdateAll xs where
    updateAll :: xs -> xs
instance (Has Updater xs True, Filter Updater xs ys, UpdateAll' ys xs) => UpdateAll xs where
    updateAll xs = updateAll' (TypeLevel.filter (Updater undefined) xs) xs

updateEnt :: UpdateAll c => Entity c -> Entity c
updateEnt (Entity c) = Entity $ updateAll c
