{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , FunctionalDependencies
           , OverlappingInstances
           , UndecidableInstances
           , ScopedTypeVariables
           , TypeOperators #-}
module Components where

data Entity c = Entity c

data CompNode c n = CompNode c n
data CompNil = CompNil

class HasComponent a b | b -> a where
    getComponent :: b -> a

instance HasComponent a (CompNode a n) where
    getComponent (CompNode a _) = a

instance HasComponent a n => HasComponent a (CompNode b n) where
    getComponent (CompNode _ n) = getComponent n

instance HasComponent a b => HasComponent a (Entity b) where
    getComponent (Entity b) = getComponent b

data True
data False

class Elem x xs b | x xs -> b where
    elem :: x -> xs -> b

data DisplayData a = DisplayData a
data Displayer a = Displayer (a -> IO ())

display :: forall a c . (HasComponent (DisplayData a) c, HasComponent (Displayer a) c) => Entity c -> IO ()
display e = f a
    where Displayer f = getComponent e :: Displayer a
          DisplayData a = getComponent e :: DisplayData a

infixr 7 :-:
type a :-: b = CompNode a b

(-:-) :: a -> c -> (a :-: c)
a -:- c = CompNode a c

with :: Entity c -> a -> Entity (a :-: c)
with (Entity c) a = Entity $ a -:- c

withDisplayData :: Entity c -> a -> Entity (DisplayData a :-: c)
withDisplayData e a = e `with` DisplayData a

withDisplayer :: Entity c -> (a -> IO ()) -> Entity (Displayer a :-: c)
withDisplayer e f = e `with` Displayer f

entity :: Entity CompNil
entity = Entity CompNil