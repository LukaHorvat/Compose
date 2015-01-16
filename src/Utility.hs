{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module Utility where

import Data.Array.MArray
import Data.Array.Storable
import Foreign.Ptr
import Foreign.Storable

asArray :: Storable a => [a] -> IO (StorableArray Int a)
asArray dat = newListArray (0, Prelude.length dat - 1) dat

class UsingPtr a p | a -> p where
    usingPtr :: a -> (Ptr p -> IO b) -> IO b

instance Storable a => UsingPtr (StorableArray Int a) a where
    usingPtr = withStorableArray 