module Examples where

import Data.Typeable
import TypeLevel
import Prelude hiding (and)

u :: a
u = undefined

ex0 :: IO ()
ex0 = do
    let res1 = xor (u :: True)  (u :: False)
    let res2 = xor (u :: True)  (u :: True)
    let res3 = and (u :: False) (u :: True)
    print $ typeOf res1
    print $ typeOf res2 
    print $ typeOf res3

{-
ex1 :: IO ()
ex1 = do
    let val1 = entity `withDisplayData` (5 :: Int)
    let val2 = val1 `withDisplayer` (print :: Int -> IO ())
    --display val1 --Compile error
    display val2
-}