module Examples where

import Data.Typeable
import TypeLevel
import Components
import Prelude hiding (and, elem)

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

ex1 :: IO ()
ex1 = do
    let val1 = entity `withDisplayData` (5 :: Int)
    let val2 = val1 `withDisplayer` (print :: Int -> IO ())
    --display val1 --Compile error
    display val2

ex2 :: IO ()
ex2 = do
    let val1 = entity `with` (1 :: Int) `with` (2.5 :: Double)
    let updater1 = Updater (+1) :: Updater Int
    let updater2 = Updater (+2) :: Updater Double 
    let val2 = val1 `with` updater1 `with` updater2
    print val2
    print $ updateEnt val2