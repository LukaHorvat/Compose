{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Linear where

import Utility
import Foreign.Storable
import Data.List
import Data.Vector.Storable (Vector, (!))
import qualified Data.Vector.Storable as V

data Tensor a = Tensor Int Int (Vector a)

instance Storable a => UsingPtr (Tensor a) a where
    usingPtr (Tensor _ _ v) = V.unsafeWith v

instance (Show a, Storable a) => Show (Tensor a) where
    show (Tensor rows cols v) = "[" ++ intercalate ", " (take rows $ Prelude.map (\i -> show $ V.slice i cols v) [0, cols..]) ++ "]"

uncheckedFromColumnList :: Storable a => [[a]] -> Tensor a
uncheckedFromColumnList [] = error "Cannot make a tensor out of an empty list"
uncheckedFromColumnList ([] : _) = error "Column cannot be empty"
uncheckedFromColumnList xss@(xs : _) = Tensor (length xs) (length xss) $ V.fromList $ concat xss

fromColumnList :: Storable a => [[a]] -> Tensor a
fromColumnList [] = error "Cannot make a tensor out of an empty list"
fromColumnList xss@(xs : _)
    | all p xss = uncheckedFromColumnList xss
    | otherwise = error "All lists must be the same length"
    where len = length xs
          p list = length list == len

fromColumn :: Storable a => [a] -> Tensor a
fromColumn [] = error "Cannot make a tensor out of an empty list"
fromColumn xs = Tensor (length xs) 1 $ V.fromList xs

map :: (Storable a, Storable b) => (a -> b) -> Tensor a -> Tensor b
map f (Tensor rows cols v) = Tensor rows cols $ V.map f v

prettyString :: (Show a, Storable a) => Tensor a -> String
prettyString t@(Tensor rows cols v) =  
    "┌" ++ spacing ++ "┐\n" ++ 
    body ++ 
    "└" ++ spacing ++ "┘\n"
    where spacing = replicate (cols * (maxWide + 1) + 1) ' '
          maxWide = V.maximum $ V.map (length . show) v
          pad s = s ++ replicate (maxWide - length s) ' '
          body = concat ["│ " ++ unwords [pad $ show $ uncheckedIndex t row col | col <- [0..cols - 1]] ++ " │\n" | row <- [0..rows - 1]]

prettyPrint :: (Show a, Storable a) => Tensor a -> IO ()
prettyPrint = putStrLn . prettyString

checkBounds :: Int -> Int -> Int -> Int -> a -> a
checkBounds rows cols row col a
    | row >= 0 && col >= 0 && 
      row < rows && col < cols = a
    | otherwise                = error "Index out of bounds"

uncheckedIndex :: Storable a => Tensor a -> Int -> Int -> a
uncheckedIndex (Tensor rows _ v) row col = v ! (col * rows + row) 

index :: Storable a => Tensor a -> Int -> Int -> a
index t@(Tensor rows cols _) row col = checkBounds rows cols row col $ uncheckedIndex t row col

zipWith :: (Num a, Storable a, Storable b) => (a -> a -> b) -> Tensor a -> Tensor a -> Tensor b
zipWith f (Tensor rows1 cols1 v1) (Tensor rows2 cols2 v2)
    | rows1 == rows2 && cols1 == cols2 = Tensor rows1 cols1 $ V.zipWith f v1 v2
    | otherwise                        = error "Tensors need to be the same size"

add :: (Num a, Storable a) => Tensor a -> Tensor a -> Tensor a
add = Linear.zipWith (+)

reduce :: (Num a, Storable a) => (a -> a -> a) -> Tensor a -> a
reduce f (Tensor _ _ v) = V.foldl1' f v

dot :: (Num a, Storable a) => Tensor a -> Tensor a -> a
dot t1 t2 = reduce (+) $ Linear.zipWith (*) t1 t2

fromMapping :: Storable a => Int -> Int -> (Int -> Int -> a) -> Tensor a
fromMapping rows cols f = Tensor rows cols (V.generate (rows * cols) g)
    where g i = let (q, r) = i `quotRem` rows in f r q

--Fast splice
colVector :: Storable a => Tensor a -> Int -> Tensor a
colVector (Tensor rows cols v) col
    | col >= 0 && col < cols = Tensor rows 1 (V.slice (col * rows) rows v)
    | otherwise              = error "Index out of bounds"

transpose :: Storable a => Tensor a -> Tensor a
transpose t@(Tensor rows cols _) = fromMapping cols rows (flip $ uncheckedIndex t)

mult :: (Num a, Storable a) => Tensor a -> Tensor a -> Tensor a
mult t1@(Tensor rows1 cols1 _) t2@(Tensor rows2 cols2 _)
    | cols1 == rows2 = fromMapping rows1 cols2 (\i j -> colVector ts i `dot` colVector t2 j)
    | otherwise      = error "Number of columns of the first tensor must match the number of rows of the second"
    where ts = Linear.transpose t1

(.*) :: (Num a, Storable a) => a -> Tensor a -> Tensor a
x .* t = Linear.map (* x) t

(*.) :: (Num a, Storable a) => Tensor a -> a -> Tensor a
t *. x = Linear.map (* x) t

withoutRow :: Storable a => Tensor a -> Int -> Tensor a
withoutRow t@(Tensor rows cols _) row = fromMapping (rows - 1) cols f
    where f i j | i >= row  = uncheckedIndex t (i + 1) j
                | otherwise = uncheckedIndex t i j

withoutCol :: Storable a => Tensor a -> Int -> Tensor a
withoutCol t@(Tensor rows cols _) col = fromMapping rows (cols - 1) f
    where f i j | j >= col  = uncheckedIndex t i (j + 1)
                | otherwise = uncheckedIndex t i j

det :: (Num a, Storable a) => Tensor a -> a
det t@(Tensor rows cols _) 
    | rows == 1 && cols == 1 = uncheckedIndex t 0 0
    | rows == cols           = sum $ Prelude.map (\i -> ((-1) ^ i) * det (laplace i) * uncheckedIndex t 0 i) [0..cols - 1]
    | otherwise              = error "Matrix needs to be square"
    where laplace i = withoutRow (withoutCol t i) 0

--Fast concat
uncheckedConcatCols :: Storable a => [Tensor a] -> Tensor a
uncheckedConcatCols [] = error "Cannot concat an empty list"
uncheckedConcatCols ts@(Tensor rows _ _ : _) = Tensor rows (length ts) $ V.concat $ Prelude.map (\(Tensor _ _ v) -> v) ts

concatCols :: Storable a => [Tensor a] -> Tensor a
concatCols [] = error "Cannot concat an empty list"
concatCols ts@(Tensor rows cols _ : _) 
    | all p ts  = uncheckedConcatCols ts
    | otherwise = error "All matrices must be the same height"
    where p (Tensor r c _) = r == rows && c == cols

rotateAroundX :: (Storable a, Floating a) => a -> Tensor a
rotateAroundX a = Tensor 4 4 $ V.fromList m
    where m = concat $ Data.List.transpose [
              [ 1, 0,      0,     0 ],
              [ 0, cos a, -sin a, 0 ],
              [ 0, sin a,  cos a, 0 ],
              [ 0, 0,      0,     1 ] ]

rotateAroundY :: (Storable a, Floating a) => a -> Tensor a
rotateAroundY a = Tensor 4 4 $ V.fromList m
    where m = concat $ Data.List.transpose [
              [ cos a, 0,  sin a, 0 ],
              [ 0,     1,  0,     0 ],
              [-sin a, 0,  cos a, 0 ],
              [ 0,     0,  0,     1 ] ]

rotateAroundZ :: (Storable a, Floating a) => a -> Tensor a
rotateAroundZ a = Tensor 4 4 $ V.fromList m
    where m = concat $ Data.List.transpose [
              [ cos a, -sin a, 0, 0 ],
              [ sin a,  cos a, 0, 0 ],
              [ 0,      0,     1, 0 ],
              [ 0,      0,     0, 1 ] ]

rotate :: (Storable a, Floating a) => a -> a -> a -> Tensor a
rotate x y z = rotateAroundX x `mult` rotateAroundY y `mult` rotateAroundZ z

e1 :: (Num a, Storable a) => Tensor a
e1 = Tensor 4 1 $ V.fromList [1, 0, 0, 0]

e2 :: (Num a, Storable a) => Tensor a
e2 = Tensor 4 1 $ V.fromList [0, 1, 0, 0]

e3 :: (Num a, Storable a) => Tensor a
e3 = Tensor 4 1 $ V.fromList [0, 0, 1, 0]

e4 :: (Num a, Storable a) => Tensor a
e4 = Tensor 4 1 $ V.fromList [0, 0, 0, 1]

zero :: (Num a, Storable a) => Tensor a
zero = Tensor 4 1 $ V.fromList [0, 0, 0, 0]

identity :: (Num a, Storable a) => Tensor a
identity = concatCols [e1, e2, e3, e4]

scale :: (Num a, Storable a) => a -> a -> a -> Tensor a
scale x y z = concatCols [x .* e1, y .* e2, z .* e3, e4]

translate :: (Num a, Storable a) => a -> a -> a -> Tensor a
translate x y z = concatCols [e1, e2, e3, Tensor 4 1 $ V.fromList [x, y, z, 1]]

perspective :: (Floating a, Storable a) => Tensor a
perspective = concatCols [e1, e2, e3 `add` (1 .* e4), e4]