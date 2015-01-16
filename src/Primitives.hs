{-# LANGUAGE TypeOperators #-}
module Primitives where

import Components
import Graphics.UI.GLUT (BufferObject, GLdouble, GLint)
import TypeLevel
import Common
import Transform

type Primitive = Entity (Transform :-: IO BufferObject :-: GLint :-: Nil)

cube :: Primitive
cube = entity 
           `with` fromIntegral size
           `with` makeVBO bakedCube
           `with` Transform.identity
    where o = [-1.0, 1.0]
          bakedCube = concat [faceL, faceR, faceF, faceB, faceU, faceD] :: [GLdouble]
          faceL = concat $ bake [[-1,  y,  z] | y <- o, z <- o]
          faceR = concat $ bake [[ 1,  y,  z] | y <- o, z <- o]
          faceF = concat $ bake [[ x,  y, -1] | x <- o, y <- o]
          faceB = concat $ bake [[ x,  y,  1] | x <- o, y <- o]
          faceD = concat $ bake [[ x, -1,  z] | x <- o, z <- o]
          faceU = concat $ bake [[ x,  1,  z] | x <- o, z <- o]
          bake l = let [a, b, d, c] = l in [a, b, c, c, d, a]
          size = length bakedCube `div` 3

getVBO :: Primitive -> IO BufferObject
getVBO (Entity c) = c `index` one

getSize :: Primitive -> GLint
getSize (Entity c) = c `index` two