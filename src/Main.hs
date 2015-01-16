{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString
import Data.IORef
import Graphics.UI.GLUT
import Common
import Foreign.Ptr
import Primitives
import Transform
import Linear

main :: IO ()
main = do
    vertSource <- Data.ByteString.readFile "vert.glsl"
    fragSource <- Data.ByteString.readFile "frag.glsl"
    getArgsAndInitialize

    createWindow "Hello"    
    vao <- makeVAO
    bindVertexArrayObject $= Just vao

    vbo <- getVBO cube  
    bindBuffer ArrayBuffer $= Just vbo

    prog <- makeProgram vertSource fragSource
    currentProgram $= Just prog

    loc <- get $ attribLocation prog "position" 
    vertexAttribArray loc $= Enabled
    vertexAttribPointer loc $= (ToFloat, VertexArrayDescriptor 3 Double 0 nullPtr) 

    time <- newIORef 0

    displayCallback $= display prog time (getSize cube)
    let update = do modifyIORef time (+(1.0/16))
                    addTimerCallback 16 update
                    postRedisplay Nothing
    update
    mainLoop

display :: Program -> IORef GLfloat -> GLint -> DisplayCallback
display prog ref size = do
    clear [ColorBuffer]
    time <- readIORef ref
    transLoc <- get $ uniformLocation prog "transform"
    let mat = Linear.perspective `mult` Linear.translate 0 0 (3 + realToFrac (sin time)) `mult` Linear.rotate time 0 0
    uniformMatrix transLoc mat
    uniLoc <- get $ uniformLocation prog "triangleColor"
    uniform uniLoc $= (Vertex3 1 0 0 :: Vertex3 GLfloat)
    drawArrays Triangles 0 size

    flush

