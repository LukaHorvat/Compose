module Common where

import Graphics.Rendering.OpenGL
import Foreign.Storable
import Utility
import Data.ByteString

makeVBO :: [GLdouble] -> IO BufferObject
makeVBO dat = do
    vbo <- genObjectName :: IO BufferObject
    bindBuffer ArrayBuffer $= Just vbo
    arr <- asArray dat
    usingPtr arr $ \ptr ->
        bufferData ArrayBuffer $= 
            (fromIntegral $ Prelude.length dat * sizeOf (undefined :: GLdouble), ptr, StaticDraw)
    return vbo

makeProgram :: ByteString -> ByteString -> IO Program
makeProgram vertSource fragSource = do
    v <- createShader VertexShader
    shaderSourceBS v $= vertSource
    compileShader v
    log1 <- get $ shaderInfoLog v
    Prelude.putStrLn log1

    f <- createShader FragmentShader
    shaderSourceBS f $= fragSource
    compileShader f
    log2 <- get $ shaderInfoLog f
    Prelude.putStrLn log2



    prog <- createProgram
    attachShader prog v
    attachShader prog f
    bindFragDataLocation prog "outColor" $= 0
    linkProgram prog
    return prog

makeVAO :: IO VertexArrayObject
makeVAO = genObjectName