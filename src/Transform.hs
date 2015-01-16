module Transform where

import Linear
import Graphics.Rendering.OpenGL.Raw.Core31
import Data.Vector.Storable
import qualified Data.Vector.Storable as V
import Graphics.Rendering.OpenGL.GL.Shaders.Uniform

data Transform = Transform
               { _position :: Tensor Double
               , _rotation :: Tensor Double
               , _scale :: Tensor Double }

newtype TransformMatrix = TransformMatrix (Tensor Double)

identity :: Transform
identity = Transform zero zero (fromColumnList [[1, 1, 1, 1]])

uniformMatrix :: (Real a, Storable a) => UniformLocation -> Tensor a -> IO ()
uniformMatrix (UniformLocation loc) (Tensor 4 4 v) = unsafeWith (V.map realToFrac v) $ glUniformMatrix4fv loc 1 0