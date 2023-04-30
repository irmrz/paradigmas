module Interp (
    interp,
    Conf(..),
    interpConf,
    initial
) where

import Graphics.Gloss(Vector, Picture, Display(InWindow), makeColorI, color, pictures, translate, white, display)
import Dibujo (Dibujo, foldDib)
import FloatingPic (FloatingPic, Output, grid,zero,half,opposite)

import qualified Graphics.Gloss.Data.Point.Arithmetic as V

-- Los vectores son de la forma (x, y)
-- Output toma algo de tipo a, tres vectores y devuelve una imagen -> toma 4 parametros 
-- Vector 1 es el punto de origen       -> x
-- Vector 2 es el punto de la derecha   -> w
-- Vector 3 es el punto de arriba       -> h
-- type Output a = a -> Vector -> Vector -> Vector -> Picture

union_pic :: Picture -> Picture -> Picture
union_pic p1 p2 = pictures [p1, p2]

-- Interpretación de un dibujo
-- formulas sacadas del enunciado
-- a -> x  -- b -> w -- c -> h
-- V.+ -> suma de vectores
-- fromIntegral pasa algo de tipo Int a algo de tipo Enum más general

rotarInterp :: FloatingPic -> FloatingPic
rotarInterp  f x w h = f (x V.+ w) h (opposite w)

rot45Interp :: FloatingPic -> FloatingPic
rot45Interp f x w h = f (x V.+ rot) rot (half (h V.- w))
                        where rot = half (w V.+ h) 

espejarInterp :: FloatingPic -> FloatingPic
espejarInterp f x w h = f (x V.+ w) (opposite w) h

encimarInterp :: FloatingPic -> FloatingPic -> FloatingPic
encimarInterp f g x w h  = (f x w h) `union_pic` (g x w h) 

apilarInterp :: Float -> Float -> FloatingPic ->FloatingPic -> FloatingPic
apilarInterp n m f g x w h  = (f (x V.+ h') w (r V.*h)) `union_pic` (g x w h')
                            where r' =  n / (n + m)
                                  r = m /(n + m)
                                  h' = r' V.* h

juntarInterp :: Float -> Float -> FloatingPic ->FloatingPic -> FloatingPic
juntarInterp n m f g x w h  =  (f x w' h) `union_pic` (g (x V.+ w') (r' V.* w) h)
                            where r' = n / (n + m)
                                  r =  m / (n + m)
                                  w' = r V.* w

interp :: Output a -> Output (Dibujo a)     --En base a nuestro tipo dibujo, tenemos que devolver una imagen, f dibuja algo ()
interp f x = foldDib f rotarInterp espejarInterp rot45Interp apilarInterp juntarInterp encimarInterp x 

-- Interpretación de un dibujo
-- formulas sacadas del enunciado
-- Configuración de la interpretación
data Conf = Conf {
        name :: String,
        pic :: FloatingPic
    }

interpConf :: Conf -> Float -> Float -> Picture 
interpConf (Conf _ p) x y = p (0, 0) (x,0) (0,y) 

-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
initial :: Conf -> Float -> IO ()
initial cfg size = do
    let n = name cfg
        win = InWindow n (ceiling size, ceiling size) (0, 0)
    display win white $ withGrid (interpConf cfg size size) size size
  where withGrid p x y = translate (-size/2) (-size/2) $ pictures [p, color grey $ grid (ceiling $ size / 10) (0, 0) x 10]
        grey = makeColorI 120 120 120 120