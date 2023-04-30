module Interp (
    interp,
    Conf(..),
    interpConf,
    initial
) where

import Graphics.Gloss(Vector, Picture, Display(InWindow), makeColorI, color, pictures, translate, white, display)
import Dibujo (Dibujo, foldDib)
import FloatingPic (FloatingPic, Output, grid)

-- Los vectores son de la forma (x, y)  
type Output a = a -> Vector -> Vector -> Vector -> Picture

-- Interpretación de un dibujo
-- formulas sacadas del enunciado
{- interpcopilot :: Output a -> Output (Dibujo a)
interpcopilot outp = foldDib (fig outp) rot espej rot45 apil jun encim
  where
    fig outp' = outp'
    rot = rotate 90
    espej = scale (-1) 1
    rot45 = rotate 45
    apil x y d1 d2 = pictures [d1, translate x y d2]
    jun x y d1 d2 = pictures [d1, translate x y d2]
    encim d1 d2 = pictures [d1, d2]
 -}
--esa funcion interp me la tiro el copilot

interp :: Output a -> Output (Dibujo a)
interp outp = foldDib (fig outp) rot espej rot45 apil jun encim
  where
    fig outp' = outp'
    rotar = rotate 90
    espejar = scale (-1) 1
    rot45 = rotate 45
    apilar x y d1 d2 = pictures [d1, translate x y]
    juntar x y d1 d2 = pictures [d1, translate x y]
    encimar d1 d2 = pictures [d1, d2]


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