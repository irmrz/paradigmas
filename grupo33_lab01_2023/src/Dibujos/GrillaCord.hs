module Dibujos.GrillaCord(
    grilla_CordConf,
    Par(..),
    interpPar,
) where

import Dibujo (Dibujo,juntar,apilar,figura,rotar)

import Graphics.Gloss (Picture,translate, scale, white, line, polygon, pictures)
import Graphics.Gloss.Data.Picture(text)

import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import FloatingPic (Output, half, zero)
import Interp (Conf(..), interp)

type Par = (Int,Int)


lengthLines = 20

-- translate mueve el dibujo a las cordenadas x y
-- scale escala el dibujo por el factor x y -> en este caso son el mismo
-- fst toma un par y te devuelve su primer elemento 
-- snd toma un par y te devuelve su segundo elemento 

interpPar :: Output Par
interpPar (a,b) x w h =onePair (a,b) x w h
        where
          size = fst w * 0.002
          onePair (a,b) x w h = translate (fst x + 10) (snd x + (snd x)/20 + 10) $ 
            scale size size $
                text $ show (a,b)

-- los valores sumados son para que no tan esten pegados a los bordes


textEjemplo :: Par -> Dibujo Par
textEjemplo (a,b) = figura (a,b)

oneLine :: Int-> Int -> Dibujo Par
oneLine a 0 = textEjemplo (a,lengthLines)
oneLine a x = juntar (fromIntegral x) 1.0 (textEjemplo (a,(lengthLines-x))) (oneLine a (x-1))

fullGrid ::Int -> Dibujo Par
fullGrid 0 = oneLine lengthLines lengthLines
fullGrid x = apilar (fromIntegral x) 1.0 (oneLine (lengthLines-x) lengthLines) (fullGrid (x-1))

testAll :: Dibujo Par
testAll = fullGrid(lengthLines)

grilla_CordConf :: Conf
grilla_CordConf = Conf {
    name = "Cord",
    pic = interp interpPar testAll
}


