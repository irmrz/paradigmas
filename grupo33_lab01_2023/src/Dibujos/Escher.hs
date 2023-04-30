module Dibujos.Escher (
    escherConf,
    interpEscher
) where

import Graphics.Gloss (Picture, black, color, line, pictures, red, white, polygon, blank)

import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo (Dibujo, figura, juntar, apilar, rot45, rotar, r180, r270, encimar, espejar,cuarteto,ciclar, encimar4)
import FloatingPic (Output, half, zero)
import Interp (Conf(..), interp)


-- Supongamos que eligen
type Escher = Bool

-- La figura escher True es un triangulo rectangulo

interpEscher:: Output Escher
interpEscher True x w h = pictures [line $ triangulo x w h, cara x w h]
                        where
                        triangulo x w h = map (x V.+) [zero, h, w, zero]
                        cara x w h = polygon $ triangulo (x V.+ half h) (half w) (half h)
interpEscher False x w h = blank

-- x es el punto de origen
-- w es el ancho
-- h es el alto
-- a todos los vectores les sumox, que es el origen
-- [posicion inicial, linea entrex y la mitad de la altura + la mitad del ancho, posicion inicial + ancho, posicion inicial ]
-- polygon viene por defecto pintado de un color solido


figBas :: Escher -> Dibujo Escher
figBas True = figura True
figBas False = figura False

-- El dibujo t.
--over(fish, over(fish2, fish3))
dibujoT :: Dibujo Escher -> Dibujo Escher
dibujoT p1 = encimar p1 (encimar p2 p3)
        where p2 = espejar (rot45 p1)
              p3 = r270 p2              

-- El dibujoU.
-- above es apilar
-- over es encimar
dibujoU :: Dibujo Escher -> Dibujo Escher
dibujoU p = encimar4 p2 -- asi es la figura del medio del dibujo
        where p2 = espejar (rot45 p)

-- corner[0] = blank
-- corner[n] = quartet(corner[n-1], side[n-1], rot(side[n-1]), u)

esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina 0 p = figBas False
esquina n p = cuarteto (esquina (n-1) p) (lado (n-1) p) (rotar (lado (n-1) p)) (dibujoU p)

--side[n] = quartet(side[n-1],side[n-1],rot(t),t)
--side[0] = blank.
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 0 p = figBas False
lado n p = cuarteto (lado (n-1) p)  (lado (n-1) p)  (rotar (dibujoT p)) (dibujoT p)

-- Nonet version zulip

noneto p q r s t u v w x = apilar 2 1   (juntar 2.0 1.0 p  (juntar 1.0 1.0 q r))
                                        (apilar 1.0 1.0 (juntar 2.0 1.0 s (juntar 1.0 1.0 t u)) 
                                                        (juntar 2.0 1.0 v (juntar 1.0 1.0 w x)))

-- El dibujo de Escher:
escher :: Int -> Escher -> Dibujo Escher
escher n f = noneto p q r s t u v w x
    where   p = esquina n (figura f)
            q = lado n (figura f)
            r = r270(esquina n(figura f))       
            s = rotar(lado n(figura f))
            t = dibujoU (figura f)
            u = r270(lado n(figura f))          
            v = rotar(esquina n(figura f))
            w = rotar(rotar(lado n(figura f)))
            x = rotar(rotar(esquina n(figura f)))

-- testAll
testAll :: Dibujo Escher
testAll = escher 5 True

escherConf :: Conf
escherConf = Conf {
    name = "Escher",
    pic = interp interpEscher testAll
}