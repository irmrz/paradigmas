module Pred (
  Pred,
  cambiar, anyDib, allDib, orP, andP
) where

import Dibujo (Dibujo, figura, rotar, espejar, rot45, apilar, juntar, encimar, r180, r270, (.-.), (///), (^^^), cuarteto, encimar4, ciclar,
    foldDib, mapDib, figuras)

type Pred a = a -> Bool

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar pred f d1 = mapDib aux_fun d1
  where aux_fun x | pred x = f x
                  | otherwise = figura x

-- Alguna básica satisface el predicado.

anyDib :: Pred a -> Dibujo a -> Bool
anyDib pred d1 = foldDib pred id id id orAux orAux (||) d1
    where orAux x y b1 b2 = b1 || b2

-- Todas las básicas satisfacen el predicado.

allDib :: Pred a -> Dibujo a -> Bool
allDib pred d1 = foldDib pred id id id andAux andAux (&&) d1
      where andAux x y b1 b2 = b1 && b2

-- Los dos predicados se cumplen para el elemento recibido.
andP :: Pred a -> Pred a -> Pred a
andP pred1 pred2 = \x -> pred1 x && pred2 x

-- Algún predicado se cumple para el elemento recibido.
orP :: Pred a -> Pred a -> Pred a
orP pred1 pred2 = \x -> pred1 x || pred2 x