module Pred (
  Pred,
  cambiar, anyDib, allDib, orP, andP
) where

import Dibujo (foldDib)

--import Dibujo --PREGUNTAR como hacer el import
--no me anda el import

type Pred a = a -> Bool

--CAMBIAR

--  Dado un predicado (una condición) sobre figuras básicas, cambiar todas las que satisfacen
--el predicado, por la figura básica indicada por el segundo argumento.
--si la figura satisface el predicado, la cambio
--PREGUNTA: Para qué me sirve esto? rta: puede ser que quiera aplicar una funcion a todos las 
--figuras de mi dibujo. Si, es eso por `cambiar (== Triangulo) (\x -> Rotar (Figura x))`

-- Entonces, a las figuras que satisfacen el predicado, les aplico la funcion del segundo argumento
-- PREGUNTA aparte: importa el orden de los tipos, en la definicion de tipos de una funcion?
-- pensemos que nuestro predicado es la f foldDib
-- el problema con usar fold acá es que devuelve algo del tipo b, y no del tipo Dibujo a

cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar pred f d1 | pred d1 = f d1        --d1 no puede ser pq es un dibujo y pred toma cosas de tipo a, lo mismo con f
                  | otherwise = pred f d1

--Tratemos de hacer ´cambiar´, pero con foldDib, como supuestamente deberia ser.
--Con foldDib, cómo sé qué función tengo que aplicar? rta: folddib, por pattern matching detecta eso

--cambiar1 :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
--cambiar1 pred fun d1 = cambiar1 (pred d1) (\x -> foldDib f g h i j k l d1)

--tenemos que llegar a fig de x y ver que se cumple pred

--auxcambiar :: Pred a -> a -> (a -> Dibujo a) -> b
--auxcambiar pred x fun | pred x = f x
--                      | otherwise = x

-- usando esta funcion o algo por el estilo tendriamos que poder obtener algo de tipo a con algo de tipo b

--cambiar1 :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
--cambiar1 pred fun d1 = foldDib (auxcambiar pred) id id id id id id d1

-- cambiar2 :: [a] -> Pred -> [a]
-- cambiar2 (x:xs) f | f x == True = a : cambiar xs f 
--                   | f x == False = cambiar xs f

--DUDA: Si en todo caso, nuestra "lista" es dibujo, entonces la función cambiar,
--debería ser reemplazando los parametros que estan como lista, por el tipo Dibujo.
--PREGUNTAR: está bien el tipo de ´cambiar´ que está en el enunciado?
--PREGUNTAR: está bien que las siguientes funciones tomen un dibujo? Rta: creo que si pq las listas son dibujos (creo)

cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar pred f d1 = mapDib aux_fun d1
  where aux_fun x | pred x = f x
                  | otherwise = figura x



--foldDib hay que usarla en las siguientes funciones. toma 7 funciones, un dibujo, 
--ANYDIB

--Alguna figura básica satisface el predicado.

oraux :: Float -> FLoat -> Bool -> Bool -> Bool
oraux x y b1 b2 = b1 || b2

anyDib :: Pred a -> Dibujo a -> Bool    -- el predicado se lo pasamos nosotros puede ser cualquier cosa
anyDib pred d1 = foldDdib pred id id id oraux oraux (||) d1


--ALLDIB

-- Todas las básicas satisfacen el predicado.
--
andaux :: Float -> FLoat -> Bool -> Bool -> Bool
andaux x y b1 b2 = b1 && b2

allDib :: Pred a -> Dibujo a -> Bool
allDib pred d1 = foldDib pred id id id andaux andaux (&&) d1

--ANDP

-- acá debería preguntar si una figura satisface 2 cualidades?
-- Los dos predicados se cumplen para el elemento recibido.
-- osea le paso dos pred
-- en que momento voy a usar andp y orp?

andP :: Pred a -> Pred a -> Pred a
andP pred1 pred2 = pred1 x && pred2 x

--ORP

-- Algún predicado se cumple para el elemento recibido.

orP :: Pred a -> Pred a -> Pred a
orP pred1 pred2 = pred1 x || pred2 x

--lo mismo que la anterior pero solo 1 cualidad