{-# LANGUAGE LambdaCase #-}
module Dibujo (
    Dibujo,
    figura, rotar, espejar, rot45, apilar, juntar, encimar,
    r180, r270,
    (.-.), (///), (^^^),
    cuarteto, encimar4, ciclar,
    foldDib, mapDib,
    figuras
) where


--haremos nuestro lenguaje en base a una serie de figuras básicas, representadas por Fig
--y tendremos operaciones para hacerle a nuestras figuras
--con este lenguaje, vamos a definir funciones que combinen programas (Dibujo), para hacer otros
-- Esas funciones se van a llamar combinadores

{-Gramática de las figuras:
<> ::= Figura <Bas> | Rotar <Fig> | Espejar <Fig> | Rot45 <Fig>
    | Apilar <Float> <Float> <Fig> Fig<Fig> 
    | Juntar <Float> <Float> <Fig> <Fig> 
    | Encimar <Fig> <Fig>
-}


--en dibujo.hs solo nos preocupamos por las coordenadas

data Dibujo a =  Fig a | Rotar (Dibujo a) | Espejar (Dibujo a) | Rot45 (Dibujo a)
    | Apilar Float Float (Dibujo a) (Dibujo a)
    | Juntar Float Float (Dibujo a) (Dibujo a)
    | Encimar (Dibujo a) (Dibujo a)
    deriving (Eq, Show)

-- Agreguen los tipos y definan estas funciones

-- Construcción de dibujo. Abstraen los constructores.

figura :: a -> Dibujo a
figura = Fig

rotar :: Dibujo a -> Dibujo a  
rotar = Rotar

espejar :: Dibujo a -> Dibujo a
espejar = Espejar

rot45 :: Dibujo a -> Dibujo a
rot45 = Rot45

apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
apilar = Apilar 

juntar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
juntar = Juntar

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar = Encimar


-- Rotaciones de múltiplos de 90.
r180 :: Dibujo a -> Dibujo a
r180 d = rotar(rotar d)

r270 :: Dibujo a -> Dibujo a
r270 d = rotar(rotar(rotar d))

-- Pone una figura sobre la otra, ambas ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) = apilar 1.0 1.0


-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) = juntar 1.0 1.0

-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) = encimar

-- Dadas cuatro figuras las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto d1 d2 d3 d4 = juntar 1.0 1.0 (apilar 1.0 1.0 d1 d2) (apilar 1.0 1.0 d3 d4)

-- Una figura repetida con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 d1 = encimar d1 (encimar (rotar d1) (encimar (r180 d1) (r270 d1)))

-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar d1 = cuarteto d1 (r270 d1) (r180 d1) (rotar d1)  --hace un dibujo con 4 figuras rotadas en las
                                                        --4 direcciones en cada uno de los 4 cuadrantes 


-- PREGUNTA: dibujo vendría a ser la """""LISTA""""" de nuestro lenguaje?

-- FOLDDIB

-- Estructura general para la semántica (a no asustarse).  
-- Ayuda: pensar en foldr y las definiciones de intro a la lógica

-- foldDib se aplica a cada constructor de dibujo 
-- fold es la estructura para la semántica, va a hacer que funcione nuestro lenguaje. Toma 7 funciones,
-- un dibujo, y devuelve un elemento de tipo b
-- FOLDDIB LE ASIGNA UNA FUNCION A CADA CONSTRUCTOR!

foldDib :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Float -> Float -> b -> b -> b) -> 
       (Float -> Float -> b -> b -> b) -> 
       (b -> b -> b) ->
       Dibujo a -> b

foldDib f g h i j k l (Fig x) = f x
foldDib f g h i j k l (Rotar d1) = g(foldDib f g h i j k l d1)
foldDib f g h i j k l (Espejar d1) = h(foldDib f g h i j k l d1) 
foldDib f g h i j k l (Rot45 d1) = i(foldDib f g h i j k l d1)
foldDib f g h i j k l (Apilar x y d1 d2) = j x y (foldDib f g h i j k l d1) (foldDib f g h i j k l d2)
foldDib f g h i j k l (Juntar x y d1 d2) = k x y (foldDib f g h i j k l d1) (foldDib f g h i j k l d2)
foldDib f g h i j k l (Encimar d1 d2) = l (foldDib f g h i j k l d1) (foldDib f g h i j k l d2)


--MAPDIB

-- Demostrar que `mapDib figura = id`

mapDib :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
mapDib f (Fig x) =  f x
mapDib f (Rotar d1) = rotar(mapDib f d1)
mapDib f (Espejar d1) = espejar(mapDib f d1)
mapDib f (Rot45 d1) = rot45(mapDib f d1)
mapDib f (Apilar x y d1 d2) =  apilar x y (mapDib f d1) (mapDib f d2)
mapDib f (Juntar x y d1 d2) = juntar x y (mapDib f d1 ) (mapDib f d2)
mapDib f (Encimar d1 d2) = encimar (mapDib f d1) (mapDib f d2)


--FIGURAS

-- Junta todas las figuras básicas de un dibujo. Las figuras basicas de un dibujo es igual a la cant de ops 
-- que podemos hacerles a las figuras con nuestros constructores
figuras :: Dibujo a -> [a]
figuras (Fig x) = x : []
figuras (Rotar d1) = figuras(d1)
figuras (Espejar d1) = figuras(d1)
figuras (Rot45 d1) = figuras(d1)
figuras (Apilar _ _ d1 d2) = figuras(d1) ++ figuras(d2)
figuras (Juntar _ _ d1 d2) = figuras(d1) ++ figuras (d2)
figuras (Encimar d1 d2) = figuras(d1) ++ figuras(d2)
