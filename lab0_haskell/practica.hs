--arbol binario

--para describir el tipo usamos data, le damos el nombre, y a, para que sea polimorfico,
--definimos los constructores

data Arbol a = Empty | Nodo (Arbol a) a (Arbol a) deriving(Eq, Show)

x = Nodo (Nodo Empty 4 Empty) 10 (Nodo Empty 8 Empty)
y = Nodo (Nodo Empty 6 (Nodo Empty 98 Empty)) 67 Empty 
z = Nodo (Nodo Empty 2 (Nodo Empty 5 (Nodo (Nodo Empty 2 Empty) 5 Empty))) 6 Empty


incrementar :: (Num a) => Arbol a -> a -> Arbol a
incrementar Empty _ =  Empty                                                --en _ podemos dejar el n
incrementar (Nodo l e r) n = Nodo (incrementar l n) (e+n) (incrementar r n) 

mapArbol :: Arbol a -> (a -> b) -> Arbol b
mapArbol Empty _ = Empty
mapArbol (Nodo l e r) f = Nodo (mapArbol l f) (f e) (mapArbol r f)

filterArbol :: Arbol a -> (a -> Bool) -> [a]
filterArbol Empty _ = []
filterArbol (Nodo l e r) f | f e  = e : filterArbol l f ++ filterArbol r f
                           | otherwise = filterArbol l f ++ filterArbol r f

mapIncrement :: (Num a) => Arbol a -> a -> Arbol a
mapIncrement arbol n = mapArbol arbol (+n)

--data Arbol a = Hoja a| Nodo (Arbol a) (Arbol a) deriving(Eq, Show)
            --Arbol a -> (a -> b)-> (b-> b -> b) -> b 
            
             
foldArbol :: Arbol a -> (a -> b -> b -> b)-> b -> b
foldArbol Empty _ x = x
foldArbol (Nodo l e r) f x = f e (foldArbol l f x) (foldArbol r f x)

--Int -> Bool -> Bool -> Bool
allNat :: Int  -> Bool -> Bool -> Bool
allNat x r1 r2 = (x > 0) && r1 && r2

--(e > (fromintegral 10)) && r1 && r2 
-- filterArbol y (> 10)           
-- foldArbol mi_arbol (allGreater10 . fromIntegral) True

listaDe :: a -> Int -> [a]
listaDe x n | even n =  replicate n x
            | odd n  = replicate (2 * n) x

alturaArbol :: Arbol a -> Int 
alturaArbol Empty = 0
alturaArbol (Nodo l e r) = 1 + max (alturaArbol l) (alturaArbol r)

-- Ejercicio 6 (c)

treeElems :: Arbol a -> [a]
treeElems Empty = []
treeElems (Nodo l  e  r) = treeElems l ++ [e] ++ treeElems r

--definir:
--un tipo "Expr" que permita representar una expresión aritmética sobre enteros
--(sin variables) con nuestros propios operadores :+:, :-:, :*: 
--Por ejemplo: (5 :*: 3) :+: 10 :-: 2 es una "Expr"


data Expr =  Valor  Int
            | Expr :+: Expr
            | Expr :-: Expr
            | Expr :*: Expr 
            deriving (Eq, Show)


w :: Expr
w = (Valor 5 :*: Valor 3) :+: Valor 10 :-: Valor 2

aritmetica :: Expr -> Int
aritmetica (Valor x) = x
aritmetica (a :+: b) = aritmetica a + aritmetica b
aritmetica (a :*: b) = aritmetica a * aritmetica b
aritmetica (a :-: b) = aritmetica a - aritmetica b

exprToString :: Expr -> String
exprToString (Valor x) = show x
exprToString (a :+: b) = exprToString a ++ " + " ++ exprToString b
exprToString (a :-: b) = exprToString a ++ " - " ++ exprToString b
exprToString (a :*: b) = exprToString a ++ " * " ++ exprToString b