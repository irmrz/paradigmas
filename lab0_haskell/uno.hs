duplica :: Int -> Int
duplica x = 2*x 

mas1 :: Int -> Int
mas1 x = x+1

polMap :: [a] -> (a -> a) -> [a]
polMap [] f = []
polMap (x:xs) f = f x : polMap xs f

esPar :: Int -> Bool
esPar x = x `mod` 2 == 0

polGenMap :: [a] -> (a -> b) -> [b]
polGenMap [] f = []
polGenMap (x:xs) f = f x : polGenMap xs f

filetInt :: [Int] -> (Int -> Bool) -> [Int]
filetInt [] f = []
filetInt (x:xs) f  | f x == True = x : filetInt xs f
                   | f x == False = filetInt xs f


-- 3
sumatoria :: [Int] -> Int          
sumatoria [] = 0                  
sumatoria (x:xs) = x + sumatoria (xs) 

foldInt :: [Int] -> ([Int] -> Int) -> Int
foldInt [] f = 0
foldInt (x:xs) f = f (x:xs)

morFoldInt :: [a] -> ([a] -> a) -> a
morFoldInt (x:xs) f = f (x:xs)


-- 4

type Radio = Float   --Define un "alias de tipo" (sinónimo)
type Lado = Float

data Figura = Circulo Radio        --Cada uno de estos es un _constructor_
            | Cuadrado Lado        --define el constructor de un "Cuadrado"
            | Rectangulo Lado Lado --define el constructor de un "Rectangulo"
            | Punto                --define el constructor de un "Punto"
              deriving (Eq, Show)

superficie :: Figura -> Float
superficie (Cuadrado lado) = lado ** 2
superficie (Circulo radio) = pi * (radio ** 2)
superficie (Rectangulo ancho alto) = alto * ancho
superficie Punto = error "La superficie de un punto es..."


-- 5
