-- Modulo para testear el modulo Pred.hs usando HUnit

import Prelude
import Test.HUnit
import Graphics.Gloss (Picture, circle, pictures)
import Dibujo (Dibujo, 
        figura, juntar, apilar, rot45, rotar,
                    r180, r270, encimar, espejar, cuarteto, ciclar, encimar4, mapDib, foldDib)
import Pred (Pred, cambiar, anyDib, allDib, orP, andP)

data Figura = Circulo | Cuadrado | Triangulo | Rombo | Cruz | Octagono deriving (Eq, Show)

--Test funciones pred

-- dibujos auxiliares
x = (encimar (rotar (figura Circulo)) (figura Rombo))

y = (encimar (rotar (figura Circulo)) (figura Cuadrado))

w = encimar (rotar (figura 1)) (figura 4)

complex = apilar 1 1 (ciclar x) (encimar4 y)

x2 = (encimar (rotar (figura Cuadrado)) (figura Rombo))

y2 = (encimar (rotar (figura Cuadrado)) (figura Cuadrado))

complex2 = apilar 1 1 (ciclar x2) (encimar4 y2)

rouandArround = ciclar(ciclar (ciclar (figura Circulo)))

--Test cambiar

auxFunCambiar :: Figura -> Dibujo Figura
auxFunCambiar Circulo = figura Cuadrado
auxFunCambiar Cuadrado = figura Triangulo
auxFunCambiar Triangulo = figura Rombo
auxFunCambiar Rombo = figura Cruz
auxFunCambiar Cruz =  figura Octagono
auxFunCambiar Octagono = figura Circulo

testCambiar :: Test
testCambiar = TestCase $ assertEqual "para cambiar" (figura Cuadrado) 
                            (cambiar (== Circulo) auxFunCambiar (figura Circulo))

testCambiar2 :: Test
testCambiar2 = TestCase $ assertEqual "para cambiar" (complex2) 
                            (cambiar (== Circulo) auxFunCambiar complex)


-- Test anydib

testAnyDib :: Test
testAnyDib = TestCase $ assertEqual "para anydib" (True) (anyDib (== Circulo) (figura Circulo))

testAnyDib2 :: Test
testAnyDib2 = TestCase $ assertEqual "para anydib: no hay ningun circulo" 
                                (False) (anyDib (== Circulo) (complex2))

testAnyDib3 :: Test
testAnyDib3 = TestCase $ assertEqual "para anydib hay alguien que es circulo" 
                        (True) (anyDib (== Circulo) (complex))
-- Test allDib

testAllDib :: Test
testAllDib = TestCase $ assertEqual "para allDib" (False) (allDib (== Circulo) (figura Cuadrado))

testAllDib2 :: Test
testAllDib2 = TestCase $ assertEqual "para allDib" (False) (allDib (== Circulo) (complex))

testAllDib3 :: Test
testAllDib3 = TestCase $ assertEqual "para allDib todos circulos" (True) 
                (allDib (== Circulo) (rouandArround))


-- Test andP

testAndP :: Test
testAndP = TestCase $ assertEqual "para andP" (False) (aux Circulo)
        where aux = andP (== Circulo) (== Cuadrado)

-- Test orP
testOrp :: Test
testOrp = TestCase $ assertEqual "para orP" (True) (aux Circulo)
        where aux = orP (== Circulo) (== Cuadrado)

tests = TestList [TestLabel "testCambiar" testCambiar,
    TestLabel "testAnyDib" testAnyDib,
    TestLabel "testAllDib" testAllDib,
    TestLabel "testAllDib2" testAllDib2,
    TestLabel "testAllDib3" testAllDib3,
    TestLabel "testAndP" testAndP,
    TestLabel "testOrp" testOrp,
    TestLabel "testCambiar2" testCambiar2,
    TestLabel "testAnyDib2" testAnyDib2,
    TestLabel "testAnyDib3" testAnyDib3
    ]
