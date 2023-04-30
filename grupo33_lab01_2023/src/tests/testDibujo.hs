-- Modulo para testear el modulo Dibujo.hs usando HUnit

import Prelude
import Test.HUnit
import Graphics.Gloss (Picture, circle, pictures)
import Dibujo (Dibujo, 
        figura, juntar, apilar, rot45, rotar,r180, r270, encimar, espejar,
        (.-.), (///),(^^^), figuras,cuarteto, ciclar, encimar4, mapDib, foldDib)

-- Test figuras constructoras

data Figura = Circulo | Cuadrado | Triangulo | Rombo | Cruz | Octagono deriving (Eq, Show)

data Figura2 = Int


x = (encimar (rotar (figura Circulo)) (figura Rombo))

y = (encimar (rotar (figura Circulo)) (figura Cuadrado))

w = encimar (rotar (figura 1)) (figura 4)

complex = apilar 1 1 (ciclar x) (encimar4 y)

-- assertEqual "descripcion" ValorEsperado funcionQueEjecuta
testFig :: Test 
testFig = TestCase $ assertEqual "para figura 1" ("Fig Circulo") 
                                                 (show $ figura Circulo)

testRotar :: Test
testRotar = TestCase $ assertEqual "para rotar 90" ("Rotar (Fig Circulo)")
                                                   (show $ rotar (figura Circulo))

testEspejar :: Test
testEspejar = TestCase $ assertEqual "para espejar" ("Espejar (Fig Circulo)")
                                                    (show $ espejar (figura Circulo))

testRot45 :: Test
testRot45 = TestCase $ assertEqual "para rot45" ("Rot45 (Fig Circulo)")
                                                (show $ rot45 (figura Circulo))

testApilar :: Test
testApilar = TestCase $ assertEqual "para apilar" ("Apilar 1.0 1.0 (Encimar (Rotar (Fig Circulo)) (Fig Rombo)) (Fig Rombo)") 
                                                  (show $ apilar 1 1 x (figura Rombo))


-- Test figuras combinadoras

testJuntar :: Test
testJuntar = TestCase $ assertEqual "para juntar" ("Juntar 3.0 2.0 (Fig Circulo) (Fig Rombo)") 
                                                  (show $ juntar 3 2 (figura Circulo) (figura Rombo))

-- assertEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
-- donde la primera a es el valor esperado y la segunda a es el valor obtenido
-- assertFailure :: String -> Assertion

--test para mapdib
shapeToNum :: Figura -> Dibujo Int
shapeToNum Circulo = figura 1
shapeToNum Cuadrado = figura 2
shapeToNum Triangulo = figura 3
shapeToNum Rombo = figura 4
shapeToNum Cruz = figura 5
shapeToNum Octagono = figura 6

testMapDib :: Test
testMapDib = TestCase $ assertEqual "para mapdib" x -- Checkea que sea igual a la identidad
                        (mapDib figura x)


testMapDib2 :: Test
testMapDib2 = TestCase $ assertEqual "pasar del tipo Dibujo Figura a Dibujo Int" w 
                        (mapDib shapeToNum x)


-- Test cuarteto

testCuarteto :: Test
testCuarteto = TestCase $ assertEqual "para cuarteto" 
        ("Apilar 1.0 1.0 (Juntar 1.0 1.0 (Fig Circulo) (Fig Rombo)) (Juntar 1.0 1.0 (Fig Cuadrado) (Fig Cruz))") 
        (show $ cuarteto (figura Circulo) (figura Rombo) (figura Cuadrado) (figura Cruz))

testCiclar :: Test
testCiclar = TestCase $ assertEqual "para ciclar"
                    (apilar 1 1 (juntar 1 1 (a) (b))
                                (juntar 1 1 (c) (d))) 
                    (ciclar (figura Circulo)) 
        where a = figura Circulo
              b = rotar (figura Circulo)
              c = r180(figura Circulo)
              d = r270(figura Circulo)

testr180 :: Test
testr180 = TestCase $ assertEqual "para r180"
                    ("Rotar (Rotar (Fig Circulo))") 
                    (show $ r180 (figura Circulo))

testr270 :: Test
testr270 = TestCase $ assertEqual "para r270"
                    ("Rotar (Rotar (Rotar (Fig Circulo)))") 
                    (show $ r270 (figura Circulo))

testr270B :: Test
testr270B = TestCase $ assertEqual "para r270"
                    (rotar(rotar (rotar x))) 
                    (r270 x)

testEncimar4 :: Test
testEncimar4 = TestCase $ assertEqual "para encimar4" 
                    (encimar a (encimar b (encimar c d)))
                    (encimar4 (figura Circulo)) 
        where a = figura Circulo
              b = rotar(figura Circulo)
              c = r180(figura Circulo)
              d = r270(figura Circulo)


testaux :: Test
testaux = TestCase $ assertEqual "para (^^^)"
                    (encimar x y)
                    ((^^^) x y)

testaux2 :: Test
testaux2 = TestCase $ assertEqual "para (///)"
                    (juntar 1 1 x y)
                    ((///) x y)

testaux3 :: Test
testaux3 = TestCase $ assertEqual "para (.-.)"
                    (apilar 1 1 x y)
                    ((.-.) x y)

testfiguras :: Test
testfiguras = TestCase $ assertEqual "para figuras"
                    ([Circulo,Rombo,Circulo,Cuadrado])
                    (figuras auxDib)
        where auxDib = apilar 1 1 x y


testfoldDib :: Test
testfoldDib = TestCase $ assertEqual "para foldDib contar figuras"
                (16)
                (foldDib aux1 id id id auxsum auxsum (+) complex)
        where aux1 x = 1
              auxsum x y z w = z+w 



tests = TestList [TestLabel "testFig" testFig,
    TestLabel "testRotar" testRotar,
    TestLabel "testEspejar" testEspejar,
    TestLabel "testRot45" testRot45,
    TestLabel "testApilar" testApilar,
    TestLabel "testJuntar" testJuntar,
    TestLabel "testMapDib" testMapDib,
    TestLabel "testMapDib2" testMapDib2,
    TestLabel "testCuarteto" testCuarteto,
    TestLabel "testCiclar" testCiclar,
    TestLabel "testr180" testr180,
    TestLabel "testr270" testr270,
    TestLabel "testr270B" testr270B,
    TestLabel "testEncimar4" testEncimar4,
    TestLabel "testaux" testaux,
    TestLabel "testaux2" testaux2,
    TestLabel "testaux3" testaux3,
    TestLabel "testfiguras" testfiguras,
    TestLabel "testfoldDib" testfoldDib
    ]