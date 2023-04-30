module Main (main) where

import Data.Maybe (fromMaybe)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt)
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Interp (Conf(name), initial)
------- Dibujos --------
import Dibujos.Ejemplo (ejemploConf)

import qualified Dibujos.Feo as F
import qualified Dibujos.Escher as E
import qualified Dibujos.GrillaCord as G
-- Lista de configuraciones de los dibujos
configs :: [Conf]
configs = [ejemploConf, F.feoConf,E.escherConf, G.grilla_CordConf]

-- String es el nombre del dibujo = n
-- Dibuja el dibujo n
initial' :: [Conf] -> String -> IO ()
initial' [] n = do
    putStrLn $ "No hay un dibujo llamado " ++ n
initial' (c : cs) n = 
    if n == name c then
        initial c 600
    else
        initial' cs n

-- Imprime la lista de dibujos disponibles
printConfigs :: [Conf] -> IO ()
printConfigs [] = do
    putStrLn "¿Qué dibujo mostrar? "
    args <- getLine 
    initial' configs $ args

printConfigs (c : cs) = do
    putStrLn $ " -- " ++ name c
    printConfigs cs

main :: IO ()
main = do
    args <- getArgs
    case args of 
        []-> putStrLn $ "Uso: dibujos <nombre>"
        ("--lista":_) -> do
            putStrLn "Los dibujos disponibles son:"
            printConfigs configs
        [x]          -> initial' configs $ head [x]
