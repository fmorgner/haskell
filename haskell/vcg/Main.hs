-- IML interpreter and VCG
-- Edgar F.A. Lederer, FHNW, 2010

module Main where

import AbsSyn
import Scanner
import Parser
import Interpreter
import VCG

front :: String -> Program
front = parser . scanner

inter :: String -> State -> State
inter = interpreter . front

vcg :: String -> [Assertion]
vcg = vcgProg . front

main :: IO ()
main = 
  do
    prog <- readFile "ExamplePrograms/intDiv.iml"
    let
      vcgs = vcg prog
    putStrLn ("vcgs =\n" ++ concat (map (\expr -> ppBExpr expr ++ "\n") vcgs))
    putStr "m = "
    mString <- getLine
    putStr "n = "
    nString <- getLine
    let
      outputState = inter prog inputState
        where
          m = read mString :: Int
          n = read nString :: Int
          inputState = updateS (updateS iniState ("m", m)) ("n", n)
      q = readS outputState "q"
      r = readS outputState "r"
    putStrLn ("q = " ++ show q)
    putStrLn ("r = " ++ show r)
