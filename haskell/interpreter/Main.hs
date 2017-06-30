
module Main where

import AbsSyn
import Scanner
import Parser2
import Interpreter

inter :: String -> State -> State
inter = interpreter . parser . scanner

main :: IO ()
main =
  do
    prog <- readFile multAss
    putStr "program text:\n"
    putStr prog
    putStr "\ntoken list:\n"
    putStr ((show . scanner) prog)
    putStr "\n\nabstract syntax tree:\n"
    putStr ((show . parser . scanner) prog)
    putStr "\n\nexecute program:\n"
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

{-
Program example files
-}

intDiv = "ExamplePrograms/integerDivision.iml"
gaussSum = "ExamplePrograms/gaussSum.iml"
multAss = "ExamplePrograms/multiAssignment.iml"
