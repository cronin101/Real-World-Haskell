--Write a program that transposes the text in a file.
--  For instance, it should convert "hello\nworld\n" to "hw\neo\nlr\nll\nod\n".

import System.Environment (getArgs)

import qualified Data.List as L

transposeText :: String -> String
transposeText = unlines . L.transpose . lines

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile $ function input

main = mainWith myFunction
  where
    mainWith function = do
      args <- getArgs
      case args of
        [input, output] -> interactWith function input output
        _               -> putStrLn "Error: exactly two arguments needed"

    myFunction = transposeText

