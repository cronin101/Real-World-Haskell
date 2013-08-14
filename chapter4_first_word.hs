--Using the command framework from the section called “A simple command
--  line framework”, write a program that prints the first word of each line
--  of its input.

import System.Environment (getArgs)

import qualified Data.Maybe as M

applyUnlessNull :: ([a] -> b) -> [a] -> Maybe b
applyUnlessNull f xs
  | null xs   = Nothing
  | otherwise = Just $ f xs

safeHead :: [a] -> Maybe a
safeHead = applyUnlessNull head

--Using the Maybe monad, lines that do not have any words are excluded from
-- the output.
firstWords :: String -> String
firstWords = unlines . M.mapMaybe (safeHead . words) . lines

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

    myFunction = firstWords

