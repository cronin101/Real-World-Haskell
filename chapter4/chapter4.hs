import qualified Data.Char as C
import qualified Data.List as L

--Write your own “safe” definitions of the standard partial list functions,
--but make sure that yours never fail.
applyUnlessNull :: ([a] -> b) -> [a] -> Maybe b
applyUnlessNull f xs
  | null xs   = Nothing
  | otherwise = Just $ f xs

safeHead :: [a] -> Maybe a
safeHead = applyUnlessNull head

safeTail :: [a] -> Maybe [a]
safeTail = applyUnlessNull tail

safeLast :: [a] -> Maybe a
safeLast = applyUnlessNull last

safeInit :: [a] -> Maybe [a]
safeInit = applyUnlessNull init

--Write a function splitWith that acts similarly to words, but takes
--  a predicate and a list of any type, and splits its input list on every
--  element for which the predicate returns False.
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith predicate [] = [[]]
splitWith predicate xs = splitHelper predicate xs
  where
    splitHelper predicate xs
      | null suf  = [pre]
      | otherwise = pre : splitHelper predicate (tail suf)
        where (pre, suf) = break predicate xs

--Use a fold (choosing the appropriate fold will make your code much
--  simpler) to rewrite and improve upon the asInt function from the section
--  called “Explicit recursion”.
asIntFold :: String -> Int
asIntFold all@(x:xs)
  | x == '-'  = negate $ foldToInt xs
  | otherwise = foldToInt all
    where
      foldToInt = L.foldl' ((. C.digitToInt) . (+). (10 *)) 0

--The asInt_fold function uses error, so its callers cannot handle errors.
--Rewrite it to fix this problem.
type ErrorMessage = String
asIntEither :: String -> Either ErrorMessage Int
asIntEither all@(x:xs)
  | x == '-'  = fmap negate $ safeFoldToInt xs
  | otherwise = safeFoldToInt all
    where
      safeFoldToInt = L.foldl' processChar $ Right 0
        where
          processChar acc x
            | C.isDigit x = fmap ((C.digitToInt x +) . (10 *)) acc
            | otherwise   = Left $ "Non-digit '" ++ [x] ++ "' detected"
