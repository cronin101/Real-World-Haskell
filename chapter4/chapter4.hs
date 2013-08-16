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

--Write your own definition of concat using foldr.
concat :: [[a]] -> [a]
concat = foldr (++) []

--Write your own definition of the standard takeWhile function, first using
--  explicit recursion, then foldr.
takeRecursiveWhile :: (a -> Bool) -> [a] -> [a]
takeRecursiveWhile _ [] = []
takeRecursiveWhile predicate (x:xs)
  | predicate x = x : takeRecursiveWhile predicate xs
  | otherwise   = []

takeFoldWhile :: (a -> Bool) -> [a] -> [a]
takeFoldWhile = flip foldr [] . stepUnlessFalse
  where
    stepUnlessFalse predicate x acc
      | predicate x = x : acc
      | otherwise   = []

--Use ghci to load the Data.List module and figure out what groupBy does,
--  then write your own implementation using a fold.
myGroupByRec :: (a -> a -> Bool) ->  [a] -> [[a]]
myGroupByRec e = groupHelper []
  where
    groupHelper acc [] = acc
    groupHelper acc (x:xs)
      | null acc    = groupHelper [[x]] xs
      | e x current = groupHelper (init acc ++ [last acc ++ [x]]) xs
      | otherwise   = groupHelper (acc ++ [[x]]) xs
        where current = last $ last acc

myGroupByFold :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupByFold e = foldr step []
  where
    step x acc
      | null acc    = [[x]]
      | e x current = (x : head acc) : tail acc
      | otherwise   = [] : [x] : acc
        where current = last $ last acc

--How many of the following Prelude functions can you rewrite using list
--  folds?
myAny :: (a -> Bool) -> [a] -> Bool
myAny = (foldr (||) False .) . map

myCycle :: [a] -> [a]
myCycle [] = error "empty list"
myCycle xs = foldr append [] [1..]
  where
    append _ acc = xs ++ acc

myWords :: String -> [String]
myWords = filter (not . (== " ")) . myGroupByFold (( . C.isSpace) . (==) . C.isSpace)

myUnLines :: [String] -> String
myUnLines =  foldr (( . ('\n' :)) . (++)) ""
