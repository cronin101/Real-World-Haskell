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
