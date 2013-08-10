--Toy functions created during Chapter 2.

add :: Num a => a -> a -> a
add a b = a + b

myDrop :: Int -> [a] -> [a]
myDrop n xs
  | n <= 0 || null xs = xs
  | otherwise         = myDrop (n - 1) xs

lastButOne :: [a] -> a
lastButOne xs
  | length xs < 2 = error "List has less than two elements"
  | otherwise     = xs !! ((length xs) - 2)
