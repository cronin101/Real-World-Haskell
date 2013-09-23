-- Toy code to find the largest magnitude continuous slice of input list.

import Data.List

-- Datatype to store the magnitude of a list alongside its elements, to aid
-- memoization.
data SummedIntList = SummedIntList {
  listElements :: [Int],
  elementTotal :: Int
} deriving (Show)

-- Constructor for empty sublist.
emptySummedList :: SummedIntList
emptySummedList = SummedIntList [] 0

-- Returns a modified summed list resulting from adding an element.
addIntToList :: SummedIntList -> Int -> SummedIntList
addIntToList (SummedIntList xs t) x = SummedIntList (xs ++ [x]) (t + x)

largestSlice :: [Int] -> [Int]
largestSlice = scanList emptySummedList emptySummedList
  where
    -- Travel along the list storing the current greatest sublist and the
    -- current contender.
    scanList :: SummedIntList -> SummedIntList -> [Int] -> [Int]
    -- Return the contents of largest stored sublist when there are no more elements.
    scanList store curr []       = largestElements store curr
    scanList store curr (x:xs)
      -- Positive elements should be added to the competing sublist
      -- since they will increase its total.
      | x >= 0                   = scanList store (addIntToList curr x) xs
      -- Negative numbers will not increase the total so the contender can
      -- be reset and the largest stored list will be kept.
      | otherwise                = scanList (largest store curr) emptySummedList xs

    largest :: SummedIntList -> SummedIntList -> SummedIntList
    largest a b
      | elementTotal a > elementTotal b = a
      | otherwise                       = b

    largestElements :: SummedIntList -> SummedIntList -> [Int]
    largestElements = (listElements . ) . largest
