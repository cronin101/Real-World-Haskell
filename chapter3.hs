import qualified Data.List as L
import qualified Data.Ord as O

--Concepts discussed in Chapter 3.

type BookID     = Int
type MagazineID = Int
type Title      = String
type Body       = [String]

data Book = Book {
  bookID    :: BookID,
  bookTitle :: Title,
  bookBody  :: Body
} deriving (Show)

data Magazine = Magazine {
  magazineID    :: MagazineID,
  magazineTitle :: Title,
  magazineBody  :: Body
} deriving (Show)


a_book = Book 01 "Some Book" ["Some", "Lines", "Of", "Text"]

type CustomerID = Int

data BookReview = BookReview {
  reviewedBook                     :: Book,
  bookReviewingCustomerID          :: CustomerID,
  bookReviewBody                   :: Body
} deriving (Show)

data MagazineReview = MagazineReview {
  reviewedMagazine                     :: Magazine,
  magazineReviewingCustomerID          :: CustomerID,
  magazineReviewBody                   :: Body
} deriving (Show)


a_book_review = BookReview a_book 01 ["This book was quite good."]

data BookRecord     = BookRecord Book BookReview
  deriving (Show)
data MagazineRecord = MagazineRecord Magazine MagazineReview

a_book_record = BookRecord a_book a_book_review

type CardHolder = String
type CardNumber = String
type Address    = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
  deriving (Show)

data Roygbiv = Red
             | Orange
             | Yellow
             | Green
             | Blue
             | Indigo
             | Violet
  deriving (Eq, Show)

data List a = Cons a (List a)
            | Nil
  deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

--Write the converse of fromList for the List type:
--  a function that takes a List a and generates a [a].
toList (Cons x (xs)) = x : (toList xs)
toList Nil           = []

data Tree a = Node a (Tree a) (Tree a)
            | Empty
  deriving (Show)

--Define a tree type that has only one constructor, like our Java example.
--Instead of the Empty constructor, use the Maybe type to refer to a node's children.
data MTree a = MNode {
  mTreeNode       :: a,
  leftMTreeChild  :: (Maybe (MTree a)),
  rightMTreeChild :: (Maybe (MTree a))
} deriving (Show)

tidySafeSecond :: [a] -> Maybe a
tidySafeSecond (_:x:_) = Just x
tidySafeSecond _       = Nothing

--Write a function that computes the number of elements in a list.
myLength []     = 0
myLength (x:xs) = 1 + myLength xs

--Add a type signature for your function to your source file.
myLength :: (Num i) => [a] -> i

--Write a function that computes the mean of a list.
myMean :: [Int] -> Double
myMean l    = sum / (fromIntegral $ length l)
  where sum = (foldr (+) 0 $ map fromIntegral l)

--Turn a list into a palindrome, i.e. it should read the same both backwards and forwards.
--For example, given the list [1,2,3], your function should return [1,2,3,3,2,1].
myPalindrome :: [a] -> [a]
myPalindrome l = l ++ (reverse l)

--Create a function that sorts a list of lists based on the length of each sublist.
mySortByLength :: [[a]] -> [[a]]
mySortByLength = L.sortBy (O.comparing length)

--Define a function that joins a list of lists together using a separator value.
intersperse :: a -> [[a]] -> [a]
intersperse sep (x:xs)
  | null xs   = x
  | otherwise = x ++ (sep : (intersperse sep xs))

--Write a function that will determine the height of a Tree.
treeHeight :: Tree a -> Int
treeHeight Empty                  = 0
treeHeight (Node this left right) = 1 + longestRemainingPath
  where
    longestRemainingPath          = max leftHeight rightHeight
    leftHeight                    = treeHeight left
    rightHeight                   = treeHeight right

--Consider three two-dimensional points a, b, and c.
--If we look at the angle formed by the line segment from a to b and the line segment from b to c, it either turns left, turns right, or forms a straight line.
--Define a Direction data type that lets you represent these possibilities.
data Point = Point {
  pX :: Int,
  pY :: Int
} deriving (Show)

data Direction = LeftTurn
               | StraightOn
               | RightTurn
  deriving (Show, Eq)

calculateDirection :: Point -> Point -> Point -> Direction
calculateDirection a b c
  | crossProductDirection > 0 = LeftTurn
  | crossProductDirection < 0 = RightTurn
  | otherwise                 = StraightOn
    where
      crossProductDirection   = ((x2 - x1) * (y3 - y1)) - ((y2 - y1) * (x3 - x1))
        where
          x1                  = pX(a)
          x2                  = pX(b)
          x3                  = pX(c)
          y1                  = pY(a)
          y2                  = pY(b)
          y3                  = pY(c)

--Define a function that takes a list of 2D points and computes the direction of each successive triple.
tripleDirections :: [Point] -> [Direction]
tripleDirections (a:b:c:[])   = [calculateDirection a b c]
tripleDirections (a:b:c:rest) = (calculateDirection a b c) : tripleDirections (b:c:rest)

--Using the code from the preceding three exercises, implement Graham's scan algorithm for the convex hull of a set of 2D points.
grahamScan :: [Point] -> [Point]
grahamScan points = firstPoint : scan sortedPoints
  where
    scan (a:b:c:[])
      | isLeftTurn a b c = [b]
      | otherwise        = []
    scan (a:b:c:rest)
      | isLeftTurn a b c = (b : (scan (b:c:rest)))
      | otherwise        = scan (a:c:rest)
    isLeftTurn a b c     = (calculateDirection a b c) == LeftTurn
    firstPoint           = head pointsByYValue
    pointsByYValue       = L.sortBy (O.comparing pY) $ L.sortBy (O.comparing pX) points
    sortedPoints         = firstPoint : (reverse $ L.sortBy (O.comparing polarAngle) (tail pointsByYValue))
      where
        polarAngle point   = angleBetween firstPoint (Point (pX point) 0) point
        angleBetween a b c = acos $ top / bottom
          where
            top                          = fromIntegral $ dotProduct ba bc
            bottom                       = magnitude(ba) * magnitude(bc)
            dotProduct (x1, y1) (x2, y2) = (x1 * x2) + (y1 * y2)
            magnitude (x, y)             = sqrt $ fromIntegral $ (x ^ 2) + (y ^ 2)
            ba                           = (pX(a) - pX(b), pY(a) - pY(b))
            bc                           = (pX(c) - pX(b), pY(c) - pY(b))

