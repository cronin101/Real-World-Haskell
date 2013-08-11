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
