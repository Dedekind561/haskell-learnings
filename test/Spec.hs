import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Data.Char as Char

my_map :: (a -> b) -> [a] -> [b]
my_map _ [] = []
my_map f (x:xs) = f x : map f xs

my_filter :: (a -> Bool) -> [a] -> [a]
my_filter _ [] = []
my_filter f list = [ z | z <- list, f z]

my_reverse :: [a] -> [a]
my_reverse [] = []
my_reverse list = reverse list

collect_vowels :: [Char] -> [Char]
collect_vowels [] = []
collect_vowels list = [ x | x <- list, x `elem` ['a','e','i','o','u'] ]

includes :: Char -> [Char] -> Bool
includes item list = item `elem` list

mapthenfilter :: (x -> Bool) -> (x -> y) -> [x] -> [y]
mapthenfilter _ _ [] = []
mapthenfilter pred iteratee list = [ iteratee x | x <- list , pred x]

alternate :: Integer -> Char -> Char
alternate num letter = if mod num 2 == 0 then Char.toLower letter else Char.toUpper letter

is_even :: Integer -> Bool
is_even n = mod n 2 == 0

total_list :: [Integer] -> Integer
total_list [] = 0
total_list (x:xs) = x + total_list xs

get_multiples :: [Integer] -> [Integer]
get_multiples list = [ x | x <- list, mod x 3 == 0 || mod x 5 == 0 ]

count_multiples :: [Integer] -> Integer
count_multiples list = total_list (get_multiples list)

rotate_array :: [Char] -> Int -> [Char]
rotate_array list n = slice list slice_point (length list) ++ slice list 0 slice_point
  where slice_point = (length list) - n

slice :: [Char] -> Int -> Int -> [Char]
slice list start end = take (end - start) (drop start list)

first_and_last :: [Char] -> [Char]
first_and_last list = [head list] ++ [last list]

alternate_case :: [Char] -> (Integer -> Char -> Char) -> [Char] -> Integer -> [Char]
alternate_case list func result i =
  if (length list == 0) then result
  else if (head list == ' ') then alternate_case (tail list) func (result ++ [head list]) i 
  else alternate_case (tail list) func (result ++ [func i (head list)]) (i + 1)

reduce :: (c -> a -> c) -> [a] -> c -> c
reduce f list acc = if (length list == 0) then acc else reduce f (tail list) (f acc (head list))

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)
    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)
    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

  describe "my_filter" $ do
    it "applies a function to each item in a collection" $ do
      my_filter (\x -> mod x 2 == 0) [1, 2, 3, 4 ,5, 6, 7 ,8, 9, 10] `shouldBe` [2, 4, 6, 8, 10]
    it "can filter out other items " $ do
      my_filter (\x -> x < 5) [1..30] `shouldBe` [1, 2, 3, 4]
      my_filter (\n -> mod n 2 == 0) [1..20] `shouldBe` [2,4,6,8,10,12,14,16,18,20]

  describe "collect_vowels" $ do
    it "can collect all the vowels in a given collection" $ do
      collect_vowels "Fraz" `shouldBe` "a"
      collect_vowels "Mitchell" `shouldBe` "ie"
      collect_vowels "haskell-learnings" `shouldBe` "aeeai"
      collect_vowels "boo" `shouldBe` "oo"
    
  describe "includes" $ do
    it "can check if an item is in a list" $ do
      includes 'a' ['a','b','c'] `shouldBe` True
      includes 'a' ['z','b','z'] `shouldBe` False
  
  describe "mapthenfilter" $ do
    it "can check if an item is in a list" $ do
      mapthenfilter (\n -> mod n 2 == 0) (\n -> n * 10) [1,2,3,4,5,6,7,8,9,10] `shouldBe` [20,40,60,80,100]
  
  describe "alternate_case" $ do
    it "can split map and join over an array of items" $ do
      alternate_case "northcoders" alternate [] 0 `shouldBe` "nOrThCoDeRs"
      alternate_case "abcd" alternate [] 0 `shouldBe` "aBcD"
      alternate_case "hello world" alternate [] 0 `shouldBe` "hElLo WoRlD"
  
  describe "alternate" $ do
    it "can capitalise a letter depending on a value being even or odd" $ do
      alternate 10 'a' `shouldBe` 'a'
      alternate 11 'a' `shouldBe` 'A'
      alternate 0 'a' `shouldBe` 'a'
  
  describe "is_even" $ do
    it "can check if a number is even or not" $ do
      is_even 10 `shouldBe` True
      is_even 11 `shouldBe` False
      is_even 0 `shouldBe` True
      is_even 5 `shouldBe` False
      is_even 25 `shouldBe` False
  
  describe "total_list" $ do
    it "get the total of a list of integers" $ do
      total_list [1,2,3] `shouldBe` 6
      total_list [1,2,3,4,5,6,7,8,9,10] `shouldBe` 55

  describe "count_multiples" $ do
    it "count all the multiples of 3 and 5" $ do
      count_multiples [1,2,3] `shouldBe` 3
      count_multiples [1,2,3,4,5] `shouldBe` 8
      count_multiples [1,2,3,4,5,6,7,8,9,10] `shouldBe` 33
      count_multiples [1..12] `shouldBe` 45

  describe "slice" $ do
    it "take a slice of a list of items" $ do
      slice ['a','b','c','d','e','f','g'] 2 6 `shouldBe` ['c','d','e','f']
      slice ['a','b','c','d','e','f','g'] 1 3 `shouldBe` ['b','c']
      slice ['a','b','c'] 0 2 `shouldBe` ['a','b']
      slice ['a','b','c','d'] 3 3 `shouldBe` []

  describe "take" $ do
    it "take the first 3 elements from a list" $ do
      take 3 "abcdefg" `shouldBe` "abc"
      take 4 "abcdefg" `shouldBe` "abcd"

  describe "rotate_array" $ do
    it "can rotate an array" $ do
      rotate_array ['a','b','c','d'] 1 `shouldBe` ['d','a','b','c']
      rotate_array ['a','b','c','d'] 2 `shouldBe` ['c','d','a','b']

  describe "head" $ do
    it "get the first item" $ do
      head [100,42,50] `shouldBe` 100
      head "abc" `shouldBe` 'a'
      head ['a','b','c'] `shouldBe` 'a'

  describe "first_and_last" $ do
    it "gets the first and last items from a list" $ do
      first_and_last "abcde" `shouldBe` "ae"
      first_and_last "mitch" `shouldBe` "mh"
      first_and_last "anat" `shouldBe` "at"

  describe "reduce" $ do
    it "can get the sum of a list of numbers" $ do
      reduce (\total num -> total + num) [1,2,3,4,5,6,7,8,9,10] 0 `shouldBe` 55

