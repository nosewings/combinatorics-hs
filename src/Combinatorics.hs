module Combinatorics where

import Control.Applicative
import Control.Arrow

import Data.Function
import Data.List
import Data.Ratio
import Data.Traversable

infixr 8 ^\, ^/
infixl 5 `choose`

--------------------------------------------------------------------------------
-- List functions
--------------------------------------------------------------------------------

cartProd :: [a] -> [b] -> [(a, b)]
cartProd as bs = [(a, b) | a <- as, b <- bs]

countWhere :: (a -> Bool) -> [a] -> Int
countWhere f = filter f >>> length

count :: (Eq a) => a -> [a] -> Int
count x = countWhere (== x)

transpose' :: [[a]] -> [[a]]
transpose' = getZipList . sequenceA . map ZipList

windows :: Int -> [a] -> [[a]]
windows m = transpose' . take m . tails

windows2 :: [a] -> [(a, a)]
windows2 = windows 2 >>> map (\[x,y] -> (x,y))

sequences :: [a] -> [[[a]]]
sequences xs = iterate (fs <*>) [[]]
  where fs = map (:) xs

indicesWhere :: (a -> Bool) -> [a] -> [Int]
indicesWhere f
  =   map f
  >>> zip [0..]
  >>> filter snd
  >>> map fst

indices :: (Eq a) => a -> [a] -> [Int]
indices x = indicesWhere (== x)

--------------------------------------------------------------------------------
-- General combinatorics stuff
--------------------------------------------------------------------------------

factorial :: (Integral a) => a -> a
factorial n = product [1..n]

(!) :: (Integral a) => a -> a
(!) = factorial

subfactorial :: (Integral a) => a -> a
subfactorial n
  = (n!)%1 * sum [((-1)^k)%(k!) | k <- [0..n]]
  & numerator

falling :: (Integral a) => a -> a -> a
falling n k = product [n-k+1 .. n]

(^\) :: (Integral a) => a -> a -> a
(^\) = falling

rising :: (Integral a) => a -> a -> a
rising n k = product [n .. n+k-1]

(^/) :: (Integral a) => a -> a -> a
(^/) = rising

choose :: (Integral a) => a -> a -> a
n `choose` k = n^\k `div` (k!)

stirling2 :: (Integral a) => a -> a -> a
stirling2 n k = sum [(-1)^k * (k`choose`i) * (k-i)^n | i <- [0..k]] `div` (k!)

catalan :: (Integral a) => a -> a
catalan n = (2*n `choose` n) `div` (n+1)

--------------------------------------------------------------------------------
-- Statistics
--------------------------------------------------------------------------------

descents :: (Ord a) => [a] -> [Int]
descents
  =   windows2
  >>> indicesWhere (uncurry (>))
  >>> map (+1)

inv :: (Ord a) => [a] -> Int
inv
  =   tails
  >>> takeWhile (not . null)
  >>> concatMap (\(x:xs) -> filter (x >) xs)
  >>> length

maj :: (Ord a) => [a] -> Int
maj = sum . descents
