module Test.Rutten where

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.Stream.Core
import Data.Stream.Rutten

propPlusCommutative :: Stream Int -> Stream Int -> Bool
propPlusCommutative xs ys = (xs + ys) `eq` (ys + xs)
 where
  eq = eqN 100

propTimesCommutative :: Stream Int -> Stream Int -> Bool
propTimesCommutative xs ys = (xs * ys) `eq` (ys * xs)
 where
  eq = eqN 100

propFromIntegerFirstElem :: Integer -> Bool
propFromIntegerFirstElem z = z == streamAt 0 (fromInteger z)

propFromIntegerLaterElems :: Integer -> Bool
propFromIntegerLaterElems z = all (test $ fromInteger z) [1..100]
 where
  test xs n = 0 == streamAt n xs

main = defaultMain tests

tests = [testProperty "+ commutative" propPlusCommutative,
         testProperty "* commutative" propTimesCommutative,
         testProperty "first element of fromInteger equals the integer" propFromIntegerFirstElem,
         testProperty "later elements of fromInteger are zero" propFromIntegerLaterElems
         ]
