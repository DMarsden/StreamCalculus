module Test.Rutten where

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.Stream.Core
import Data.Stream.Rutten

propPlusCommutative :: Stream Int -> Stream Int -> Bool
propPlusCommutative xs ys = all (test (xs + ys) (ys + xs)) [0..100]
 where
  test vs ws n = (streamAt n vs) == (streamAt n ws)

propFromIntegerFirstElem :: Integer -> Bool
propFromIntegerFirstElem z = z == streamAt 0 (fromInteger z)

propFromIntegerLaterElems :: Integer -> Bool
propFromIntegerLaterElems z = all (test $ fromInteger z) [1..100]
 where
  test xs n = 0 == streamAt n xs

main = defaultMain tests

tests = [testProperty "plus commutative" propPlusCommutative,
         testProperty "first element of fromInteger equals the integer" propFromIntegerFirstElem,
         testProperty "later elements of fromInteger are zero" propFromIntegerLaterElems
         ]
