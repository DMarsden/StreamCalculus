module Test.Hinze where

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.Stream.Core
import Data.Stream.Hinze
import Control.Applicative

propPlusCommutative :: Stream Int -> Stream Int -> Bool
propPlusCommutative xs ys = all (test (xs + ys) (ys + xs)) [0..100]
 where
  test vs ws n = (streamAt n vs) == (streamAt n ws)

propPureConstant :: Int -> Bool
propPureConstant x = all (test $ pure x) [1..100]
 where
  test xs n = x == streamAt n xs

main = defaultMain tests

tests = [testProperty "plus commutative" propPlusCommutative,
         testProperty "pure constant" propPureConstant
         ]
