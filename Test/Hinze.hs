module Test.Hinze where

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.Stream.Core
import Data.Stream.Hinze
import Control.Applicative
import Control.Monad
import Control.Comonad

propPlusCommutative :: Stream Int -> Stream Int -> Bool
propPlusCommutative xs ys = (xs + ys) `eq` (ys + xs)
 where
  eq = eqN 100

propTimesCommutative :: Stream Int -> Stream Int -> Bool
propTimesCommutative xs ys = (xs * ys) `eq` (ys * xs)
 where
  eq = eqN 100

propPureConstant :: Int -> Bool
propPureConstant x = all (test $ pure x) [1..100]
 where
  test xs n = x == streamAt n xs

main = defaultMain tests

tests = [testProperty "+ commutative" propPlusCommutative,
         testProperty "* commutative" propTimesCommutative,
         testProperty "pure constant" propPureConstant
         ]
