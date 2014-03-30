module Test.Core where

import Test.QuickCheck

import Data.Stream.Core

prop_takeN_length xs = all (test xs) [1..100]
 where
  test xs n = (length $ takeN n xs) == fromInteger n
