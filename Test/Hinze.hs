module Test.Hinze where

import Test.QuickCheck

import Data.Stream.Core
import Data.Stream.Hinze
import Control.Applicative

prop_plusCommutative xs ys = all (test (xs + ys) (ys + xs)) [0..100]
 where
  test vs ws n = (streamAt n vs) == (streamAt n ws)

prop_pureConstant x = all (test $ pure x) [1..100]
 where
  test xs n = x == streamAt n xs
