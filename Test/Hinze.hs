module Test.Hinze where

import Test.QuickCheck

import Data.Stream.Core
import Data.Stream.Hinze

prop_plusCommutative xs ys = all (test (xs + ys) (ys + xs)) [0..100]
 where
  test vs ws n = (streamAt n vs) == (streamAt n ws)
