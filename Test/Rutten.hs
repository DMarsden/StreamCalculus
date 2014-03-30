module Test.Rutten where

import Test.QuickCheck

import Data.Stream.Core
import Data.Stream.Rutten

prop_plusCommutative xs ys = all (test (xs + ys) (ys + xs)) [0..100]
 where
  test vs ws n = (streamAt n vs) == (streamAt n ws)

prop_fromIntegerFirstElem z = z == streamAt 0 (fromInteger z)

prop_fromIntegerLaterElems z = all (test $ fromInteger z) [1..100]
 where
  test xs n = 0 == streamAt n xs
