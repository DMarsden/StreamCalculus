module Test.Core where

import Test.QuickCheck

import Data.Stream.Core

prop_takeN_length xs = all (test xs) [1..100]
 where
  test xs n = (length $ takeN n xs) == fromInteger n

prop_merge xs ys = all (test $ merge xs ys) [0..100]
 where
  test zs n = streamAt (2 * n) zs == streamAt n xs && streamAt (2 * n + 1) zs == streamAt n ys

prop_evenMerge xs ys = all (test $ evens $ merge xs ys) [0..100]
 where
  test zs n = streamAt n zs == streamAt n xs

prop_oddMerge xs ys = all (test $ odds $ merge xs ys) [0..100]
 where
  test zs n = streamAt n zs == streamAt n ys

nats = Stream 0 (fmap (+1) nats)

prop_streamAt_nats = all test [0..100]
 where
  test n = streamAt n nats == n

prop_dropN_remaining = all test [0..100]
 where
  test n = streamAt 0 (dropN n nats) == n

prop_fmap n = all test [0..100]
 where
  test m = streamAt m (fmap (+n) nats) == n + m
