module Test.Core where

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.Stream.Core

propTakeNLength :: Stream Int -> Bool
propTakeNLength xs = all (test xs) [1..100]
 where
  test xs n = (length $ takeN n xs) == fromInteger n

propMerge :: Stream Int -> Stream Int -> Bool
propMerge xs ys = all (test $ merge xs ys) [0..100]
 where
  test zs n = streamAt (2 * n) zs == streamAt n xs && streamAt (2 * n + 1) zs == streamAt n ys

propEvenMerge :: Stream Int -> Stream Int -> Bool
propEvenMerge xs ys = all (test $ evens $ merge xs ys) [0..100]
 where
  test zs n = streamAt n zs == streamAt n xs

propOddMerge :: Stream Int -> Stream Int -> Bool
propOddMerge xs ys = all (test $ odds $ merge xs ys) [0..100]
 where
  test zs n = streamAt n zs == streamAt n ys

nats = Stream 0 (fmap (+1) nats)

propStreamAt :: Bool
propStreamAt = all test [0..100]
 where
  test n = streamAt n nats == n

propDropNRemaining :: Bool
propDropNRemaining = all test [0..100]
 where
  test n = streamAt 0 (dropN n nats) == n

propFMap :: Integer -> Bool
propFMap n = all test [0..100]
 where
  test m = streamAt m (fmap (+n) nats) == n + m

tests = [testProperty "takeN length" propTakeNLength,
         testProperty "merge" propMerge,
         testProperty "merge even" propEvenMerge,
         testProperty "merge odd" propOddMerge,
         testProperty "streamAt" propStreamAt,
         testProperty "dropN remaining" propDropNRemaining,
         testProperty "fmap" propFMap
         ]

main = defaultMain tests

