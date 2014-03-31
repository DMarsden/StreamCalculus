module Data.Stream.Core  where

import Data.List
import Test.QuickCheck

data Stream a = Stream { front :: a, derivative :: (Stream a) }

streamAt :: Integer -> (Stream a) -> a
streamAt 0 xs = front xs
streamAt n xs = streamAt (n - 1) (derivative xs)

merge :: Stream a -> Stream a -> Stream a
merge xs ys = Stream (front xs) (merge ys xs)

evens :: Stream a -> Stream a
evens xs = Stream (front xs) (derivative $ derivative xs)

odds :: Stream a -> Stream a
odds = evens . derivative

takeN :: Integer -> Stream a -> [a]
takeN 0 xs = []
takeN n xs = (front xs) : (takeN (n-1) (derivative xs))

dropN :: Integer -> (Stream a) -> (Stream a)
dropN 0 xs = xs
dropN n xs = dropN (n - 1) (derivative xs)

instance Functor Stream where
 fmap f xs = Stream (f $ front xs) (fmap f $ derivative xs)

instance (Show a) => Show (Stream a) where
 show xs = "[" ++ (concat $ intersperse ", " $ map show $ takeN 5 xs) ++ ", ..."

instance (Arbitrary a) => Arbitrary (Stream a) where
 arbitrary = do
              x <- arbitrary
              y <- arbitrary
              z <- arbitrary
              return $ testStream x y z
  where
   testStream x y z = Stream x (Stream y (Stream z $ testStream x y z))
