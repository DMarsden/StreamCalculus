module Data.Stream.Core  where

import Data.List
import Test.QuickCheck

data Stream a = Stream { front :: a, derivative :: (Stream a) }

streamAt :: Integer -> (Stream a) -> a
streamAt 0 xs = front xs
streamAt n xs = streamAt (n - 1) (derivative xs)

merge :: Stream a -> Stream a -> Stream a
merge xs ys = unfold f g (xs,ys)
 where
  f (xs,ys) = front xs
  g (xs,ys) = (ys, derivative xs)

evens :: Stream a -> Stream a
evens = unfold front (derivative . derivative)

odds :: Stream a -> Stream a
odds = evens . derivative

takeN :: Integer -> Stream a -> [a]
takeN 0 xs = []
takeN n xs = (front xs) : (takeN (n-1) (derivative xs))

dropN :: Integer -> (Stream a) -> (Stream a)
dropN 0 xs = xs
dropN n xs = dropN (n - 1) (derivative xs)

eqN :: (Eq a) => Integer -> Stream a -> Stream a -> Bool
eqN 0 _ _ = True
eqN n xs ys = (front xs == front ys) && eqN (n-1) xs ys

constant :: a -> Stream a
constant x = s 
 where
  s = Stream x s

instance Functor Stream where
 fmap f = unfold (f . front) derivative

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

unfold :: (a -> b) -> (a -> a) -> a -> Stream b
unfold f g s = Stream (f s) (unfold f g (g s))

iterate :: (a -> a) -> a -> Stream a
iterate f = unfold id f
