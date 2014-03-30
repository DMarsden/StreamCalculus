module Data.Stream.Core where

import Data.List

data Stream a = Stream { front :: a, derivative :: (Stream a) }

streamAt :: Integer -> (Stream a) -> a
streamAt 0 xs = front xs
streamAt n xs = streamAt (n - 1) (derivative xs)

merge :: (Stream a) -> (Stream a) -> (Stream a)
merge xs ys = Stream (front xs) (merge ys xs)

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
