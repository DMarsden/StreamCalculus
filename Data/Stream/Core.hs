module Data.Stream.Core where

data Stream a = Stream { front :: a, derivative :: (Stream a) }

streamAt 0 xs = front xs
streamAt n xs = streamAt (n - 1) (derivative xs)

merge xs ys = Stream (front xs) (merge ys xs)


takeN 0 xs = []
takeN n xs = (front xs) : (takeN (n-1) (derivative xs))

dropN 0 xs = xs
dropN n xs = dropN (n - 1) (derivative xs)

instance Functor Stream where
 fmap f xs = Stream (f $ front xs) (fmap f $ derivative xs)
