module Data.Stream.Rutten where

import Data.Stream.Core

embed :: (Num a) => a -> Stream a
embed z = Stream z (embed 0)

x = Stream 0 (embed 1)

combine f xs ys = Stream (f (front xs) (front ys)) (combine f (derivative xs) (derivative ys))

(|*|) :: (Num a) => Stream a -> Stream a -> Stream a
xs |*| ys = Stream (front xs * (front ys)) (((derivative xs) |*| ys) + ((embed $ front xs) |*| (derivative ys)))

infixl 7 |*|

(|/|) :: (Fractional a) => Stream a -> Stream a -> Stream a
xs |/| ys = xs |*| inv ys

infixl 7 |/|

(|**|) :: (Num a) => Stream a -> Integer -> Stream a
xs |**| 0 = fromInteger 1
xs |**| n = xs |*| (xs |**| (n - 1))

inv :: (Fractional a) => (Stream a) -> (Stream a)
inv xs = Stream (recip $ front xs) ((embed (-front xs)) |*| (derivative xs |*| inv xs))

infixr 8 |**|

-- We do pointwise operations for these, will handle
-- special cases such as stream multiplication and exp
-- using separate syntax to avoid confusion.

instance (Num a) => Num (Stream a) where
 xs + ys = combine (+) xs ys
 xs - ys = combine (-) xs ys
 xs * ys = combine (*) xs ys
 abs xs = fmap abs xs
 signum xs = fmap signum xs
-- This is the key difference to the Hinze scheme
 fromInteger z = embed $ fromInteger z

instance (Fractional a) => Fractional (Stream a) where
 xs / ys = combine (/) xs ys
 recip xs = fmap recip xs
 fromRational r = embed $ fromRational r

instance (Floating a) => Floating (Stream a) where
 pi = embed pi
 exp xs = fmap exp xs
 sqrt xs = fmap sqrt xs
 log xs = fmap log xs
 xs ** ys = combine (**) xs ys
 logBase xs ys = combine logBase xs ys
 sin xs = fmap sin xs
 cos xs = fmap cos xs
 tan xs = fmap tan xs
 asin xs = fmap asin xs
 acos xs = fmap acos xs
 atan xs = fmap atan xs
 sinh xs = fmap sinh xs
 cosh xs = fmap cosh xs
 tanh xs = fmap tanh xs
 asinh xs = fmap asinh xs
 acosh xs = fmap acosh xs
 atanh xs = fmap atanh xs
