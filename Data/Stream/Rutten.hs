module Data.Stream.Rutten where

import Data.Stream.Core

embed z = Stream z (embed 0)
x = Stream 0 (embed 1)

instance (Num a) => Num (Stream a) where
 xs + ys = Stream (front xs + (front ys)) (derivative xs + (derivative ys))
 xs - ys = Stream (front xs - (front ys)) (derivative xs - (derivative ys))
 abs xs = fmap abs xs
 signum xs = fmap signum xs
 fromInteger z = embed (fromInteger z)
 xs * ys = Stream (front xs * (front ys)) (((derivative xs) * ys) + ((embed $ front xs) * (derivative ys)))
