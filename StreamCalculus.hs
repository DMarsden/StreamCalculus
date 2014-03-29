module Main where

import Control.Applicative

data Stream a = Stream { front :: a,  derivative :: (Stream a) }

zeroes = constant 0

combine binop xs ys = Stream ((front xs) `binop` (front ys)) (combine binop (derivative xs) (derivative ys))

instance Functor Stream where
 fmap f xs = Stream (f $ front xs) (fmap f (derivative xs))

instance Applicative Stream where
 pure x = Stream x (pure x)
 fs <*> xs = (front fs (front xs)) ((derivative fs) (derivative xs))

instance (Num a) => Num (Stream a) where
 xs + ys = combine (+) xs ys
 xs - ys = combine (-) xs ys
 xs * ys = combine (*) xs ys
 abs xs =  abs <$> xs
 signum xs = signum <$> xs
 fromInteger z = pure $ fromInteger z

instance (Fractional a) => Fractional (Stream a) where
 xs / ys = combine (/) xs ys
 recip xs = recip <$> xs
 fromRational r = pure $ fromRational r

instance (Floating a) => Floating (Stream a) where
 pi = pure pi
 exp xs = exp <$> xs
 sqrt xs = sqrt <$> xs
 log xs = log <$> xs
 xs ** ys = combine (**) xs ys
 logBase xs ys = combine logBase xs ys
 sin xs = sin <$> xs
 cos xs = cos <$> xs
 tan xs = tan <$> xs
 asin xs = asin <$> xs
 acos xs = acos <$> xs
 atan xs = atan <$> xs
 sinh xs = sinh <$> xs
 cosh xs = cosh <$> xs
 tanh xs = tanh <$> xs
 asinh xs = asinh <$> xs
 acosh xs = acosh <$> xs
 atanh xs = atanh <$> xs

main = print "prototype"
