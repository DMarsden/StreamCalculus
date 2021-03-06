module Data.Stream.Hinze where

import Data.Stream.Core
import Control.Applicative
import Control.Monad
import Control.Comonad

instance Monad Stream where
  return = constant
  xs >>= f = join (fmap f xs)
    where
      join ~(Stream xs xss) = Stream (front xs) (join (fmap derivative xss))

instance Comonad Stream where
 extract = front
 extend f xs = Stream (f xs) (extend f $ derivative xs)

instance Applicative Stream where
 pure x = s 
  where
   s = Stream x s
 fs <*> xs = Stream (front fs (front xs)) ((derivative fs) <*> (derivative xs))

instance (Num a) => Num (Stream a) where
 xs + ys =  liftA2 (+) xs ys
 xs - ys = liftA2 (-) xs ys
 xs * ys = liftA2 (*) xs ys
 abs xs =  abs <$> xs
 signum xs = signum <$> xs
 fromInteger z = pure $ fromInteger z

instance (Fractional a) => Fractional (Stream a) where
 xs / ys = liftA2 (/) xs ys
 recip xs = recip <$> xs
 fromRational r = pure $ fromRational r

instance (Floating a) => Floating (Stream a) where
 pi = pure pi
 exp xs = exp <$> xs
 sqrt xs = sqrt <$> xs
 log xs = log <$> xs
 xs ** ys = liftA2 (**) xs ys
 logBase xs ys = liftA2 logBase xs ys
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
