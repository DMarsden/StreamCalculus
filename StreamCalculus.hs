module Main where

data Stream a = Stream { front :: a,  derivative :: (Stream a) }

zeroes = constant 0

constant x = Stream x (constant 0)

combine :: (a -> a -> a) -> (Stream a) -> (Stream a) -> (Stream a)
combine binop xs ys = Stream ((front xs) `binop` (front ys)) (combine binop (derivative xs) (derivative ys))

instance Functor Stream where
 fmap f xs = Stream (f $ front xs) (fmap f (derivative xs))

instance (Num a) => Num (Stream a) where
 xs + ys = combine (+) xs ys
 xs - ys = combine (-) xs ys
 xs * ys = combine (*) xs ys
 abs xs = fmap abs xs
 signum xs = fmap signum xs
 fromInteger z = constant $ fromInteger z

main = print "prototype"
