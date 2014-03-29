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

instance (Fractional a) => Fractional (Stream a) where
 xs / ys = combine (/) xs ys
 recip xs = fmap recip xs
 fromRational r = constant $ fromRational r

instance (Floating a) => Floating (Stream a) where
 pi = constant pi
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

main = print "prototype"
