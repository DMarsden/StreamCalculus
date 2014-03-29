module Main where

data Stream a = Stream { front :: a,  derivative :: (Stream a) }

zeroes = constant 0

constant x = Stream x (constant 0)

main = print "prototype"
