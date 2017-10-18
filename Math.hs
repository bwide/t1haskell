module Math where

import Primitives
import Memory
import Commands

sum :: Integer -> Integer -> Integer
sum x y =  calculate ( (Number x) :+: (Number y) )