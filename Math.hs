module Math where

import Primitives
import Memory
import Commands

sum :: Integer -> Integer -> Integer
sum x y =  calculate ( (Number x) :+: (Number y) )

mult :: Integer -> Integer -> Integer
mult x y = value "mult" (calculateExpression (LoopPre ((Number x) :<: (Number 1)) -- Expression )  