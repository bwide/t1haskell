module Math where

import Primitives
import Memory
import Commands

sum :: Integer -> Integer -> Integer
sum x y =  calculate ( (Number x) :+: (Number y) )

mult :: Integer -> Integer -> Value
mult x y = 
    value store "x" where
        store = calc ( ( LoopPre (y1 :>: (Number 0)) ) ( Seq  (cmd1) (cmd2) ) ) start where
            cmd1 = --update x = x + x
            cmd2 = -- update y = y - 1
            y1 = Number (integerOf (value start "y"))
            start = calc (Atrib ("x") (Integer x)) (calc (Atrib "y" (Integer y)) initial)
            