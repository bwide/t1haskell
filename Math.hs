module Math where

import Primitives
import Memory
import Commands
import Prelude hiding (sum)

sum :: Integer -> Integer -> Integer
sum x y =  calculate ( (Number x) :+: (Number y) )

mult :: Integer -> Integer -> Value
mult x y = 
    value store "x" where
        store = calc ( ( LoopPre ((y1 start) :>: (Number 0)) ) ( Seq  (cmd1) (cmd2) ) ) start where
            cmd1 = Atrib "x" (Integer (calculate (x1 :+: x2))) where --update x = x - x 
                x1 = (Number (integerOf (value start "x1")))
                x2 = (Number (integerOf (value start "x")))
            cmd2 = Atrib "y" (Integer (calculate ((y1 start) :+: (Number (-1))))) -- update y = y - 1
            y1 sto = Number (integerOf (value sto "y"))
            start = calc (Atrib ("x") (Integer x)) (calc (Atrib "y" (Integer y)) (calc (Atrib "x1" (Integer x)) initial))
            