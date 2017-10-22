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
        start = calc (Atrib ("x") (Integer x)) (calc (Atrib "y" (Integer y)) (calc (Atrib "x1" (Integer x)) initial))
        store = calc ( LoopPre ((intAt start "y") :>: (Number 0)) ( Seq  (cmd1) (cmd2) ) ) start where
            cmd1 = Atrib "x" (Integer (calculate ( (intAt start "x") :+: (intAt start "x1") ))) --update x = x - x 
            cmd2 = Atrib "y" (Integer (calculate ((intAt start "y") :+: (Number (-1))))) -- update y = y - 1

intAt:: Store -> String -> ArithmeticExpression
intAt sto str = Number (integerOf (value sto str))