module Math where

import Primitives
import Memory
import Commands
import Prelude hiding (sum)

sum :: Integer -> Integer -> Integer
sum x y =  calculate ( (Number x) :+: (Number y) )

mult :: Integer -> Integer -> Value
mult x y = 
    value s2 "x" where
        s1 = calc (Atrib ("x") (Integer x)) (calc (Atrib "y" (Integer y)) (calc (Atrib "x1" (Integer x)) store))
        s2 = calc ( LoopPre ((getI "y") :>: (Number 0)) ( Seq (cmd1) (cmd2) ) ) s1 where
            cmd1 = Atrib "x" (Integer (calculate ((getI "x") :+: (getI "x1") ))) --update x = x - x 
            cmd2 = Atrib "y" (Integer (calculate ((getI "y") :+: (Number (-1))))) -- update y = y - 1

intAt:: Store -> String -> ArithmeticExpression
intAt sto str = Number (integerOf (value sto str))