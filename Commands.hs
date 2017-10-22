module Commands where

import Prelude
import Memory
import Primitives

calculateBool::BooleanExpression -> Bool
calculateBool (Boolean x) = x
calculateBool (And x y) = (calculateBool x) && (calculateBool y )
calculateBool (Or x y) = (calculateBool x) || (calculateBool y )
calculateBool (Not x) = not (calculateBool x)
calculateBool (x :>: y) = (calculate x) > (calculate y)

calculate::ArithmeticExpression -> Integer
calculate (Number x) = x
calculate (x :+: y) = (calculate x) + (calculate y)
calculate (x :*: y) = (calculate x) * (calculate y)


calc::Expression -> Store -> Store
calc Nil store = store
calc (Atrib string value) store = (update store value string)
calc (Seq cmd1 cmd2) store = calc cmd1 (calc cmd2 store)
calc (If boolExp cmd1 cmd2) store
    | calculateBool boolExp = calc cmd1 store
    | otherwise = calc cmd2 store
calc (LoopPre boolExp cmd) store
    | calculateBool boolExp = (calc (Seq cmd (LoopPre boolExp cmd)) store)
    | otherwise = calc Nil store
calc (LoopPost cmd boolExp) store = (calc (Seq cmd cmd2) store) where
    cmd2
        | calculateBool boolExp = (LoopPost cmd boolExp)
        | otherwise = calc Nil store

integerOf :: Value -> Integer
integerOf (Integer x) = x