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


calculateExpression::Expression -> Store -> Store
calculateExpression Nil store = store
calculateExpression (Atrib string value) store = (update store value string)
calculateExpression (Seq cmd1 cmd2) store = calculateExpression cmd1 (calculateExpression cmd2 store)
calculateExpression (If boolExp cmd1 cmd2) store
    | calculateBool boolExp = calculateExpression cmd1 store
    | otherwise = calculateExpression cmd2 store
calculateExpression (LoopPre boolExp cmd) store
    | calculateBool boolExp = (calculateExpression (Seq cmd (LoopPre boolExp cmd)) store)
    | otherwise = calculateExpression Nil store
calculateExpression (LoopPost cmd boolExp) store = (calculateExpression (Seq cmd cmd2) store) where
    cmd2
        | calculateBool boolExp = (LoopPost cmd boolExp)
        | otherwise = Nil
            