module DataTypes where

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


calculateExpression::Expression -> Store -> ()
calculateExpression Nil store = ()
