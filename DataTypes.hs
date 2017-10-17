module DataTypes where

import Prelude

data ArithmeticExpression = 
    Number Integer 
    | ArithmeticExpression :+: ArithmeticExpression 
    | ArithmeticExpression :*: ArithmeticExpression 
    | ArithmeticExpression :>: ArithmeticExpression
    | ArithmeticExpression :<: ArithmeticExpression deriving (Eq, Show)

data BooleanExpression = 
    Boolean Bool 
    | And BooleanExpression BooleanExpression 
    | Or BooleanExpression BooleanExpression 
    | Not BooleanExpression deriving (Eq, Show)

data Value = Integer | Bool 

data Expression = 
    Nil
    | Atrib Bool String
    | Seq Expression Expression
    | Esc Expression Expression
    | LoopPre BooleanExpression Expression
    | LoopPost Expression BooleanExpression deriving (Show)

calculateBool::BooleanExpression -> Bool
calculateBool (Boolean x) = x
calculateBool (And x y) = (calculateBool x) && (calculateBool y )
calculateBool (Or x y) = (calculateBool x) || (calculateBool y )
calculateBool (Not x) = (calculateBool x)

calculate::ArithmeticExpression -> Value
calculate (Number x) = x
calculate (x :+: y) = (calculate x) + (calculate y)
calculate (x :*: y) = (calculate x) * (calculate y)
-- calculate (x :<: y) = BooleanValue (calculate x) < (calculate y)
-- calculate (x :>: y) = BooleanValue (calculate x) > (calculate y)

calculateExpression::Expression -> ()
calculateExpression Nil = ()