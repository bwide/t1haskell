module DataTypes where

import Prelude

data ArithmeticExpression = 
    Number Integer
    | BooleanValue Bool 
    | ArithmeticExpression :+: ArithmeticExpression 
    | ArithmeticExpression :*: ArithmeticExpression 
    | ArithmeticExpression :>: ArithmeticExpression
    | ArithmeticExpression :<: ArithmeticExpression deriving (Eq, Show)

data BooleanExpression = 
    Boolean Bool 
    | And BooleanExpression BooleanExpression 
    | Or BooleanExpression BooleanExpression 
    | Not BooleanExpression deriving (Eq, Show)

data Value = Integer Integer | Bool Bool

data Expression = 
    Nil
    | Atrib Bool String
    | Seq Expression Expression
    | If BooleanExpression Expression Expression
    | LoopPre BooleanExpression Expression
    | LoopPost Expression BooleanExpression deriving (Show)

calculateBool::BooleanExpression -> Bool
calculateBool (Boolean x) = x
calculateBool (And x y) = (calculateBool x) && (calculateBool y )
calculateBool (Or x y) = (calculateBool x) || (calculateBool y )
calculateBool (Not x) = not (calculateBool x)

calculate::ArithmeticExpression -> Value
calculate (Number x) = Integer x
calculate (BooleanValue x) =  Bool x
calculate (x :+: y) = Integer (x1 + x2)
    where 
        (Integer x1) = calculate x
        (Integer x2) = calculate y
calculate (x :*: y) = Integer (x1 * x2)
    where 
        (Integer x1) = calculate x
        (Integer x2) = calculate y
calculate (x :>: y) = Bool (x1 > y1)
    where
        (Bool x1) = calculate x
        (Bool y1) = calculate y



calculateExpression::Expression -> ()
calculateExpression Nil = ()
