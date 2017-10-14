module DataTypes where

import Prelude

data BooleanExpression = 
    Boolean Bool 
    | And BooleanExpression BooleanExpression 
    | Or BooleanExpression BooleanExpression 
    | Not BooleanExpression deriving (Eq, Show)

data ArithmeticExpression = 
    Number Integer 
    | ArithmeticExpression :+: ArithmeticExpression 
    | ArithmeticExpression :*: ArithmeticExpression deriving (Eq, Show)

data Valor = Integer | Bool deriving (Show, Eq)

data Expression = 
    Nil
    | Atrib Bool String
    | Atrib Integer String
    | Seq Expression Expression
    | Esc Expression Expression
    | LoopPre BooleanExpression Expression
    | LoopPost Expression BooleanExpression deriving (Show)

calculateBool::BooleanExpression -> Bool
calculateBool (Boolean x) = x
calculateBool (And x y) = (calculateBool x) && (calculateBool y )
calculateBool (Or x y) = (calculateBool x) || (calculateBool y )
calculateBool (Not x) = (calculateBool x)