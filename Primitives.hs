module Primitives where

data ArithmeticExpression = 
    Number Integer
    | ArithmeticExpression :+: ArithmeticExpression 
    | ArithmeticExpression :*: ArithmeticExpression deriving (Eq, Show)

data BooleanExpression = 
    Boolean Bool 
    | ArithmeticExpression :>: ArithmeticExpression
    | ArithmeticExpression :<: ArithmeticExpression
    | And BooleanExpression BooleanExpression 
    | Or BooleanExpression BooleanExpression 
    | Not BooleanExpression deriving (Eq, Show)

data Value = Integer Integer | Bool Bool | Null deriving (Eq, Show)

data Expression = 
    Nil
    | Atrib String Value
    | Get String
    | Seq Expression Expression
    | If BooleanExpression Expression Expression
    | LoopPre BooleanExpression Expression
    | LoopPost Expression BooleanExpression deriving (Show)