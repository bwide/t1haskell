module DataTypes where

import Prelude

data 

data Primitive = PrimitiveBoolean Bool | PrimitiveNumber Integer

data Boolean = Boolean Bool |
               And Boolean Boolean |
               Or Boolean Boolean |
               Not Boolean

data ArithmeticExpression = Number Integer |
                            ArithmeticExpression :+: ArithmeticExpression |
                            ArithmeticExpression :*: ArithmeticExpression