module Parser where

import ParserSt
import Commands
import Primitives

boolParser::Parser BooleanExpression
boolParser = 
    (symbol "(" >=> \_ -> boolParser >=> \b1 -> symbol "||" >=> \_ -> boolParser >=> \b2 -> symbol ")" >=> \_ -> returnP (Or b1 b2)) +++
    (symbol "(" >=> \_ -> boolParser >=> \b1 -> symbol "," >=> \_ -> boolParser >=> \b2 -> symbol ")" >=> \_ -> returnP (And b1 b2)) +++
    (symbol "!" >=> \_ -> boolParser >=> \b -> returnP (Not b)) +++
    (symbol "True" >=> \_ -> returnP (Boolean True)) +++
    ((symbol "False" >=> \_ -> returnP (Boolean False)))

parser123::Parser Int
parser123 = symbol "x" >=> \_ -> parInteger >=> \var -> returnP (var) 

-- do one of the above for numbers
aritParser::Parser ArithmeticExpression
aritParser = 
    ( parInteger >=> \int -> symbol "-" >=> \_ -> parInteger >=> \int1 -> returnP ( (Number (toInteger int)) :+: (Number (toInteger (-int1))) ) ) +++
    ( parInteger >=> \int -> symbol "*" >=> \_ -> parInteger >=> \int1 -> returnP ( (Number (toInteger int)) :*: (Number (toInteger int1)) ) ) +++
    ( parInteger >=> \int -> symbol "+" >=> \_ -> parInteger >=> \int1 -> returnP ( (Number (toInteger int)) :+: (Number (toInteger int1)) ) ) +++
    (parInteger >=> \int -> returnP (Number (toInteger int))) -- or expressions

atribParser::Parser Expression
atribParser = (many alphaNum) >=>
     \var -> (symbol ":=") >=>
     \_ -> aritOrBool >=>
     \val -> returnP (Atrib var val)

seqParser::Parser Expression
seqParser = 
    expressionParser >=>
    \cmd1 -> (symbol ";") >=>
    \_ -> expressionParser >=>
    \cmd2 -> returnP (Seq cmd1 cmd2)

ifParser::Parser Expression
ifParser = 
    (symbol "if") >=> \_ -> symbol "(" >=> \_ -> boolParser >=>
    \bool -> symbol ")" >=> \_ -> 
    (symbol "{") >=> \_ -> 
    expressionParser >=> \cmd ->
    (symbol "}") >=> \_ -> 
    ((symbol "else") >=> \_ -> 
    (symbol "{") >=> \_ -> 
    expressionParser >=> \cmd1 ->
    (symbol "}") >=> \_ -> (returnP (If bool cmd cmd1))) +++ 
    (symbol "" >=> \_ -> returnP (If bool cmd Nil))

-- loops 
preLoopParser::Parser Expression
preLoopParser = 
    (symbol "while") >=> \_ -> symbol "(" >=> \_ -> boolParser >=>
    \bool -> symbol ")" >=> \_ -> 
    (symbol "{") >=> \_ -> 
    expressionParser >=> \cmd ->
    (symbol "}") >=> \_ -> returnP (LoopPre bool cmd)

postLoopParser::Parser Expression
postLoopParser = 
    (symbol "do") >=> \_ -> (symbol "{") >=> \_ -> 
    expressionParser >=> \cmd ->
    (symbol "}") >=> \_ ->
    (symbol "while") >=> \_ -> symbol "(" >=> \_ -> boolParser >=> \bool -> symbol ")" >=>
    \_ ->  returnP (LoopPre bool cmd)

-- getter
-- boolVar = (many alphaNum) >=> \s -> returnP (boolOf (value store s)) >=> \b ->
    
-- MARK: - helpers
boolExp::String -> BooleanExpression
boolExp s = (fst ((parse boolParser s) !! 0) )

aritExp::String -> ArithmeticExpression
aritExp s = (fst ((parse aritParser s) !! 0) )

aritOrBool::Parser Value
aritOrBool = 
    (boolParser >=> \val -> returnP (Bool (calculateBool val))) +++
    (aritParser >=> \val -> returnP (Integer (calculate val)))

expressionParser::Parser Expression
expressionParser = ifParser +++ preLoopParser +++ postLoopParser +++ atribParser +++ seqParser