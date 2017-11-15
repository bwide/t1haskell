module Parser where

import ParserSt
import Commands
import Primitives

boolParser::Parser Value
boolParser = (symbol "True" >=> \_ -> returnP (Bool True)) +++
    ((symbol "False" >=> \_ -> returnP (Bool False))) -- or expressions

-- do one of the above for numbers
aritParser::Parser Value
aritParser = parInteger >=> \int -> returnP (Integer (toInteger int)) -- or expressions

-- valueParser::Parser Value
-- valueParser = 

atribParser::Parser (String, Value)
atribParser = (many alphaNum) >=>
     \var -> (symbol ":=") >=>
     \_ -> boolParser >=>
     \val -> returnP (var, val)

-- getter

--sequence of expressions

-- ifs
ifParser::Parser Expression
ifParser = (symbol "if") >=> \_ -> boolParser >=> \bool -> 

-- loops