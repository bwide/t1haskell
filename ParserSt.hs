module ParserSt where 

{-
Um parser é um programa que toma um string de caracteres e produz alguma fo
de árvore que torna explícita a estrutura sintática de um string.

Exemplo: string = 2*3+4 tree= Plus (Times (L 2) (L3)) (L 4)

Portanto, 

type Parser = String -> Tree

Entretanto, um parser em geral, pode não conparSumir todo o string. Desta forma,

type Parser = String -> (Tree, String)

Além disso, um parser nem sempre sucede. Por exemplo, um parser para números
aplicado a um string. Poratnto, um parser pode ser do tipo:

type Parser = String -> [(Tree, String)]

com a convenção de que uma lista vazia denota um fracasso e uma lista unitária
representa sucesso.

Finalmente, diferentes parser (para números, identificadors, caracteres, etc) 
retornam diferentes tipos de árvores. Desta forma, deixamos o tipo específico
de árvore como um parâmetro.
-}

type Parser treeType = String -> [(treeType, String)]

{-
Parsers Elementares
-}

-- return v sempre sucede, retornando v sem conparSumir o string s
returnP:: a -> Parser a 
returnP v = \s -> [(v,s)]

retex01 = returnP 2 "abc"
retex02 = returnP 'a' "hello"

-- failure sempre fracassa, retornando a lista vazia
failure::Parser a 
failure = \s -> [] 

faiex01 = failure "hello"

-- fracassa se o string é vazio. Do contrário, retorna o primeiro caracter.
item::Parser Char
item = \s -> case s of 
	           "" -> []
	           (x:xs) -> [(x,xs)]

itex01 = item "hello"

-- parser application function
parse::Parser a -> String -> [(a,String)]
parse p inp = p inp 

parex01 = parse (returnP 2) "Hello"
parex02 = parse item "Hello"
parex03 = parse failure "Hello"

{-
  Composição ou Sequência de Parsers

  parser p fracassa se a aplicação do parser p ao string
  de entrada falha. De outra forma, aplica a função ao
  valor resultado para retornar um segundo parser, o qual
  aplicado ao string resultado retorna o valor final.
-}

(>=>)::Parser a -> (a -> Parser b) -> Parser b
p >=> f = \s -> case parse p s of 
	              []-> [] 
	              [(v,out)]-> parse (f v) out

par01 :: Parser(Char,Char)
par01 = item >=> \v1 -> 
        item >=> \v2 -> returnP (v1,v2)

{-
f = \v1 -> item >=>
-}

par02 = item >=> \v1 -> 
        item >=> \v2 -> 
        item >=> \v3 -> returnP(v1,v2,v3)

par03 = par01 >=> \(v1,v2) -> 
        item >=> \v3 -> returnP(v1,v2,v3)

exseq01 = parse par01 "Hello"
 
{-
Parser escolha: aplica o primeiro parser ao string de entrada e se
este falha, então aplica o segundo.
-} 

(+++):: Parser a -> Parser a -> Parser a 
p +++ q = \s -> case parse p s of 
	             [] ->  parse q s 
	             [(v,out)] -> [(v,out)]

exch01 = parse (item +++ returnP 'd') "abc"
exch02 = parse (failure +++ returnP 'd') "abc"
exch03 = parse (failure +++ failure) "abc"

{-
Parser derivados
-}

isDigit,isLower,isUpper,isAlpha::Char -> Bool 
isDigit = \c -> elem c ['0'..'9']
isLower = \c -> elem c ['a'..'z']
isUpper = \c -> elem c ['A'..'Z']
isAlpha = \c -> (isLower c) || (isUpper c)
isAlphaNum = \c -> (isAlpha c) || (isDigit c)
isSpace = \c -> c == ' '


sat::(Char -> Bool) -> Parser Char
sat pred =  item >=> 
              \x -> if pred x then returnP x else failure

digit = sat isDigit
lower = sat isLower
upper = sat isUpper
letter = sat isAlpha
alphaNum = sat isAlphaNum
matchSpace = sat isSpace

char::Char -> Parser Char 
char x = sat (==x)

exsat01 = parse digit "123"
exsat02 = parse digit "abc"
exsat03 = parse (char 'a') "abc"


string:: String -> Parser String 
string [] = returnP [] 
string (x:xs) = char x >=> \x -> 
                string xs >=> \xs -> returnP (x:xs)

exstr01 = parse (string "alfio") "alfio martini"

{-
Repetition
-}

many, many1::Parser a -> Parser [a]
many p = many1 p +++ returnP [] 

many1 p = p >=> \v -> 
          many p >=> \vs -> returnP (v:vs)

exmany01 = parse (many digit) "123abc"
exmany02 = parse (many letter) "123abc"
      

ident::Parser String
ident = letter >=> \x ->
        many alphaNum >=> \xs -> returnP (x:xs)

nat::Parser Int
nat = many digit >=> \xs -> returnP (read xs)

space::Parser String
space = many matchSpace >=> \xs -> returnP (' ':xs)

der01 = parse space "    alfio"
der02 = parse ident "12345    alfio"
der03 = parse (nat >=> \num -> 
        space >=> \sp -> 
        ident >=> \id -> returnP (num,id)) "12345    alfio"

{-
token t ignora espaços antes e depois de t
-}

token :: Parser a -> Parser a 
token t = space >=> \_ -> 
          t >=> \v ->
          space >=> \_ -> returnP v 

identifier:: Parser String
identifier = token ident 

symbol:: String -> Parser String 
symbol xs = token (string xs)

natural::Parser Int
natural = token nat

natS::Parser String
natS = many1 digit >=> \xs -> returnP xs

parSum::Parser Int
parSum = ( (symbol "(") >=> 
        \_ -> parSum >=>
        \n1 -> (symbol "+") >=>
        \_ -> parSum >=> 
        \n2 -> (symbol ")") >=> 
        \_ -> returnP (n1+n2) ) +++ (natural >=> \n -> returnP n)
        
{-
Um parser para uma lista de naturais, onde espaços ocorrem livremente
antes e depois do construtor, dos números e das vírgulas.
-}

parNegInt::Parser Int
parNegInt = (symbol "-" >=> \_ -> natural >=> \nat -> returnP ( -nat ))

parInteger::Parser Int
parInteger = parNegInt +++ natural

parNatList::Parser [Int]
parNatList = ( (symbol "[") >=>
             \_ -> parInteger >=>
             \d1 -> (many ( symbol "," >=> \_ -> parInteger )) >=>
             \d2 -> (symbol "]") >=> \_ -> returnP (d1:d2)  )

parNatList2::Parser [Int]
parNatList2 = ( symbol "[" >=> \_ -> a >=> \ans -> symbol "]" >=> \_ -> returnP ans )

a::Parser [Int]
a = parInteger >=> \int1 -> (many ( symbol "," >=> \_ -> parInteger)) >=> \int2 -> returnP (int1:int2) +++ (returnP "" >=> \_ -> returnP [])

