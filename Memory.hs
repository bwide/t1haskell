module Memory
( Store, 
  initial,     -- Store
  value,       -- Store -> Var -> Integer
  update       -- Store -> Var -> Integer -> Store
 ) where

import Primitives

type Var = Value

-- The implementation is given by a newtype declaration, with one
-- constructor, taking an argument of type [ (String,Var) ].

data Store = Store [ (String, Var) ] deriving (Show, Eq)

-- instance Eq Store where 
-- (Store sto1) == (Store sto2) = (sto1 == sto2)                 

-- instance Show Store where
-- show (Store sto) = show sto                 
--  
initial :: Store
initial = Store []

value  :: Store -> String -> Var
value (Store []) v         = Null
value (Store ((string, var):sto)) v 
  | v==string            = var
  | otherwise       = value (Store sto) v

update  :: Store -> Var -> String -> Store
update (Store sto) v n = Store ((n,v):sto)