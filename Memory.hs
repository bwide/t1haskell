module Memory 
  ( Store, 
   initial, 
   value, 
   update ) where

import DataTypes

-- Var is the type of variables.                    

type Var = Char

newtype Store = Store (Var -> Primitive)      

initial :: Store 
initial = Store (\v -> PrimitiveNumber 0)

value :: Store -> Var -> Primitive
value (Store sto) v = sto v

update  :: Store -> Var -> Primitive -> Store
update (Store sto) v n = Store (\w -> if v==w then n else sto w)