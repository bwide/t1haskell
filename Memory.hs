module Memory 
  ( Store, 
   initial, 
   value, 
   update ) where

import Prelude
import Primitives

type Var = Value

newtype Store = Store (Var -> Integer)      

initial :: Store 
initial = Store (\v -> 0)

value :: Store -> Var -> Integer
value (Store sto) v = sto v

update  :: Store -> Var -> Integer -> Store
update (Store sto) v n = Store (\w -> if v==w then n else sto w)