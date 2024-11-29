module Lib
( multiplyMod
) where
  
multiplyMod::Int->Int->Int->Maybe Int
multiplyMod _ _ 0 = Nothing
multiplyMod a b m = Just( mod (mod a m *mod b m) m)

