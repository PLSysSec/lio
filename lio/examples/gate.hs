import LIO
import LIO.DCLabel

import LIO.TCB


-- | Add two numbers if the computation is invoked by Alice or Bob.
addGate :: DCGate (Int -> Int -> Maybe Int)
addGate = gate $ \pd a b ->
  if pd `elem` (map dcPrivDesc ["Alice", "Bob"])
    then Just $ a + b
    else Nothing


alice, bob, clark :: DCPriv
alice = PrivTCB . dcPrivDesc $ "Alice"
bob   = PrivTCB . dcPrivDesc $ "Bob"
clark = PrivTCB . dcPrivDesc $ "Clark"

main = putStrLn . show $ 
  [ callGate addGate alice 1 2 -- Just 3
  , callGate addGate bob   3 4 -- Just 7
  , callGate addGate clark 5 6 -- Nothing
  ]
