module SimulationState
       (SimulationState(..)
       , nop, inc, sum_f
       , simulate_step
       , map_outputs_to_inputs
       , get_input) where

import Monad

data Func = Func (Carrier->Carrier)
type Carrier = Maybe Integer
type Connection = Maybe Int

{- 
A simulation is a set of connected functions 
The functions map to the inputs.
The function application gives the outputs.
The connections feed the output to the inputs
-}
data SimulationState = SimulationState
     {
     funcs :: [Func],
     connections :: [Connection], -- which output feeds to input n, Nothing means unconnected
     inputs :: [Carrier],
     outputs :: [Carrier]
     }

-- feed output to inputs for all connections
feed :: SimulationState -> SimulationState
feed (SimulationState f c i o) = SimulationState f c i' o where
     i' = map_outputs_to_inputs i o c

app :: Func -> Carrier -> Carrier
app (Func f) c =  f c

apply :: SimulationState -> SimulationState
apply (SimulationState f c i o) = SimulationState f c i o' where
      o' = map (uncurry app) (zip f i)

simulate_step :: SimulationState -> SimulationState
simulate_step s = feed (apply s)

get_input inputs connections index = inputs !! (connections !! index)

map_outputs_to_inputs :: [Carrier] -> [Carrier] -> [Connection] -> [Carrier]
map_outputs_to_inputs inputs outputs connections = map selector numbered_connections
  where numbered_connections = (zip connections [0..])
        selector = (\c -> select_connected inputs outputs c)

select_connected inputs outputs (c, i) =
  case c of
       Nothing -> inputs !! i
       Just n -> outputs !! n

nop = Func id
inc = Func (liftM (1+))
sum_f = Func id

{-
Simulator process:
feed (take input, transfer output, push/pull)
apply (apply function, set output)

Structure:
Functions of dom -> cod
Node has Function and input and output ports
feeds from output to compatible input

an input that is not connected to anything just has Nothing as state
if input 0 is unconnected to anything it has connectivity Nothing.
-}