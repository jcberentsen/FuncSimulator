import Test.HUnit
import Monad

-- Production code below
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
map_outputs_to_inputs inputs outputs connections = map (\c -> select_connected inputs outputs c) (zip connections [1..])

select_connected inputs outputs (c, i) =
  case c of
       Nothing -> inputs !! 0 -- ?
       Just n -> outputs !! n
 
nop = Func id
inc = Func (liftM (1+))
sum_f = Func id

---- Test Harness below
nop_sim = SimulationState [nop] [] [Just 1] []
nop_inc_sim = SimulationState [nop, inc] [Just 0, Just 1] [Nothing, Just 1] []
nop_nop_swap_sim = SimulationState [nop, nop] [Just 1, Just 0] [Just 1, Just 2] []

---- Tests below

test_feed = "When feeding 1 to input then output Just 1" ~:
  [Just 1] ~=? outputs (simulate_step nop_sim)

test_feed_inc = "Given nop_inc simulation simulating twice must output [Nothing, Just 3]" ~:
  [Nothing, Just 3] ~=? outputs (simulate_step (simulate_step nop_inc_sim))

test_swap = "Given nop_nop_swap, when stepping twice on input [Just 1, Just 2] then must output [Just 2, Just 1]" ~:
  [Just 2, Just 1] ~=? outputs (simulate_step (simulate_step nop_nop_swap_sim))

-- i is input o is output
--   i    oi   o
--   0    01   1
--   -> 0 -> 1 ->
test_feeding = "Given input (Just 1, Nothing) and both outputs feed from slot 0 then the input 1 yields Just 1" ~:
  Just 1 ~=? (get_input [Just 1, Nothing] [0, 0] 1)

test_that_output_feeds_by_connections_to_input = "Outputs feed connectivity to inputs" ~:
  [Nothing, Just 1, Just 2] ~=? (map_outputs_to_inputs [] [Just 1, Nothing, Just 2] [Just 1, Just 0, Just 2])

test_that_unconnected_input_reuses_input_and_ignores_output = "Unconnected input stays in old state" ~:
  [Just 1] ~=? (map_outputs_to_inputs [Just 1] [Just 2] [Nothing])

test_that_an_unconnected_inc_element_given_1_yields_2 = "Given an unconnected inc, when feeding in Just 1, the resulting output becomes Just 2" ~:
  [Just 2] ~=? outputs (simulate_step single_inc)
    where single_inc = SimulationState [inc] [Nothing] [Just 1] []

test_sum_of_inputs = "Given a sum element, when given 1 and 2, output 3" ~:
  [Just 3] ~=? outputs (simulate_step sigma)
    where sigma = SimulationState [sum_f] [Nothing, Nothing] [Just 1, Just 2] []

tests = TestList [ test_feed    
                 , test_feed_inc
                 , test_swap
                 , test_feeding
                 , test_that_output_feeds_by_connections_to_input
                 , test_that_unconnected_input_reuses_input_and_ignores_output
                 , test_that_an_unconnected_inc_element_given_1_yields_2
                 , test_sum_of_inputs
                 ]

main = runTestTT tests

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