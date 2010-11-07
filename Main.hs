module Main (main) where

import Test.HUnit
import Monad
import SimulationState

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

test_that_unconnected_inputs_reuses_input_and_ignores_output = "Unconnected inputs stays in old state" ~:
  [Just 1, Just 2] ~=? (map_outputs_to_inputs [Just 1, Just 2] [Nothing, Nothing] [Nothing, Nothing])

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
                 , test_that_unconnected_inputs_reuses_input_and_ignores_output
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