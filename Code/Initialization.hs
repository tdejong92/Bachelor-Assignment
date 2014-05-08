module Initialization where

import Prelude
import Data.List
import Language
import Data.Maybe

-- @TODO: determine closest agent whenever location is determined

-- System
has = System { 
	agents 	= [ha1]--, ha2, sa1, agent1,agent2]
}

-- Agents
-- Agent that sends location every 100 t
ha1	= Agent {
	idNr		= "habitantAgent1",
	plans		= ha1Plans,
	beliefs		= [Counter 0, OnLocation ha1 (0,0)] 
}
ha1Plans :: [Action]
ha1Plans	| x `mod` 100 == 0				= [Send (Message location sa1)]	-- send message with location of ha1 to sa1
			| elem NewRound $ beliefs ha1	= [UpdateCounter]
			| otherwise						= []
	where
	 Counter x								= beliefs ha1 !! 0
	 location								= beliefs ha1 !! 1
	 
ha2	= Agent {
	idNr		= "habitantAgent2",
	plans		= ha2Plans,
	beliefs		= [Counter 0, OnLocation ha2 (1,1)]
}
ha2Plans :: [Action]
ha2Plans	| x `mod` 100 == 0				= [Send (Message location sa1)]	-- send message with location of ha1 to sa1
			| elem NewRound $ beliefs ha1	= [UpdateCounter]
			| otherwise						= []
	where
	 Counter x								= beliefs ha2 !! 0
	 location								= beliefs ha2 !! 1				
						
sa1	= Agent {
	idNr		= "sensorAgent1",
	plans		= sa1Plans,
	beliefs		= [Counter 0, OnLocation sa1 (1,1)]
}
sa1Plans :: [Action]
sa1Plans	| isJust $ contains (beliefs sa1) "OnLocation"	= [DetermineRange a1 l1]						-- determine the range of the agent on that location
			| isJust $ contains (beliefs sa1) "InRange"		= [Send (Message l2 agent1)]	-- send a message to another agent if a habitant is within the specified range
			| otherwise										= []
	where
	 Just (OnLocation a1 l1)	= contains (beliefs sa1) "OnLocation"
	 Just (InRange a2)			= contains (beliefs sa1) "InRange"
	 l2							= beliefs a2 !! 1											

agent1	= Agent {
	idNr	= "homeAgent1",
	plans	= agent1Plans,
	beliefs	= [Counter 0, OnLocation agent1 (1,1)]
}
agent1Plans :: [Action]
agent1Plans	| isJust $ contains (beliefs agent1) "OnLocation"	= [Send (Message l2 agent2)]
			| otherwise											= []
	where
	 Just (OnLocation a1 l1)	= contains (beliefs agent1) "OnLocation"
	 l2							= beliefs a1 !! 1
	 
agent2	= Agent {
	idNr	= "homeAgent2",
	plans	= agent2Plans,
	beliefs	= [Counter 0, OnLocation agent2 (1,1), Not (On Lamp), Not (On Heater), Not (On Radio)]
}
agent2Plans :: [Action]
agent2Plans	| (isJust $ contains (beliefs agent2) "OnLocation") && (elem (Not (On Lamp)) (beliefs agent2)) 	= [TurnOn Lamp]
			| otherwise 																							= []

contains :: [Proposition] -> String -> Maybe Proposition
contains [] _					= Nothing
contains (b:bs) "OnLocation"	= case b of
									OnLocation a l	-> Just b
									otherwise		-> contains bs "OnLocation"
contains (b:bs) "InRange"		= case b of
									InRange a		-> Just b
									otherwise		-> contains bs "InRange"
contains _ _					= Nothing