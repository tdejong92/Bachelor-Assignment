module Initialization where

import Prelude
import Data.List
import Language
import Data.Maybe
import Debug.Trace

-- @TODO: determine closest agent whenever location is determined

-- System
has0 = System { 
	agents 	= [ha1, ha2, sa1, agent1,agent2],
	devices	= [(Lamp,False),(Heater,False),(Radio,False)]
}

-- Agents
-- Agent that sends location every 100 t
ha1	= Agent {
	idNr		= "habitantAgent1",
	plans		= [Plan [CounterCond 10000] 	[Send "sensorAgent1"]],
				   --Plan [NewRoundCond] 							[UpdateCounter]],
	beliefs		= [Counter 0, OnLocation "habitantAgent1"],
	location	= (0,0)
}
	where
	 
ha2	= Agent {
	idNr		= "habitantAgent2",
	plans		= [Plan [CounterCond 10000] 	[Send "sensorAgent1"]],
				   --Plan [NewRoundCond] 						[UpdateCounter]],
	beliefs		= [Counter 0, OnLocation "habitantAgent2"],
	location	= (1,1)
}			
						
sa1	= Agent {
	idNr		= "sensorAgent1",
	plans		= [Plan [OnLocationCond] 					[DetermineRange],
				   Plan [InRangeCond] 						[Send "homeAgent1"]],
	beliefs		= [Counter 0],
	location	= (2,2)
}										

agent1	= Agent {
	idNr		= "homeAgent1",
	plans		= [Plan [InRangeCond] 					[Send "homeAgent2"]],
	beliefs		= [Counter 0],
	location	= (3,3)
}

agent2	= Agent {
	idNr		= "homeAgent2",
	plans		= [Plan [Cond (InRange "habitantAgent1"),NotC (OnCond Lamp)] [TurnOn Lamp],
					Plan [Cond (InRange "habitantAgent2"),NotC (OnCond Heater)] [TurnOn Heater],
					Plan [CounterCond 50000, OnCond Lamp] [TurnOff Lamp],
					Plan [CounterCond 50000, OnCond Heater] [TurnOff Heater],
					Plan [CounterCond 50000, OnCond Radio] [TurnOff Radio]],
	beliefs		= [Counter 0, (On Lamp), Not (On Heater), Not (On Radio)],
	location	= (4,4)
}