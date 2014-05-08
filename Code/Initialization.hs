module Initialization where

import Prelude
import Data.List
import Language
import Data.Maybe

-- @TODO: determine closest agent whenever location is determined

-- System
has = System { 
	agents 	= [ha1, ha2, sa1, agent1,agent2]
}

-- Agents
-- Agent that sends location every 100 t
ha1	= Agent {
	idNr		= "habitantAgent1",
	plans		= [Plan [CounterCond [ 100*x | x<-[0..] ]] 	[Send sa1],
				   Plan [NewRoundCond] 							[UpdateCounter]],
	beliefs		= [Counter 0, OnLocation ha1 $ location ha1],
	location	= (0,0)
}
	where
	 
ha2	= Agent {
	idNr		= "habitantAgent2",
	plans		= [Plan [CounterCond [ 100*x | x<-[0..] ]] 	[Send sa1],
				   Plan [NewRoundCond] 						[UpdateCounter]],
	beliefs		= [Counter 0, OnLocation ha2 $ location ha2],
	location	= (1,1)
}			
						
sa1	= Agent {
	idNr		= "sensorAgent1",
	plans		= [Plan [OnLocationCond] 					[DetermineRange],
				   Plan [InRangeCond] 						[Send agent1]],
	beliefs		= [Counter 0],
	location	= (2,2)
}										

agent1	= Agent {
	idNr		= "homeAgent1",
	plans		= [Plan [OnLocationCond] 					[Send agent2]],
	beliefs		= [Counter 0],
	location	= (3,3)
}

agent2	= Agent {
	idNr		= "homeAgent2",
	plans		= [Plan [OnLocationCond] [TurnOn Lamp]],
	beliefs		= [Counter 0, Not (On Lamp), Not (On Heater), Not (On Radio)],
	location	= (4,4)
}