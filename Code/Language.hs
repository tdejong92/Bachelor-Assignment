module Language where

import Prelude
import Data.List

-- Language
data HomeAutomationSystem	= System {
	agents		:: [Agent],
	devices		:: [DeviceStatus]
}
	deriving Show

data Proposition	=	OnLocation String
					|	InRange String
					|	On Device
					|	Counter Int
				--	|	And Proposition Proposition
				--	|	Or Proposition Proposition
				--	|	Implies Proposition Proposition
					|	Not Proposition		
	deriving (Eq,Show,Read)
	
data Condition		= OnLocationCond
					| InRangeCond
					| CounterCond Int
					| OnCond Device
					| NotC Condition
					| Cond Proposition
	deriving (Eq,Show,Read)
	
data DeviceStatus	= DeviceStatus Device Bool
	deriving (Show,Read)
instance Eq DeviceStatus where
	DeviceStatus d1 s1 == DeviceStatus d2 s2 = d1 == d2
	
data Device = Lamp | Heater | Radio	
	deriving (Eq,Show,Read)
	
data Action	= TurnOn Device
			| TurnOff Device
			| Send String
			| DetermineRange
	deriving (Eq,Show,Read)
	
data Plan	= Plan [Condition] [Action]	
	deriving (Eq,Show,Read)
				
data Message	= Message Proposition String
	deriving (Eq,Show,Read)
				
data Agent = Agent { 
	idNr		:: String,
	plans		:: [Plan],
	beliefs		:: [Proposition],
	location	:: Location
	}	
  deriving (Show,Read)
instance Eq Agent where
	Agent idNr1 plans1 beliefs1 location1 == Agent idNr2 plans2 beliefs2 location2 = idNr1 == idNr2
	 
--type Transport	= Car | Bicycle | Foot

type Location	= (Int,Int)