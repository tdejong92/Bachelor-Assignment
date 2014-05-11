module Language where

import Prelude
import Data.List

-- Language
data HomeAutomationSystem	= System {
	agents		:: [Agent],
	devices		:: [(Device,Bool)]
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
	
data Device	= Lamp | Heater | Radio
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
  deriving (Eq,Show,Read)
	 
--type Transport	= Car | Bicycle | Foot

type Location	= (Int,Int)