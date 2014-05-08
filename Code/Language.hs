module Language where

import Prelude
import Data.List

-- Language
data HomeAutomationSystem	= System {
	agents		:: [Agent]
}
	deriving Show

data Proposition	=	OnLocation Agent Location
					|	InRange Agent
					|	On Device
					|	Counter Int
					|	NewRound
				--	|	And Proposition Proposition
				--	|	Or Proposition Proposition
				--	|	Implies Proposition Proposition
					|	Not Proposition		
	deriving (Eq,Show,Read)
	
-- data Condition		= CounterWithin [Int]	
					-- | OnLocation
					-- | InRange
					-- | Proposition
	
data Device	= Lamp | Heater | Radio
	deriving (Eq,Show,Read)
	
data Action	= TurnOn Device
			| Send Message
			| DetermineRange Agent	Location
			| UpdateCounter
	deriving (Eq,Show,Read)
	
data Plan	= Plan Bool [Action]	
	deriving (Eq,Show,Read)
				
data Message	= Message Proposition Agent
	deriving (Eq,Show,Read)
				
data Agent = Agent { 
	idNr		:: String,
	plans		:: [Action],
	beliefs		:: [Proposition]
	--range		:: Int
	--transport	:: Transport
	}	
  deriving (Eq,Show,Read)
	 
--type Transport	= Car | Bicycle | Foot

type Location	= (Int,Int)