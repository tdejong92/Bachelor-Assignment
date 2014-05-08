module Test where

import Prelude
import Data.List
import Debug.Trace
import Data.Maybe

-- data Agent = Agent { 
	-- idNr		:: String,
	-- plans		:: [Plan [Condition] [Action]],
	-- beliefs		:: [Proposition]
	-- }	
  -- deriving (Eq,Show,Read)

-- ha1	= Agent {
	-- idNr		= "habitantAgent1",
	-- plans		= [Plan [CounterWithin counterRange] []],
	-- beliefs		= [Counter 0, OnLocation] 
-- }
	-- where
	 -- counterRange	= [ 100*x | x<-[0..]

-- data Proposition	=	OnLocation
					-- |	InRange Agent
					-- |	Counter Int
					-- |	NewRound
					-- |	And Proposition Proposition
					-- |	Or Proposition Proposition
					-- |	Implies Proposition Proposition
					-- |	Not Proposition		
	-- deriving (Eq,Show,Read)	
	 
-- data Message	= Message Int String
	-- deriving (Eq,Show,Read)

-- agents	= ["e","a","c","b","d"]	
	
-- m1	= Message 0 "a"
-- m2	= Message 1 "a"
	
-- messages	= [[m1],[m2],[Message 2 "b", Message 3 "c"]]

-- TODO: implement	 
-- order :: [[Message]] -> [[Message]]
-- order msgs	= map (getMessageList msgsGrouped) agents
	-- where
	 -- msgsGrouped	= groupBy compareReceivers $ concat msgs
	 
-- compareReceivers msg1 msg2	= agent1==agent2
	-- where
	 -- Message prop1 agent1 	= msg1
	 -- Message prop2 agent2	= msg2
	 
-- getMessageList :: [[Message]] -> String -> [Message]
-- getMessageList [] _						= []
-- getMessageList (m:ms) agent | agent==agent'	= m
							-- | otherwise		= getMessageList ms agent
	-- where
	 -- Message prop agent'	= head m