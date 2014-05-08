module Functions where

import Prelude
import Data.List
import Language
import Initialization
import Debug.Trace

-- TODO: check implementation of addBelief

--run :: (HomeAutomationSystem -> [[Message]] -> (HomeAutomationSystem, [[Message]])) -> HomeAutomationSystem -> [[Message]] -> [[[Message]]]
run world has input = (map idNr $ agents has) : run world has' input'
	where
	 (has',output)	= world has input
	 input'			= order output

-- updates the world of agents by updating every agent with the corresponding incoming messages	 
world :: HomeAutomationSystem -> [[Message]] -> (HomeAutomationSystem, [[Message]])
world has input = (has',output)
	where
	 input'				= updateCounter input				-- adds a NewRound message for each agent in order to update their counter
	 (agents',output) 	= unzip $ map agentUpdate input'	-- combines the result of updating every agent with the new messages
	 has'				= has{agents=agents'}				-- updates the system
	 
-- auxiliary function that updates each agent's counter every time the world-function is executed	 
updateCounter :: [[Message]] -> [[Message]]
updateCounter []				= []
updateCounter (mList:msList)	= (mList':updateCounter msList)
	where
	 ((Message prop agent):ms)	= mList
	 mList'						= ((Message NewRound agent):mList)
	 
-- updating a specific agent with its incoming messages	by determining its plan and performing the corresponding actions
agentUpdate :: [Message] -> (Agent,[Message])
agentUpdate [m]			= (agent'',output)
	where
	 Message prop agent	= m
	 beliefs'			= addBelief (beliefs agent) prop
	 agent'				= agent{beliefs=beliefs'}
	 actions			= plans agent'
	 (agent'',output)	= performActions agent' actions
agentUpdate (m:msgs) 	= (agent'',output++output')
	where
	 Message prop agent	= m
	 beliefs'			= addBelief (beliefs agent) prop
	 agent'				= agent{beliefs=beliefs'}
	 actions			= plans agent'
	 (agentTemp,output)	= performActions agent' actions
	 Message p a		= head msgs
	 (agent'',output') 	= agentUpdate ((Message p agentTemp):tail msgs)		-- updates the agent in the next message

--	 
performActions :: Agent -> [Action] -> (Agent,[Message])
performActions agent []									= (agent,[])
performActions agent [UpdateCounter]					= performActions agent' $ plans agent'
	where
	 ((Counter x):beliefs')								= beliefs agent
	 beliefs''											= [ x | x <- ((Counter (x+1)):beliefs'), x /= NewRound ]
	 agent'												= agent{beliefs=beliefs''}
performActions agent [TurnOn d]							= (agent',[])
	where
	 beliefs' 											= addBelief (removeLocationBeliefs $ beliefs agent) (On d)
	 agent' 											= agent{beliefs=beliefs'}
performActions agent [Send (Message prop recAgent)]		= (agent,[Message prop recAgent])
performActions agent [DetermineRange agent1 location]	| inRange agent agent1 	= performActions agent' $ plans agent'
														| otherwise	= (agent,[])
	where
	 beliefs' 											= addBelief (beliefs agent) (InRange agent1)
	 agent' 											= agent{beliefs=beliefs'}
performActions agent (act:acts)							= (agent'',output')
	where
	 (agent',output)									= performActions agent [act]
	 (agent'',output')									= performActions agent' acts

addBelief	:: [Proposition] -> Proposition -> [Proposition]
addBelief [] newProp		= [newProp]
addBelief (p:props) newProp | p == Not newProp || Not p == newProp	= newProp : (addBelief props newProp)
							| otherwise	= p : (addBelief props newProp)

removeLocationBeliefs :: [Proposition] -> [Proposition]
removeLocationBeliefs []		= []
removeLocationBeliefs (b:bs)	= case b of
									OnLocation agent loc 	-> removeLocationBeliefs bs
									otherwise 				-> (b:(removeLocationBeliefs bs))
	 
order :: [[Message]] -> [[Message]]
order msgs	= map (getMessageList msgsGrouped) (agents has)
	where
	 msgsGrouped	= groupBy compareReceivers $ concat msgs
	 
compareReceivers msg1 msg2	= agent1==agent2
	where
	 Message prop1 agent1 	= msg1
	 Message prop2 agent2	= msg2
	 
getMessageList :: [[Message]] -> Agent -> [Message]
getMessageList [] _							= []
getMessageList (m:ms) agent | agent==agent'	= m
							| otherwise		= getMessageList ms agent
	where
	 Message prop agent'					= head m
	 
	 
-- TODO: implement
inRange :: Agent -> Agent -> Bool
inRange agent1 agent2			= (sqrt $ fromIntegral ((x1-x2)^2 + (y1-y2)^2)) < fromIntegral range
	where
	 (b1a:b1b:b1s)				= beliefs agent1
	 OnLocation agent1 (x1,y1)	= b1b
	 (b2a:b2b:b2s)				= beliefs agent2
	 OnLocation agent2 (x2,y2)	= b2b
	 
range :: Int
range = 10