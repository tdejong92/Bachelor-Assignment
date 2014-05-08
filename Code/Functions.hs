module Functions where

import Prelude
import Data.List
import Language
import Initialization
import Debug.Trace
import Data.Maybe

-- TODO: check implementation of addBelief
-- TODO: remove location from OnLocation
-- TODO: add devices to system

msgs = [[],[],[]]

--run :: (HomeAutomationSystem -> [[Message]] -> (HomeAutomationSystem, [[Message]])) -> HomeAutomationSystem -> [[Message]] -> [[[Message]]]
run world has input | any (==False) $ map null input  	= (map idNr $ agents has) : run world has' input'
					| otherwise							= run world has' input' 
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
updateCounter (mList:msList)	| null mList == False	= (mList':updateCounter msList)
								| otherwise				= 
	where
	 (m:ms)				= mList
	 Message prop agent	= m
	 mList'				= ((Message NewRound agent):mList)
	 
-- updating a specific agent with its incoming messages	by determining its plan and performing the corresponding actions
agentUpdate :: [Message] -> (Agent,[Message])
agentUpdate [m]			= (agent'',output)
	where
	 Message prop agent	= m
	 beliefs'			= addBelief (beliefs agent) prop
	 agent'				= agent{beliefs=beliefs'}
	 (agent'',output)	= performActions agent' $ getActions agent'
agentUpdate (m:msgs) 	= (agent'',output++output')
	where
	 Message prop agent	= m
	 beliefs'			= addBelief (beliefs agent) prop
	 agent'				= agent{beliefs=beliefs'}
	 (agentTemp,output)	= performActions agent' $ getActions agent'
	 Message p a		= head msgs
	 (agent'',output') 	= agentUpdate ((Message p agentTemp):tail msgs)		-- updates the agent in the next message

--	 
performActions :: Agent -> [Action] -> (Agent,[Message])
performActions agent []									= (agent,[])
performActions agent [UpdateCounter]					= performActions agent' $ getActions agent'
	where
	 ((Counter x):beliefs')								= beliefs agent
	 beliefs''											= [ x | x <- ((Counter (x+1)):beliefs'), x /= NewRound ]
	 agent'												= agent{beliefs=beliefs''}
performActions agent [TurnOn d]							= (agent',[])
	where
	 beliefs' 											= addBelief (removeLocationBeliefs $ beliefs agent) (On d)
	 agent' 											= agent{beliefs=beliefs'}
performActions agent [Send recAgent]					= (agent',[Message prop recAgent])
	where
	 (prop,agent')	= retrieve (OnLocationCond) agent
performActions agent [DetermineRange]					| inRange agent a 	= (agent'',[])		-- Reevaluate plans
														| otherwise			= (agent,[])
	where
	 ((OnLocation a l),agent')							= retrieve OnLocationCond agent
	 beliefs' 											= addBelief (beliefs agent') (InRange a)
	 agent''											= agent'{beliefs=beliefs'}
performActions agent (act:acts)							= (agent'',output')
	where
	 (agent',output)									= performActions agent [act]
	 (agent'',output')									= performActions agent' acts
	 
getActions :: Agent -> [Action]
getActions agent = getActionsAux (plans agent) (beliefs agent)
	 
getActionsAux :: [Plan] -> [Proposition] -> [Action]
getActionsAux [] _				= []
getActionsAux (p:ps) beliefs	| implies beliefs conditions	= actions ++ (getActionsAux ps beliefs)
								| otherwise						= getActionsAux ps beliefs
	where
	 Plan conditions actions	= p 

implies :: [Proposition] -> [Condition] -> Bool
implies beliefs cs	= all isJust $ map (retrieveAux beliefs) cs
	 
-- retrieves the proposition that has caused a condition of the agent to evaluate as true, or Nothing.
	-- if isJust: condition evaluates to true
retrieve :: Condition -> Agent -> (Proposition,Agent)
retrieve condition agent	= (prop,agent{beliefs=beliefs'})
	where
	 Just (prop, beliefs')	= retrieveAux (beliefs agent) condition
	 
retrieveAux :: [Proposition] -> Condition -> Maybe (Proposition,[Proposition])
retrieveAux [b] OnLocationCond		= case b of
										(OnLocation x y)	-> Just (b,[])
										otherwise			-> Nothing
retrieveAux (b:bs) OnLocationCond	= case b of
										(OnLocation x y) 	-> Just (b,bs)
										otherwise			-> Just (bRes,(b:bsRes))
	where
	 Just (bRes,bsRes)	= retrieveAux bs OnLocationCond
retrieveAux [b] InRangeCond			= case b of
										(InRange x)		-> Just (b,[])
										otherwise		-> Nothing
retrieveAux (b:bs) InRangeCond 		= case b of
										(InRange x)		-> Just (b,bs)
										otherwise		-> Just (bRes,(b:bsRes))
	where
	 Just (bRes,bsRes)	= retrieveAux bs InRangeCond

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