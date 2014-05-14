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
-- TODO: implement agentUpdate after retrieving has instead of agent

msgs = [[],[],[],[],[]]

(b1:bs1)	= beliefs ha1
beliefs2	= [Counter 99] ++ bs1
ha1t		= ha1{beliefs=beliefs2}
agents2		= ha1t : (tail $ agents has0)
has1		= has0{agents=agents2}

props		= [(Just 1,1),(Just 2,2),(Just 3,3)]


run :: (HomeAutomationSystem -> [[Message]] -> (HomeAutomationSystem, [[Message]])) -> HomeAutomationSystem -> [[Message]] -> [[DeviceStatus]]--[[[Message]]]
run world has input | any (==False) $ map null input 	= trace ("ROUND") (devices has') : run world has' input'
					| otherwise							= run world has' input' 
	where
	 (has',output)	= world has input
	 input'			= order has output
	 a				= head $ agents has

	 
	 
	 
	 
-- updates the world of agents by updating every agent with the corresponding incoming messages	 
world :: HomeAutomationSystem -> [[Message]] -> (HomeAutomationSystem, [[Message]])
world has input = (has',output) -- trace ("WORLD: " ++ show output ++ " ENDWORLD")
	where
	 --(agents',output)
	 (agents',output) = unzip $ map (agentUpdate has) $ zip (updateCounter (agents has)) input	-- combines the result of updating every agent with the new messages
	 has'				= has{agents=agents'}												-- updates the system
	 
	 
	 
	 
	 
	 
	 
-- auxiliary function that updates each agent's counter every time the world-function is executed	 
updateCounter :: [Agent] -> [Agent]
updateCounter []			= []
updateCounter [a]			= [a{beliefs=beliefs'}]
	where
	 ((Counter x):bs)		= beliefs a
	 beliefs'				= ((Counter (x+1)):bs)
updateCounter (a:agents)	= (a{beliefs=beliefs'}:updateCounter agents)
	where
	 ((Counter x):bs)		= beliefs a
	 beliefs'				= ((Counter (x+1)):bs)
	 
-- updating a specific agent with its incoming messages	by determining its plan and performing the corresponding actions
agentUpdate :: HomeAutomationSystem -> (Agent,[Message]) -> (Agent,[Message])
agentUpdate has (agent,[])		= (agent',output) --trace ("AGENTUPDATE: " ++ show (idNr agent',output)) 
	where
	 (agent',output)		= performActions has agent $ getActions agent
agentUpdate has (agent,[m])		= (agent'',output) -- trace ("AGENTUPDATE: " ++ show (idNr agent'',output)) 
	where
	 Message prop a			= m
	 beliefs'				= addBelief (beliefs agent) prop
	 agent'					= agent{beliefs=beliefs'}
	 (agent'',output)		= performActions has agent' $ getActions agent'
agentUpdate has (agent,(m:msgs))= (agent'',output++output') -- trace ("AGENTUPDATE: " ++ show (idNr agent'',output++output')) 
	where
	 Message prop a			= m
	 beliefs'				= addBelief (beliefs agent) prop
	 agent'					= agent{beliefs=beliefs'}
	 (agentTemp,output)		= performActions has agent' $ getActions agent'
	 Message p a2			= head msgs
	 (agent'',output') 		= agentUpdate has (agentTemp,msgs)		-- updates the agent in the next message

--	 
performActions :: HomeAutomationSystem -> Agent -> [Action] -> (Agent,[Message])
performActions has agent []									= (agent,[]) -- trace ("actions: " ++ show (idNr agent) ++ "  " ++ []) 
performActions has agent [TurnOn d]							= (agent',[]) -- trace ("actions: " ++ show (idNr agent) ++ "  " ++ show [TurnOn d])
	where
	 beliefs' 											= trace ("DEVICE TURNED ON!" ++ show d) addBelief (beliefs agent) (On d)
	 agent' 											= agent{beliefs=beliefs'}
	 agents'											= update (agents has) agent'
	 has'												= has {agents=agents'}
	 devices'											= turn (devices has') d True
	 has''												= update (devices has) (DeviceStatus d False)
performActions has agent [TurnOff d]						= (agent',[]) -- trace ("actions: " ++ show (idNr agent) ++ "  " ++ show [TurnOn d])
	where
	 beliefs' 											= trace ("DEVICE TURNED OFF!" ++ show d) addBelief (beliefs agent) (Not (On d))
	 agent' 											= agent{beliefs=beliefs'}
performActions has agent [Send recAgent]					| isJust $ fst $ retrieve OnLocationCond agent	= (agentl',[Message propl recAgent]) -- trace ("actions: " ++ show (idNr agent) ++ "  Send")
														| isJust $ fst $ retrieve InRangeCond agent		= (agentr',[Message propr recAgent])
														| otherwise									= error "No information about proposition found!"
	where
	 (Just propl,agentl')	= retrieve OnLocationCond agent
	 (Just propr,agentr')	= retrieve InRangeCond agent
performActions has agent [DetermineRange]					| inRange agent a 	= (agent'',[])		-- trace ("actions: " ++ show agent ++ "  " ++ show [DetermineRange]) Reevaluate plans
														| otherwise			= (agent,[])
	where
	 (Just (OnLocation a1),agent')						= retrieve OnLocationCond agent
	 a													= getAgent has a1
	 beliefs' 											= addBelief (beliefs agent') (InRange a1)
	 agent''											= agent'{beliefs=beliefs'}
performActions has agent (act:acts)							= (agent'',output') -- trace ("actions: " ++ show (idNr agent) ++ "multiple")
	where
	 (agent',output)									= performActions has agent [act]
	 (agent'',output')									= performActions has agent' acts
	 
getActions :: Agent -> [Action]
getActions agent = getActionsAux (plans agent) (beliefs agent)
	 
getActionsAux :: [Plan] -> [Proposition] -> [Action]
getActionsAux [] _				= []
getActionsAux (p:ps) beliefs	| implies beliefs conditions	= actions ++ (getActionsAux ps beliefs)
								| otherwise						= getActionsAux ps beliefs
	where
	 Plan conditions actions	= p 

implies :: [Proposition] -> [Condition] -> Bool
implies beliefs cs	= all isJust props
	where
	 props = map fst $ map (retrieveAux beliefs) cs
	 
-- retrieves the proposition that has caused a condition of the agent to evaluate as true, or Nothing.
	-- if isJust: condition evaluates to true
retrieve :: Condition -> Agent -> (Maybe Proposition,Agent)
retrieve condition agent	| (idNr agent == "habitantAgent1" || idNr agent == "habitantAgent2") && condition == OnLocationCond	= (Just prop,agent{beliefs=beliefs''})
							| isJust $ fst $ retrieveAux (beliefs agent) condition = (Just prop,agent{beliefs=beliefs'})
							| otherwise																							= (Nothing,agent)
	where
	 (Just prop, beliefs')	= retrieveAux (beliefs agent) condition  -- trace ("pattern: " ++ show (idNr agent) ++ "  " ++ show condition ++ show (beliefs agent)) 
	 beliefs''				= (head beliefs' : prop : tail beliefs')
	 
retrieveAux :: [Proposition] -> Condition -> (Maybe Proposition,[Proposition])
retrieveAux [b] OnLocationCond		= case b of
										(OnLocation x)	-> (Just b,[])
										otherwise		-> (Nothing,[])
retrieveAux (b:bs) OnLocationCond	= case b of
										(OnLocation x) 	-> (Just b,bs)
										otherwise		-> (bRes,(b:bsRes))
	where
	 (bRes,bsRes)	= retrieveAux bs OnLocationCond
retrieveAux [b] InRangeCond			= case b of
										(InRange x)		-> (Just b,[])
										otherwise		-> (Nothing,[])
retrieveAux (b:bs) InRangeCond 		= case b of
										(InRange x)		-> (Just b,bs)
										otherwise		-> (bRes,(b:bsRes))
	where
	 (bRes,bsRes)	= retrieveAux bs InRangeCond
retrieveAux [b] (Cond (InRange agent))		| b == InRange agent	= (Just b,[])
									| otherwise				= (Nothing,[])
retrieveAux (b:bs) (Cond (InRange agent)) 	| b == InRange agent 	= (Just b,bs)
									| otherwise				= (bRes,(b:bsRes))
	where
	 (bRes,bsRes)	= retrieveAux bs (Cond (InRange agent))
retrieveAux (b:bs) (CounterCond i)	| x `mod` i == 0	= (Just (Counter x),bs)
									| otherwise			= (Nothing,bs)
	where
	 Counter x						= b
retrieveAux [b] (OnCond device)		| b == (On device)		= (Just b,[])
									| otherwise						= (Nothing,[])
retrieveAux (b:bs) (OnCond device)	| b == (On device)		= (Just b,bs)
									| otherwise						= (bRes,(b:bsRes))
	where
	 (bRes,bsRes)	= retrieveAux bs (OnCond device)
retrieveAux [b] (NotC (OnCond d))		| b == Not (On d)				= (Just b,[])
										| otherwise						= (Nothing,[])
retrieveAux (b:bs) (NotC (OnCond d))	| b == Not (On d)				= (Just b,bs)
										| otherwise						= (bRes,(b:bsRes))
	where
	 (bRes,bsRes)	= retrieveAux bs (NotC (OnCond d))

addBelief	:: [Proposition] -> Proposition -> [Proposition]
addBelief [] newProp		= [newProp]
addBelief (p:props) newProp | p == Not newProp || Not p == newProp	= newProp : (addBelief props newProp)
							| otherwise	= p : (addBelief props newProp)


-- ORDER BEGIN							
order :: HomeAutomationSystem -> [[Message]] -> [[Message]]
order has msgs	= map (getMessageList has msgsGrouped) (agents has)
	where
	 msgsGrouped	= groupBy compareReceivers $ concat msgs
	 
compareReceivers msg1 msg2	= agent1==agent2
	where
	 Message prop1 agent1 	= msg1
	 Message prop2 agent2	= msg2
	 
getMessageList :: HomeAutomationSystem -> [[Message]] -> Agent -> [Message]
getMessageList _ [] _							= []
getMessageList has (m:ms) agent | agent==agent'	= m
							| otherwise		= getMessageList has ms agent
	where
	 Message prop agentString				= head m
	 agent'									= getAgent has agentString
-- ORDER END	 

update :: Eq a => [a] -> a -> [a]
update (x:xs) y	| x==y		= (y:xs)
				| otherwise	= (x:update xs y)

	 
-- TODO: implement
inRange :: Agent -> Agent -> Bool
inRange agent1 agent2			= (sqrt $ fromIntegral ((x1-x2)^2 + (y1-y2)^2)) < fromIntegral range
	where
	 (x1,y1)					= location agent1
	 (x2,y2)					= location agent2
	 
getAgent :: HomeAutomationSystem -> String -> Agent
getAgent has agentName = head [ agent | agent <- (agents has), (idNr agent) == agentName ]

 
turn :: [DeviceStatus] -> Device -> Bool -> [DeviceStatus]
turn (ds:dss) device b	| x == device	= db : dss
						| otherwise		= ds : turn dss device b
	where
	 DeviceStatus x y	= ds
	 db				= DeviceStatus x b

range :: Int
range = 10