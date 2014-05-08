run :: (HomeAutomationSystem -> [[Message]] -> (HomeAutomationSystem, [[Message]])) -> HomeAutomationSystem -> [[Message]] -> [[[Message]]]
run world has input = output : run world has' input'
	where
	 (has',output)	= world has input
	 input'			= order output

-- updates the world of agents by updating every agent with the corresponding incoming messages	 
-- updates the counter for every agent
world :: HomeAutomationSystem -> [[Message]] -> (HomeAutomationSystem, [[Message]])
world has input = (has',output)
	where
	 (agents',output) 	= unzip $ map agentUpdate input
	 has'				= has{agents=agents'}
	 
-- updating a specific agent with its incoming messages	by determining its plan and performing the corresponding actions
agentUpdate :: [Message] -> (Agent,[Message])
agentUpdate (m:msgs) = (agent',output)
	where
	 Message prop agent	= m
	 beliefs'			= addBelief (beliefs agent) prop
	 agent'				= agent{beliefs=beliefs'}
	 actions			= achievePlans agent'
	 (agent'',output)	= performActions agent' actions

--	 
performActions :: Agent -> [Action] -> (Agent,[Message])
performActions agent []								= trace ("no action") (agent,[])
performActions agent [TurnOn d]						= trace ("turnon action") (agent',[]) -- Geen bericht versturen na aanzetten apparaat?
	where
	 beliefs' = addBelief (beliefs agent) (On d)
	 agent' = agent{beliefs=beliefs'}
performActions agent [Send (Message prop recAgent)]	= trace ("message action: " ++ show prop) (agent,[Message prop recAgent])
performActions agent [DetermineRange agent1 location]	| inRange agent agent1 	= performActions agent' (achievePlans agent')
														| otherwise	= (agent,[])
	where
	 beliefs' 	= addBelief (beliefs agent) (InRange agent1)
	 agent' 	= agent{beliefs=beliefs'}
performActions agent (act:acts)						= (agent'',output')
	where
	 (agent',output)	= performActions agent [act]
	 (agent'',output')	= performActions agent' acts

-- DETERMINE ACTIONS	  
-- Determine for which plans the conditions are met by the agent's beliefs and perform the actions necessary for reaching those plans
achievePlans	:: Agent -> [Action]
achievePlans agent@Agent{plans=[]}	= []
achievePlans agent	| implies (beliefs agent) c	= a ++ (achievePlans agent{plans=gs})
					| otherwise					= achievePlans agent{plans=gs}
	where
	 (g:gs)		= plans agent
	 Plan c a	= g

-- Determine whether a certain list of propositions implies another list of propositions
implies	:: [Proposition] -> [Proposition] -> Bool
implies props1 props2	= intersect props2 props1 == props2

-- UPDATE AGENTS
-- Updates a list of agents if any of the agent's ids matches the changed agent
updateAgents	:: [Agent] -> Agent -> [Agent]
updateAgents agents agent	= map (updateAgent agent) agents

-- Updates a single agent if its id matches the changed agent
updateAgent a1 a2	| (idNr a1) == (idNr a2)	= a1
				| otherwise					= a2

getAgent	:: String -> HomeAutomationSystem -> Agent
getAgent agentId system	= head [ agent | agent <- agents system, idNr agent == agentId ]

addBelief	:: [Proposition] -> Proposition -> [Proposition]
addBelief [] newProp		= [newProp]
addBelief (p:props) newProp | p == Not newProp || Not p == newProp	= newProp : (addBelief props newProp)
							| otherwise	= p : (addBelief props newProp)
	 
incrementCounter :: Agent -> Agent
incrementCounter agent	= agent{beliefs=beliefs'}
	where
	 (b:bs)		= (beliefs agent)		-- the beliefs of the agent, with b being the counter status
	 Counter i	= b						
	 beliefs'	= (Counter (i+1)):bs	-- the beliefs of the agent with the counter incremented

-- TODO: implement	 
order :: [[Message]] -> [[Message]]
order (m:ms) rs = map 
	where
	 Message prop recAgent	= m
	 agentIndex				= elemIndex (getAgent recAgent) has
	 newList				= (rs !! agentIndex) ++ [m]
	 
-- TODO: implement
inRange :: Agent -> Agent -> Bool
inRange agent1 agent2	= (sqrt $ fromIntegral ((x1-x2)^2 + (y1-y2)^2)) < fromIntegral range
	where
	 (b1a:b1b:b1s)	= beliefs agent1
	 OnLocation agent1 (x1,y1)	= b1b
	 (b2a:b2b:b2s)	= beliefs agent2
	 OnLocation agent2 (x2,y2)	= b2b