setNodeCost :: [Char] -> [([Char], Float)] -> Float -> [([Char], Float)]
setNodeCost name nodes value = map (\node -> if fst node == name then (name, value) else node) nodes

getNodeCost :: [Char] -> [([Char], Float)] -> Float
getNodeCost name nodes = snd (head (filter (\node ->  fst node == name) nodes))

getRelationWeight :: ([Char], [Char]) -> [(([Char], [Char]), Float)] -> Float
getRelationWeight (origin, destiny) relations = snd (head (filter (\relation ->  fst relation == (origin, destiny)) relations))

removeFromQueue :: [Char] -> [[Char]] -> [[Char]]
removeFromQueue name queue = filter (\item -> item /= name) queue

-- get all the destinies that has origin "origin"
getDestinies :: [Char] -> [(([Char], [Char]), Float)] -> [[Char]]
getDestinies origin relations = map (\((origin, destiny), weight) -> destiny) (filter (\relation ->  fst (fst relation) == origin) relations)

finalResult :: [([Char], Float)] -> Float
finalResult nodes = maximum (map (\nodes -> snd nodes) nodes)

-- Poda a árvore de busca, selecionando apenas os caminhos que valem a pena.
pruneTree :: ([Char], [Char]) -> [(([Char], [Char]), Float)] -> [([Char], Float)] -> [[Char]] -> [[Char]]
pruneTree (origin, destiny) relations nodes queue
    | isWorth (origin, destiny) relations nodes = queue -- Se o candidato valer a pena deixe como está
    | otherwise = removeFromQueue destiny queue -- caso contrário, remova ele da fila
    
pathCost :: ([Char], [Char]) -> [(([Char], [Char]), Float)] -> [([Char], Float)] -> Float
pathCost (origin, destiny) relations nodes = ((getNodeCost origin nodes) + (getRelationWeight (origin, destiny) relations))


isWorth :: ([Char], [Char]) -> [(([Char], [Char]), Float)] -> [([Char], Float)] -> Bool
isWorth (origin, destiny) relations nodes
    | (getNodeCost destiny nodes) > pathCost (origin, destiny) relations nodes = True
    | otherwise = False

-- Atualiza o custo de um nó se valer a pena
updateCostIfWorth :: ([Char], [Char]) -> [(([Char], [Char]), Float)] -> [([Char], Float)] -> [([Char], Float)]
updateCostIfWorth (origin, destiny) relations nodes
    | isWorth (origin, destiny) relations nodes = setNodeCost destiny nodes (pathCost (origin, destiny) relations nodes)
    | otherwise = nodes

-- Executa updateCostIfWorth e pruneTree. Devolve uma tupla com os resultados.
step :: ([Char], [Char]) -> [(([Char], [Char]), Float)] -> [([Char], Float)] -> [[Char]] -> ([([Char], Float)], [[Char]])
step (origin, destiny) relations nodes queue = (updateCostIfWorth (origin, destiny) relations nodes, pruneTree (origin, destiny) relations nodes queue)

-- Adiciona itens conectados à origem na fila.
-- enqueue = 

-- Executa um step pra cada item da lista
-- wrapper _ _ _ [] = ([],[])
-- wrapper (origin, destiny) relations nodes queue = step (origin, destiny) relations nodes queue)
