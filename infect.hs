setNodeCost :: [Char] -> [([Char], Float)] -> Float -> [([Char], Float)]
setNodeCost name nodes value = map (\node -> if fst node == name then (name, value) else node) nodes

getNodeCost :: [Char] -> [([Char], Float)] -> Float
getNodeCost name nodes = snd (head (filter (\node ->  fst node == name) nodes))

getRelationWeight :: ([Char], [Char]) -> [(([Char], [Char]), Float)] -> Float
getRelationWeight (origin, destiny) relations = snd (head (filter (\relation ->  fst relation == (origin, destiny)) relations))

finalResult :: [([Char], Float)] -> Float
finalResult nodes = maximum (map (\nodes -> snd nodes) nodes)

-- Poda a árvore de busca, selecionando apenas os caminhos que valem a pena.
pruneTree :: ([Char], [Char]) -> [(([Char], [Char]), Float)] -> [([Char], Float)] -> [[Char]] -> [[Char]]
pruneTree (origin, destiny) relations nodes queue
    | isWorth (origin, destiny) relations nodes = queue++[destiny] -- Se o candidato valer a pena, adicionar na fila
    | otherwise = queue
    
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
