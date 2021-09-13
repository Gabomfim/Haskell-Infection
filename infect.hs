setNodeCost :: [Char] -> [([Char], Float)] -> Float -> [([Char], Float)]
setNodeCost name nodes value = map (\node -> if fst node == name then (name, value) else node) nodes

getNodeCost :: [Char] -> [([Char], Float)] -> Float
getNodeCost name nodes = snd (head (filter (\node ->  fst node == name) nodes))

getRelationWeight :: ([Char], [Char]) -> [(([Char], [Char]), Float)] -> Float
getRelationWeight (origin, destiny) relations = snd (head (filter (\relation ->  fst relation == (origin, destiny)) relations))

removeFromQueue :: [Char] -> [[Char]] -> [[Char]]
removeFromQueue name queue = filter (\item -> item /= name) queue

-- The list is just for the first in the queue
createOriginDestinyList :: [(([Char], [Char]), Float)] -> [([Char], Float)] -> [[Char]] -> [([Char], [Char])]
createOriginDestinyList relations nodes queue = map (\destiny -> ((head queue), destiny)) (getDestinies (head queue) relations)

-- get all the destinies that has origin "origin"
getDestinies :: [Char] -> [(([Char], [Char]), Float)] -> [[Char]]
getDestinies origin relations = map (\((origin, destiny), weight) -> destiny) (filter (\relation ->  fst (fst relation) == origin) relations)

finalResult :: [([Char], Float)] -> Float
finalResult nodes = maximum (map (\nodes -> snd nodes) nodes)

-- Poda a árvore de busca, selecionando apenas os caminhos que valem a pena.
pruneTree :: ([Char], [Char]) -> [(([Char], [Char]), Float)] -> [([Char], Float)] -> [[Char]] -> [[Char]]
pruneTree (origin, destiny) relations nodes queue
    | isWorth (origin, destiny) relations nodes = queue -- Se o candidato valer a pena deixe como está.
    | otherwise = removeFromQueue destiny queue -- caso contrário, remova ele.
    
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
step :: ([Char], [Char]) -> [(([Char], [Char]), Float)] -> [([Char], Float)] -> ([([Char], Float)], [[Char]])
step (origin, destiny) relations nodes = (updateCostIfWorth (origin, destiny) relations nodes, pruneTree (origin, destiny) relations nodes (getDestinies origin relations))

-- gere uma lista (origem, destino) para o primeiro item da fila de nomes
-- escolha a primeira tupla (origem, destino) da lista
-- execute step essa tupla (origem, destino) -> isso removerá a origem da fila de nomes, colocará os destinos nessa fila e atualizará o que for necessário
-- remova essa tupla origem destino da lista de origens e destinos
-- quando essa lista ficar vazia, execute o algoritmo novamente

singleLevelDFS :: [(([Char], [Char]), Float)] -> ([([Char], Float)], [[Char]]) -> [([Char], [Char])] -> ([([Char], Float)], [[Char]])
singleLevelDFS _ (nodes, queue) [] = (nodes, queue)
singleLevelDFS relations (nodes, nextInQueue) originDestinyList = singleLevelDFS relations (step (head originDestinyList) relations nodes) (tail originDestinyList)

-- pra cada item na fila, ele roda o mesmo código
--iterator (node, queue) queue = iterator (head queue) (tail queue):q $ singleLevelDFS relations (nodes, [head queue]) originDestinyList

--wrapper :: [(([Char], [Char]), Float)] -> ([([Char], Float)], [[Char]]) -> [([Char], [Char])] -> ([([Char], Float)], [[Char]])
wrapper _ (nodes, []) _ = (nodes, [])
wrapper relations (nodes, queue) originDestinyList = (\(n, q) -> wrapper relations (n, (tail queue)++q) (createOriginDestinyList relations n ((tail queue)++q))) $ singleLevelDFS relations (nodes, [head queue]) originDestinyList

-- começa o algoritmo | queue deve conter apenas o infectado | nodes devem começar todos com infinito, menos o infectado, que deve ter peso 0.
run :: [(([Char], [Char]), Float)] -> ([([Char], Float)], [[Char]]) -> Float
run relations (nodes, queue) = finalResult (fst (wrapper relations (nodes, queue) (createOriginDestinyList relations nodes queue)))