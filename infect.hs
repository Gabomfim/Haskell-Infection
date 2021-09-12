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

-- cria a tupla para representar o grafo e as distâncias dos nós
interpreter :: [String] -> ([(String, Float)], [((String, String), Float)])
interpreter lista = (createDistances lista [], createRelations lista)

-- cria um vetor de tuplas (nome, dist) que representam os nos e suas distancias
createDistances :: [String] -> [(String, Float)] -> [(String, Float)]
-- entrada chegou no final, coloca o nó inicial com distância 0
createDistances [nome] distancias = setNodeCost nome distancias 0
createDistances (nome1:nome2:dist:rest) distancias =  createDistances rest dist
    where dist = distancias ++ (createTuple nome1 distancias) ++ (createTuple nome2 distancias)

--função auxiliar para checar se nome existe em tupla de nos
checkName :: String -> [(String, Float)] -> Bool
checkName name [] = False
checkName name (a:as)
    | name == fst a = True
    | otherwise = checkName name as

-- cria tupla se nome não estiver presente no vetor original
createTuple :: String -> [(String, Float)] -> [(String, Float)]
createTuple name list 
    | checkName name list = []
    | otherwise = [(name, 1/0)]

-- cria um vetor de ((nome, nome), dist) que representa as arestas do grafo
createRelations :: [String] ->  [((String, String), Float)]
createRelations [nome] = []
createRelations (nome1:nome2:dist:rest) = [((nome1, nome2), (read dist :: Float)), ((nome2, nome1), (read dist :: Float))] ++ createRelations rest
