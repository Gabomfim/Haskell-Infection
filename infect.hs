getNodeCost :: [Char] -> [([Char], Float)] -> Float
getNodeCost name nodes = snd (head (filter (\node ->  fst node == name) nodes))

getRelationWeight :: ([Char], [Char]) -> [(([Char], [Char]), Float)] -> Float
getRelationWeight (origin, destiny) relations = snd (head (filter (\relation ->  fst relation == (origin, destiny)) relations))

finalResult :: [([Char], Float)] -> Float
finalResult nodes = maximum (map (\nodes -> snd nodes) nodes)



-- Poda a árvore de busca, selecionando apenas os caminhos que valem a pena
-- pruneTree origin destiny queue = soma o peso da relacao de origem pra destino, se o candidato valer a pena, adicionar na fila

