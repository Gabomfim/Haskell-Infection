import System.IO  
import Control.Monad
import Data.List

receive = do  
        let list = []
        handle <- openFile "in1.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
        print singlewords



-- AUX METHODS --

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


-- INPUT --

-- cria a tupla para representar o grafo e as distâncias dos nós
interpreter :: [String] -> (([(String, Float)], [String]), [((String, String), Float)])
interpreter lista = (patientZero lista [], createRelations lista)

-- cria um vetor de tuplas (nome, dist) que representam os nos e suas distancias
patientZero :: [String] -> [(String, Float)] -> ([(String, Float)], [String])
-- entrada chegou no final, coloca o nó inicial com distância 0, coloca o infectado numa fila e retorna
patientZero [nome] distancias = ((setNodeCost nome distancias 0), [nome])
patientZero (nome1:nome2:dist:rest) distancias =  patientZero rest dist
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


-- ALGORITMO --

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
infect :: ([Char], [Char]) -> [(([Char], [Char]), Float)] -> [([Char], Float)] -> ([([Char], Float)], [[Char]])
infect (origin, destiny) relations nodes = (updateCostIfWorth (origin, destiny) relations nodes, pruneTree (origin, destiny) relations nodes (getDestinies origin relations))



singleLevelDFS :: [(([Char], [Char]), Float)] -> ([([Char], Float)], [[Char]]) -> [([Char], [Char])] -> ([([Char], Float)], [[Char]])
singleLevelDFS _ (nodes, queue) [] = (nodes, queue)
singleLevelDFS relations (nodes, nextInQueue) originDestinyList = singleLevelDFS relations (infect (head originDestinyList) relations nodes) (tail originDestinyList)

-- gere uma lista (origem, destino) para o primeiro item da fila de nomes
-- escolha a primeira tupla (origem, destino) da lista
-- execute infect essa tupla (origem, destino) -> isso removerá a origem da fila de nomes, colocará os destinos nessa fila e atualizará o que for necessário
-- remova essa tupla origem destino da lista de origens e destinos
-- quando essa lista ficar vazia, execute o algoritmo novamente
wrapper :: [(([Char], [Char]), Float)] -> ([([Char], Float)], [[Char]]) -> [([Char], [Char])] -> ([([Char], Float)], [[Char]])
wrapper _ (nodes, []) _ = (nodes, [])
wrapper relations (nodes, queue) originDestinyList = (\(n, q) -> wrapper relations (n, (tail queue)++q) (createOriginDestinyList relations n ((tail queue)++q))) $ singleLevelDFS relations (nodes, [head queue]) originDestinyList

-- começa o algoritmo | queue deve conter apenas o infectado | nodes devem começar todos com infinito, menos o infectado, que deve ter peso 0.
run :: [(([Char], [Char]), Float)] -> ([([Char], Float)], [[Char]]) -> Float
run relations (nodes, queue) = finalResult (fst (wrapper relations (nodes, queue) (createOriginDestinyList relations nodes queue)))

-- TESTCASE:
-- run [(("a", "b"), 2), (("b", "a"), 2), (("a", "c"), 3), (("c", "a"), 3), (("c", "d"), 4), (("d", "c"), 4)] ([("a", 0) , ("b", 1/0), ("c", 1/0), ("d", 1/0)], ["a"])
-- interpreter ["a", "b", "2", "a", "c", "3", "c", "d", "4", "a"]


-- ([("a",0.0),("b",Infinity),("c",Infinity),("d",Infinity)],["a"])
-- ([("a", 0) , ("b", 1/0), ("c", 1/0), ("d", 1/0)], ["a"])

-- out: [(("a", "b"), 2), (("b", "a"), 2), (("a", "c"), 3), (("c", "a"), 3), (("c", "d"), 4), (("d", "c"), 4)]
-- out: [(("a","b"),2.0),(("b","a"),2.0),(("a","c"),3.0),(("c","a"),3.0),(("c","d"),4.0),(("d","c"),4.0)]