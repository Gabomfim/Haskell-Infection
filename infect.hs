
getAccWeight name nodes = snd (filter (\node ->  fst node == name) nodes)

finalResult :: [([Char], Int)] -> Int

finalResult nodes = maximum (map (\nodes -> snd nodes) nodes)
