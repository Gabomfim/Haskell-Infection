import System.IO  
import Control.Monad
import Data.List

receive = do  
        let list = []
        handle <- openFile "in1.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
        print singlewords



getAccWeight name nodes = snd (filter (\node ->  fst node == name) nodes)

finalResult :: [([Char], Int)] -> Int

finalResult nodes = maximum (map (\nodes -> snd nodes) nodes)
