-- learn you a haskell ch 7
--
import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

getStockOver1000 :: [(Float, Int, Int, Int)] -> Maybe (Float, Int, Int, Int)
getStockOver1000 = find (\(val,y,m,d) -> val > 1000)

sliceSentence :: String -> String
sliceSentence str = "First word: " ++ fw ++ ", the rest:" ++ rest
    where (fw, rest) = span (/=' ') str

countSimElems :: (Eq a, Ord a) => [a] -> [(a,Int)]
countSimElems = map (\l@(x:xs) -> (x, length l)) . group . sort 

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = 
    let nlen = length needle
    in foldl (\acc x -> if take nlen x == needle then True else acc) 
             False 
             (tails haystack)

-- end at 7.3 Data.Char
