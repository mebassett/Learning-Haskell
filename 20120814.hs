-- why is i Ord?  Int -> a -> [a] works fine
replicate' :: (Num i, Ord i) => i -> a -> [a] 
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

take' :: Int -> [a] -> [a]
take' n _
    | n <= 0    = []
take' _ []      = []
take' n (x:xs)  = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

replicate2 :: Int -> a -> [a]
replicate2 n x = take' n (repeat' x)

-- zip' :: [a....oh I get the point already.
--

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smaller = quicksort [a | a <- xs, a <= x]
        bigger = quicksort [a | a <- xs, a > x]
    in smaller ++ [x] ++ bigger

