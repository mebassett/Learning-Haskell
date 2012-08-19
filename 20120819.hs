-- learning you a haskell at ch6
-- curry
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a->a
divideByTen = (/10)

tenDividedBy :: (Floating a) => a->a
tenDividedBy = (10 / )

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- higher order functions (this is getting a bit boring)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (x1:xs1) (x2:xs2) = [f x1 x2] ++ (myZipWith f xs1 xs2) 
-- I guess the book says we can write that f x1 x2 : myZipWith..   forgot about that

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = g
    where g x y = f y x

largestDivisible :: (Integral a) => a -> a
largestDivisible n = head (filter p [100000,99999..])
    where p x = x `mod` n == 0

lotsOfNumbers :: Int -> [Int]
lotsOfNumbers n = takeWhile (<n) [1,2..]

collatz :: (Integral a) => a -> [a] --I don't think we know that this terminates
collatz 1 = [1]
collatz n
    | even n = n:collatz (n `div` 2)
    | odd n = n:collatz (n*3 + 1)

numLongChains :: Int -> Int
numLongChains n = length ( filter (\xs -> length xs > 15) (map collatz [1..n]))

-- folds, maps, filters, lambdas, been there, done that

sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x==y then True else acc) False ys

--ch6 done
