-- more learn you a haskell..ended at 4.2

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c<-st, c `elem` ['A'..'Z']]

factorial :: Integer -> Integer
factorial n = product [1..n]

--circumference :: Float -> Float
--circumference f = 2 * pi * f
--circumference :: Int -> Int -- apparently overloading types doesn't work. :(
--circumference i = 6 * i
--
--what about type variables?

--circumference :: Num a => a -> a 
--circumference f = 2 * pi * f  -- this throws an error too
--                                 "could not deduce (Floating a) 
--                                 arising from a use of `pi`
--
circumference :: Floating a => a -> a -- this works, but not the type matching I
                                      -- wanted
circumference f = 2 * pi * f

-- this doesn't work either
--my_f :: Num a => a -> a
--my_f x :: Int = a + a
--my_f x :: Double = a * pi
--

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two"
sayMe x = "nope"

factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

iwillFail :: Char -> String
iwillFail 'a' = "Bob"

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a,b,c) -> a
first (x, _, _) = x

tell :: (Show a) => [a] -> String
tell [] = "emply"
tell (x:[]) = "just " ++ show x
tell (x:y:[]) = "just " ++ show x ++ " and " ++ show y
tell (x:y:_) = "looong, first is " ++ show x

getCapital :: String -> String
getCapital "" = "nada"
getCapital all@(x:_) = "the first letter of " ++ all ++ " is " ++ [x]
