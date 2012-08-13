-- even more learn you a haskel.  ending at 5.2

bmi_tell :: (RealFloat a) => a -> String
bmi_tell bmi
    | bmi <= 18.5   = "emo!"
    | bmi <= 25.0   = "normal..."
    | bmi <= 30.0   = "heavy!"
    | otherwise     = "fat!"

calc_bmi :: (RealFloat a) => a -> a -> a
calc_bmi weight height = weight / height^2

bmi_advise :: (RealFloat a) => a -> a -> String
bmi_advise w h = bmi_tell (calc_bmi w h)

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

my_compare :: (Ord a) => a -> a -> Ordering
a `my_compare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

initials :: String -> String -> String 
initials first_name last_name = [f] ++ ". "  ++ [l] ++ "."
    where   (f:_) = first_name
            (l:_) = last_name  
-- we can do better, how 'bout an initials that takes one string and returns the
-- initials by looking at capital letters?

improved_initials :: String -> [String]
improved_initials name = [[i] ++ ". " | i<-capitals]
    where capitals = [cap | cap<-name, cap `elem` ['A'..'Z']]

-- close, but this is a list of Strings, not a String itself.  Can we reduce? 
-- I'm sure we can, it's all functional, so reduce and lambdas should abound.
-- google suggests that \x y -> x ++ y is the lambda, and foldl is the function

improved_initials2 :: String -> String
improved_initials2 name = (foldl (\x y -> x ++ ". " ++ [y]) 
                                 [f] 
                                 rest) ++ "."
    where f:rest = [cap | cap<-name, cap `elem` ['A'..'Z']]

-- that took some work to figure out!
-- so as best I can tell if foldl :: Lambda(?) -> a -> [b] 
-- then whatever is bound to the Lambda(?) type must be of
-- \ ? :: a -> * -> a, that it, the lambda (lower case, not a typeclass here) 
-- must start taking the same type as the initial folding point and return that 
-- same type too.  am I gonna remember this or figure this out from this note
-- in two weeks time? ha!

-- so do lambdas have an anonymous type signature? 

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = let sideArea = 2 * pi * r *h
                   topArea = pi * r^2
               in  sideArea + 2 * topArea

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton."
                                               xs -> "a long list."

describeList2 :: (Show a) => [a] -> String 
describeList2 xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton."
          what (x:rest) = "a long list starting with " ++ show x ++ " and is "++
                          show (length rest + 1) ++ "long."

-- this intro to haskell is almost too easy. I know what recursion is, dagnabit!
-- but it hardly feels like work, so I'll keep doing this tutorial. :)

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "nil list"
maximum' [x] = x
maximum' (x:xs)
    | x > max_tail = x
    | otherwise = max_tail
    where max_tail = maximum' xs

