-- working through ch2 of "learn you a haskell for great good!"

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 then x else x*2

weird x = succ (if x > 100 then x else x^2)

first_m_powers_of_n m n = take m [n^x | x <- [1,2..]]

fizzBuzz n = [if x `mod` 3 == 0 && x `mod` 5 == 0
              then "FizzBuzz"
              else if x `mod` 3 == 0
              then "Fizz"
              else if x `mod` 5 == 0
              then "Buzz" else "" | x <- [1..n]]

integerPointsOnCircle radius num = [(x,y) | x <- [1..num], 
                                            y <- [1..num], 
                                            x^2 + y^2 == radius^2]

removeNonUpperCase st = [c | c<-st, c `elem` ['A'..'Z']]

triangles = [(a,b,c) | c<-[1..100], b<-[1..100], a<-[1..100]]
rightTriangles = [(a,b,c) | (a,b,c)<-triangles, a^2 + b^2 == c^2]


