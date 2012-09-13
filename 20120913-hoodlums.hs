module Schools where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Map (Map, (!))
import qualified Data.Map as Map

(!!!) = (Vector.!)

type Student = String

data School = School {schPan :: Int, schPriority :: [Student]}
	deriving (Show, Eq, Ord)

data Pref = Pref {prefStudent :: Student, prefChoices :: [Int]}
	deriving (Show, Eq, Ord)

data Offer = Offer {offStudent :: Student, offSchool :: Int}
	deriving (Show, Eq, Ord)

schools :: [School]
schools = [ School 2 ["A", "B", "D", "C"],
	    School 2 ["B", "D", "A", "C"]]

prefs :: [Pref]
prefs = [ Pref "A" [0, 1],
	  Pref "B" [1, 0],
	  Pref "C" [1, 0],
	  Pref "D" [0, 1]]


-- So given a list of schools, and for each school a list of all students in order of preference, and given a list of students with a list of their ordered preference of schools, give a list of offers (1 per student) that gives the best student-school-preference.
--
-- our algorithm is simple.  We take a student, try to match them up to their first choice, if they appear in the first schoolPan of students.  If so, we make an offer, if not, we move on to the next student, and then look up the students second choice, and so fourth.
type State = (Vector Int, [Offer], [Pref])

-- so this is what the meetup came up with..
allocate :: [School] -> [Pref] -> [Offer] 
allocate school prefs = offers
	where 
		(_,offers,_) = until done iteration (pans, [], prefs) 
		
		iteration :: State -> State
		iteration (pans, offers, unallocated) = foldl go (pans, offers, []) prefs
		done :: State -> Bool
		done (_,_, []) = True
		done _	       = False
	
		ranking :: School -> Map Student Int
		ranking (School _ priority) = Map.fromList $ zip priority [0..]
		
		schools' :: Vector (Map Student Int)
		schools' = Vector.fromList $ map ranking schools 
		
		pans = Vector.fromList $ map schPan schools
		
		go :: State -> Pref -> State
		go (pans, offers, unallocated) p@(Pref student choices)  = 
			foldr f (pans, offers, p:unallocated) choices
			where f :: Int -> State -> State
			      f sch cont = if rank < (pans !!! sch ) 
					   then (pans', Offer student sch : offers, unallocated)
					   else cont
				where rank = (schools' !!! sch) ! student 
				      pans' = Vector.imap conditionalInc pans 
				      conditionalInc i pan | i == sch 	= pan
							   | otherwise 	= pan + 1 

--and here is my attempt at a lispy solution:
--allocate' :: [School] -> [Pref] -> [Offer]
--allocate' (_ []) ->sdfa 
	
