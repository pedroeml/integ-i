

data Gender = MALE | FEMALE deriving Show

instance Eq Gender where
	MALE == MALE = True
	FEMALE == FEMALE = True
	_ == _ = False

data Person = None | Person {
	name :: String,
	gender :: Gender,
	preferenceList :: [String],
	notYetProposed :: [String],
	matchingPartner :: Person,
	free :: Bool
} deriving Show

{-
instance Show Person where
	show (Person name gender _ _ _ free) = "(" ++ show name ++ ", " ++ show gender ++ ", " ++ show free ++ ")"
-}
instance Eq Person where
	(Person nameA genderA _ _ _ _) == (Person nameB genderB _ _ _ _) = nameA == nameB && genderA == genderB
	_ == _ = False

getName :: Person -> String
getName (Person name _ _ _ _ _) = name

getGender :: Person -> Gender
getGender (Person _ gender _ _ _ _) = gender

getMatchingPartner :: Person -> Person
getMatchingPartner (Person _ _ _ _ matchingPartner _) = matchingPartner

isFree :: Person -> Bool
isFree (Person _ _ _ _ _ free) = free

isNotYetProposedEmpty :: Person -> Bool
isNotYetProposedEmpty (Person _ _ _ [] _ _) = True
isNotYetProposedEmpty (Person _ _ _ (x:xs) _ _) = False

popNotYetProposedPerson :: Person -> (Person, String)
popNotYetProposedPerson (Person n g p (x:xs) m f) = ((Person n g p xs m f), x)

setMatchingPartner :: Person -> Person -> Person
setMatchingPartner (Person n g p (x:xs) _ _) None = (Person n g p (x:xs) None True)
setMatchingPartner (Person n g p (x:xs) _ _) partner = (Person n g p (x:xs) partner False)

prefersSomeoneOverCurrentMatchingPartner :: Person -> Person -> Bool
prefersSomeoneOverCurrentMatchingPartner (Person _ _ _ _ None True) _ = True
prefersSomeoneOverCurrentMatchingPartner (Person _ _ (x:xs) _ m _) someone
	| someone == m = False 		-- se someone e o partner são os mesmos, então não há razões para comparar
	| otherwise = prefersAoverB (x:xs) (getName m) (getName someone)
		where
			prefersAoverB :: [String] -> String -> String -> Bool
			prefersAoverB [] _ _ = False
			prefersAoverB (x:xs) matchingPartner someone
				| x == matchingPartner = False	-- se encontrar primeiro o atual partner, então não prefere someone
				| x == someone = True	-- se encontrar primeiro someone, então prefere someone do que partner
				| otherwise = prefersAoverB xs matchingPartner someone	-- continua a recursão

thereIsAnyManFree :: [Person] -> Bool
thereIsAnyManFree [] = False
thereIsAnyManFree (x:xs)
	| isFree x = True
	| otherwise = thereIsAnyManFree xs

doMatching :: [Person] -> [Person] -> [(Person, Person)]
doMatching [] _ = []
doMatching _ [] = []
doMatching (x:xs) (y:ys) = whileThereIsAnyManFree (x:xs) (y:ys)
	where
		whileThereIsAnyManFree :: [Person] -> [Person] -> [(Person, Person)]
		whileThereIsAnyManFree menList womenList
			| thereIsAnyManFree menList






m1 = Person {name = "1", gender = MALE, preferenceList = ["4", "1", "2", "3"], notYetProposed = ["4", "1", "2", "3"], matchingPartner = None, free = True}
m2 = Person {name = "2", gender = MALE, preferenceList = ["2", "3", "1", "4"], notYetProposed = ["2", "3", "1", "4"], matchingPartner = None, free = True}
m3 = Person {name = "3", gender = MALE, preferenceList = ["2", "4", "3", "1"], notYetProposed = ["2", "4", "3", "1"], matchingPartner = None, free = True}
m4 = Person {name = "4", gender = MALE, preferenceList = ["3", "1", "4", "2"], notYetProposed = ["3", "1", "4", "2"], matchingPartner = None, free = True}
a1 = Person {name = "1", gender = FEMALE, preferenceList = ["4", "1", "3", "2"], notYetProposed = ["4", "1", "3", "2"], matchingPartner = None, free = True}
a2 = Person {name = "2", gender = FEMALE, preferenceList = ["1", "3", "2", "4"], notYetProposed = ["1", "3", "2", "4"], matchingPartner = None, free = True}
a3 = Person {name = "3", gender = FEMALE, preferenceList = ["1", "2", "3", "4"], notYetProposed = ["1", "2", "3", "4"], matchingPartner = None, free = True}
a4 = Person {name = "4", gender = FEMALE, preferenceList = ["4", "1", "2", "3"], notYetProposed = ["4", "1", "2", "3"], matchingPartner = None, free = True}

men = [m1, m2, m3, m4]
women = [a1, a2, a3, a4]
