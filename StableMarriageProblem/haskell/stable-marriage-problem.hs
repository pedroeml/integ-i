

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
	matchingPartner :: String,
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

getMatchingPartner :: Person -> String
getMatchingPartner (Person _ _ _ _ matchingPartner _) = matchingPartner

isFree :: Person -> Bool
isFree (Person _ _ _ _ _ free) = free

isNotYetProposedEmpty :: Person -> Bool
isNotYetProposedEmpty (Person _ _ _ [] _ _) = True
isNotYetProposedEmpty (Person _ _ _ (x:xs) _ _) = False

popNotYetProposedPerson :: Person -> [Person] -> (Person, Person)
popNotYetProposedPerson (Person n g p (x:xs) m f) l = ((Person n g p xs m f), findName x l)

findName :: String -> [Person] -> Person
findName name [] = None
findName name (x:xs)
	| name == getName x = x
	| otherwise = findName name xs

setMatchingPartner :: Person -> String -> Person
setMatchingPartner (Person n g p l _ _) partnerName = (Person n g p l partnerName False)

prefersSomeoneOverCurrentMatchingPartner :: Person -> Person -> Bool
prefersSomeoneOverCurrentMatchingPartner (Person _ _ _ _ "" True) _ = True
prefersSomeoneOverCurrentMatchingPartner (Person _ _ (x:xs) _ m _) someone
	| getName someone == m = False 		-- se someone e o partner são os mesmos, então não há razões para comparar
	| otherwise = prefersAoverB (x:xs) m (getName someone)
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

{-
Tentei adaptar o algoritmo presente em Matching.java no código comentado abaixo, mas não foi possível concluir.
Por conta de Haskell não permitir que os dados sejam modificados, tentei contornar isso criando sucessivamente
uma nova lista 'men' e 'women' a cada vez que um type class de Person fosse criado a partir de uma modificação
como troca de partner e pop da notYetPorposedPersons. Esta estratégia é complicada de manter para este problema
ainda quando é necessário adaptar iteração na lista 'men' dentro de while (thereIsAnyManFree()) usando apenas
recursão.

doMatching :: [Person] -> [Person] -> ([Person], [Person])
doMatching [] _ = []
doMatching _ [] = []
doMatching menList womenList = whileThereIsAnyManFree menList womenList
	where
		whileThereIsAnyManFree :: [Person] -> [Person] -> ([Person], [Person])
		whileThereIsAnyManFree (x:xs) (y:ys)
			| thereIsAnyManFree (x:xs) = whileThereIsAnyManFree menList womenList
			| otherwise = ((x:xs), (y:ys))
				where
					(menList, womenList) = foreachMan (x:xs) (y:ys) 

foreachMan :: [Person] -> [Person] -> ([Person], [Person])
foreachMan [] _ = ([], [])
foreachMan _ [] = ([], [])
foreachMan (x:xs) (y:ys)
	| not (isFree x) = foreachMan xs (y:ys)
	| not (isNotYetProposedEmpty x) = foreachMan (updateLists m (updateLists p (x:xs))) (updateLists w (y:ys))
	| otherwise = ([x] ++ foreachMan xs (y:ys), (y:ys))
		where
			(m, w, p) = aux (popNotYetProposedPerson x (y:ys))
				where
					aux :: (Person, Person) -> [Person] -> (Person, Person, Person) 
					aux man woman menList
						| isFree woman = (m, w, None)
						| prefersSomeoneOverCurrentMatchingPartner woman man = (m, w, p)
							where
								m = setMatchingPartner man (getName woman)
								w = setMatchingPartner woman (getName man)
								p = setMatchingPartner (findName (getMatchingPartner w) menList) ""
-}

updateLists :: Person -> [Person] -> [Person]
updateLists p [] = []
updateLists None (x:xs) = (x:xs)
updateLists p (x:xs)
	| p == x = (p:xs)
	| otherwise = [x] ++ updateLists p xs

m1 = Person {name = "1", gender = MALE, preferenceList = ["4", "1", "2", "3"], notYetProposed = ["4", "1", "2", "3"], matchingPartner = "", free = True}
m2 = Person {name = "2", gender = MALE, preferenceList = ["2", "3", "1", "4"], notYetProposed = ["2", "3", "1", "4"], matchingPartner = "", free = True}
m3 = Person {name = "3", gender = MALE, preferenceList = ["2", "4", "3", "1"], notYetProposed = ["2", "4", "3", "1"], matchingPartner = "", free = True}
m4 = Person {name = "4", gender = MALE, preferenceList = ["3", "1", "4", "2"], notYetProposed = ["3", "1", "4", "2"], matchingPartner = "", free = True}
a1 = Person {name = "1", gender = FEMALE, preferenceList = ["4", "1", "3", "2"], notYetProposed = ["4", "1", "3", "2"], matchingPartner = "", free = True}
a2 = Person {name = "2", gender = FEMALE, preferenceList = ["1", "3", "2", "4"], notYetProposed = ["1", "3", "2", "4"], matchingPartner = "", free = True}
a3 = Person {name = "3", gender = FEMALE, preferenceList = ["1", "2", "3", "4"], notYetProposed = ["1", "2", "3", "4"], matchingPartner = "", free = True}
a4 = Person {name = "4", gender = FEMALE, preferenceList = ["4", "1", "2", "3"], notYetProposed = ["4", "1", "2", "3"], matchingPartner = "", free = True}

men = [m1, m2, m3, m4]
women = [a1, a2, a3, a4]
