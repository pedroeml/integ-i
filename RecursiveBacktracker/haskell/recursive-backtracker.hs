

data Color = BACK | ANORMAL | TEMPORARY | PATH deriving Show

type Labyrinth = [[Color]]

instance Eq Color where
	BACK == BACK = True
	ANORMAL == ANORMAL = True
	TEMPORARY == TEMPORARY = True
	PATH == PATH = True
	_ == _ = False


grid = [[BACK, ANORMAL, BACK, ANORMAL, ANORMAL],
		[BACK, ANORMAL, BACK, ANORMAL, ANORMAL],
		[BACK, BACK, BACK, ANORMAL, BACK],
		[BACK, ANORMAL, BACK, BACK, BACK],
		[BACK, ANORMAL, BACK, ANORMAL, BACK]]

getXYColor :: Labyrinth -> (Int, Int) -> Color
getXYColor (x:xs) (l, c)
	| l == 0 = getColor x c
	| otherwise = getXYColor xs (l - 1, c)
		where
			getColor :: [Color] -> Int -> Color
			getColor (x:xs) 0 = x
			getColor (x:xs) c = getColor xs (c - 1)

setXYColor :: Labyrinth -> (Int, Int) -> Color -> Labyrinth
setXYColor (x:xs) (l, c) color
	| l == 0 = (setColor x c color) : xs
	| otherwise = x : (setXYColor xs (l - 1, c) color)
		where
			setColor :: [Color] -> Int -> Color -> [Color]
			setColor (x:xs) 0 color = color : xs
			setColor (x:xs) c color = x : (setColor xs (c - 1) color)

searchPath :: Labyrinth -> (Bool, Labyrinth)
searchPath labyrinth = searchPath' labyrinth (0, 0) False

searchPath' :: Labyrinth -> (Int, Int) -> Bool -> (Bool, Labyrinth)
searchPath' labyrinth _ True = (True, labyrinth)
searchPath' labyrinth (l, c) False
	| l < 0 || c < 0 || l >= length labyrinth || c >= length (head labyrinth) = (False, labyrinth)	-- Se as coordenadas estiverem fora dos limites
	| not ((getXYColor labyrinth (l, c)) == BACK) = (False, labyrinth)	-- Se as coordenadas estiverem em uma barreira ou beco sem saída
	| l == ((length labyrinth) - 1) && c == ((length (head labyrinth)) - 1) = searchPath' labyrinth' (l, c) True 	-- Coordenadas pertencem ao caminho e é a saída do labirinto
	| fst north = north 	-- Caso recursivo: Tentar encontrar um caminho nas coordenadas vizinhas
	| fst south = south
	| fst west = west
	| fst east = east
	| otherwise = (False, (setXYColor labyrinth (l, c) TEMPORARY)) 	-- Beco sem saída
		where
			labyrinth' = setXYColor labyrinth (l, c) PATH
			north = searchPath' labyrinth' (l - 1, c) False
			south = searchPath' (snd north) (l + 1, c) False
			west = searchPath' (snd south) (l, c - 1) False
			east = searchPath' (snd west) (l, c + 1) False
