import Data.Map.Strict
import qualified Data.List as List


data BinaryTree = Null | Node {
	value :: Char,
	frequency :: Int,
	left :: BinaryTree,
	right :: BinaryTree
}

instance Show BinaryTree where
	show (Null) = "null"
	show (Node v f l r) = "(" ++ show v ++ ", " ++ show f ++ ", LEFT=" ++ show l ++ ", RIGHT=" ++ show r ++ ")"

instance Eq BinaryTree where
	(Node v0 f0 l0 r0) == (Node v1 f1 l1 r1) = totalFrequency (Node v0 f0 l0 r0) == totalFrequency (Node v1 f1 l1 r1) && (v0 == v1) && (f0 == f1)
	_ == _ = False

instance Ord BinaryTree where
	left > right = totalFrequency left > totalFrequency right
	left >= right = totalFrequency left >= totalFrequency right
	left < right = totalFrequency left < totalFrequency right
	left <= right = totalFrequency left <= totalFrequency right

isLeaf :: BinaryTree -> Bool
isLeaf (Node _ _ Null Null) = True
isLeaf _ = False

getCharacter :: BinaryTree -> Char
getCharacter (Node v _ _ _) = v

getFrequency :: BinaryTree -> Int
getFrequency (Node _ f _ _) = f

getLeft :: BinaryTree -> BinaryTree
getLeft Null = Null
getLeft (Node _ _ l _) = l

setLeft :: BinaryTree -> BinaryTree -> BinaryTree
setLeft (Node v f l r) Null = (Node v f Null r)
setLeft (Node v f l r) node = (Node v f node r)

getRight :: BinaryTree -> BinaryTree
getRight Null = Null
getRight (Node _ _ _ r) = r

setRight :: BinaryTree -> BinaryTree -> BinaryTree
setRight (Node v f l r) Null = (Node v f Null r)
setRight (Node v f l r) node = (Node v f l node)

totalFrequency :: BinaryTree -> Int
totalFrequency Null = 0
totalFrequency (Node _ frequency left right) = frequency + (totalFrequency left) + (totalFrequency right)

frequenciesFromSequence :: String -> Map Char Int
frequenciesFromSequence (x:xs) = frequenciesFromSequence' xs (singleton x 1)
	where
		frequenciesFromSequence' :: String -> Map Char Int -> Map Char Int
		frequenciesFromSequence' [] charFrequencies = charFrequencies
		frequenciesFromSequence' (x:xs) charFrequencies
			| member x charFrequencies = frequenciesFromSequence' xs (adjust (+1) x charFrequencies)
			| otherwise = frequenciesFromSequence' xs (insert x 1 charFrequencies)

createFlorest :: Map Char Int -> [BinaryTree]
createFlorest charFrequencies = createFlorest' charFrequencies (keys charFrequencies) []
	where
		createFlorest' :: Map Char Int -> [Char] -> [BinaryTree] -> [BinaryTree]
		createFlorest' _ [] florest = List.sort florest
		createFlorest' charFrequencies (x:xs) florest = createFlorest' charFrequencies xs (florest ++ [node])
			where
				node = Node x (charFrequencies ! x) Null Null

createFlorestFromSequence :: String -> [BinaryTree]
createFlorestFromSequence sequence = createFlorest (frequenciesFromSequence sequence)

invertsSymbolCodes :: Map Char String -> Map String Char
invertsSymbolCodes symbolCodes = invertsSymbolCodes' symbolCodes (keys symbolCodes) empty
	where
		invertsSymbolCodes' :: Map Char String -> [Char] -> Map String Char -> Map String Char
		invertsSymbolCodes' symbolCodes [] codesSymbol = codesSymbol
		invertsSymbolCodes' symbolCodes (x:xs) codesSymbol = invertsSymbolCodes' symbolCodes xs (insert binaryCode x codesSymbol)
			where
				binaryCode = symbolCodes ! x

huffmanCode :: [BinaryTree] -> BinaryTree
huffmanCode [tree] = tree
huffmanCode (x:y:ys) = huffmanCode newQueue
	where
		newTree :: BinaryTree -> BinaryTree -> BinaryTree
		newTree (Node '_' 0 l0 r0) node = Node '_' 0 node (Node '_' 0 l0 r0)
		newTree node (Node '_' 0 l1 r1) = Node '_' 0 node (Node '_' 0 l1 r1)
		newTree nodeA nodeB = Node '_' 0 nodeA nodeB
		newQueue = List.sort (ys ++ [newTree x y])

sampleSequence = "TAATTAGAAATTCTATTATA"
sampleFlorest = createFlorestFromSequence sampleSequence
sampleHuffman = huffmanCode sampleFlorest

{-
('_', 0, 
	LEFT=('T', 9, LEFT=null, RIGHT=null), 
	RIGHT=('_', 0, 
		LEFT=('A', 9, LEFT=null, RIGHT=null), 
		RIGHT=('_', 0, 
			LEFT=('C', 1, LEFT=null, RIGHT=null), 
			RIGHT=('G', 1, LEFT=null, RIGHT=null)
			)
		)
	)
-}