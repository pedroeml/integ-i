import Data.Map.Strict
import qualified Data.List as List


data BinaryTree = Null | Node {
	value :: Char,
	frequency :: Int,
	left :: BinaryTree,
	right :: BinaryTree
} deriving Show

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


sampleSequence = "TTAGAACCTCTTT"
sampleTree = Node 'C' 10 (Node 'D' 5 Null Null) Null
