
scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
string1 = "writers"
string2 = "vintner"
type AlignmentType = (String,String)

similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore [] (y:ys) = similarityScore [] ys + score '-' y
similarityScore (x:xs) [] = similarityScore xs [] + score x '-'
similarityScore (x:xs) (y:ys) =
  max (similarityScore xs ys + score x y) (max (similarityScore (x:xs) ys + score x '-') (similarityScore xs (y:ys) + score '-' y))


score :: Char -> Char -> Int
score x y
  | x == y = scoreMatch
  | x == '-' = scoreSpace
  | y == '-' = scoreSpace
  | otherwise = scoreMismatch

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = [ x | x <- xs, valueFcn x == maxValue]
  where maxValue = maximum $ map valueFcn xs

optAlignments :: String -> String -> [AlignmentType]
optAlignments string1 string2