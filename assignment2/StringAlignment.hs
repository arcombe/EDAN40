module StringAlignment where

  scoreMatch = 0
  scoreMismatch = -1
  scoreSpace = -1
  string1 = "writers"
  string2 = "vintner"

  type AlignmentType = (String,String)

  similarityScore :: String -> String -> Int
  similarityScore [] []         = 0
  similarityScore [] (y:ys)     = similarityScore [] ys + score '-' y
  similarityScore (x:xs) []     = similarityScore xs [] + score x '-'
  similarityScore (x:xs) (y:ys) =
    max (similarityScore xs ys + score x y) (max (similarityScore (x:xs) ys + score x '-') (similarityScore xs (y:ys) + score '-' y))


  score :: Char -> Char -> Int
  score x y
    | x == '-'  = scoreSpace
    | y == '-'  = scoreSpace
    | x == y    = scoreMatch
    | otherwise = scoreMismatch

  attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
  attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

  maximaBy :: Ord b => (a -> b) -> [a] -> [a]
  maximaBy valueFcn xs = [ x | x <- xs, valueFcn x == maxValue]
    where maxValue = maximum $ map valueFcn xs

  optAlignments :: String -> String -> [AlignmentType]
  optAlignments [] []         = [("", "")]
  optAlignments (x:xs) []     = attachHeads x '-' (optAlignments xs []) -- Align against empty string, eg. "abc" and "" -> [("","")] - > [("c","-")] -> [("bc","--")] -> [("abc","---")]
  optAlignments [] (y:ys)     = attachHeads '-' y (optAlignments [] ys)
  optAlignments (x:xs) (y:ys) = maximaBy (tupleArgFunc similarityScore) (
    attachHeads x y (optAlignments xs ys) ++
    attachHeads x '-' (optAlignments xs (y:ys)) ++
    attachHeads '-' y (optAlignments (x:xs) ys)
    )
      where tupleArgFunc f (arg1, arg2) = f arg1 arg2
