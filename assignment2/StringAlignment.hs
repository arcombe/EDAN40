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

  optAlignmentsMemoized :: String -> String -> [AlignmentType]
  optAlignmentsMemoized str1 str2 = map (\(res1,res2) -> (reverse res1, reverse res2)) $ snd $ opt (length str1) (length str2)
    where
      opt :: Int -> Int -> (Int, [AlignmentType])
      opt i j = table!!i!!j

      table = [ [entry i j | j<-[0..]] | i<-[0..] ]

      entry :: Int -> Int -> (Int, [AlignmentType])
      entry 0 0 = (0, [("","")])
      entry i 0 = (i*scoreSpace, [(take i str1, replicate i '-')])
      entry 0 j = (j*scoreSpace, [(replicate j '-', take j str2)])
      entry i j = (fst (head bestSteps), concat $ map snd bestSteps )
        where
          bestSteps = maximaBy fst [up, left, diag]

          ci = str1!!(i-1)
          cj = str2!!(j-1)

          up = updateScore (updateAlignments (opt (i-1) j) ci '-') (score ci '-')
          left = updateScore (updateAlignments (opt i (j-1)) '-' cj) (score '-' cj)
          diag = updateScore (updateAlignments (opt (i-1) (j-1)) ci cj) (score ci cj)
          updateScore :: (Int, [AlignmentType]) -> Int -> (Int, [AlignmentType])
          updateScore e s = (fst e + s, snd e )
          updateAlignments :: (Int, [AlignmentType]) -> Char -> Char -> (Int, [AlignmentType])
          updateAlignments e h1 h2 = (fst e, attachHeads h1 h2 (snd e))

  outputOptAlignments :: String -> String -> IO()
  outputOptAlignments str1 str2 = do
    let alignments = optAlignmentsMemoized str1 str2
    putStrLn ("There are " ++ show (length alignments) ++  " optimal alignments:")
    mapM_ (\(f,s) -> (putStrLn ("\n"++f++"\n"++s))) alignments
