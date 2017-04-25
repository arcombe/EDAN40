import Data.Char
import Data.Maybe

-- Takes 2 tuples. First tuple with 2 functions and second with values.
-- Returns a tuple where the values been in the functions.
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

-- Takes a function and a value. Returns Nothing if the value is Nothing
-- else the value is put in the function.
mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

-- Takes 2 values. Returns x if first value i Nothing otherwise the first
-- value.
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

-- Trys to do the function f with value x. If not succesful returns x.
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

-- If the function is equal to the parameter return x. Else run the function
-- with the new value.
fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

-- Picks an element from xs
pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs

-- The function substitute wildcard t s replaces each occurrence of the
-- element wildcard in the list t with the list s. For example:
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute w t s = concat [ if a == w then s else [a] | a <- t]

-- The function match wildcard p s tries to match the two lists p and s.
-- The list p is considered a pattern which may contain elements equal to wildcard.
-- The list s may not contain any wildcards. The pattern p is said to match the
-- list s if every element in p matches corresponding elements in s.
-- A non-wildcard matches a single element in s if they are equal and
-- a wildcard matches an arbitrarily long (non-empty) sublist.
-- If the two lists match, the function returns Just r where r is the
-- sublist in s which matches the first occurence of wildcard in p.
-- If the lists don't match, the result is Nothing. Here are some examples:
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match w [] [] = Just []
match w [] _ = Nothing
match w _ [] = Nothing
match w (t:ts) (s:ss)
    | w == t = orElse (longerWildcardMatch (t:ts) (s:ss)) (singleWildcardMatch (t:ts) (s:ss))
    | t == s = match w ts ss
    | otherwise = Nothing

singleWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (t:ts) (s:ss) = mmap (const [s]) (match t ts ss)

longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
longerWildcardMatch (t:ts) (s:ss) = mmap (s:) (match t (t:ts) ss)

-- f: function
-- t: transform
-- k, m: tuple with the replacement
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
--transformationApply w f t (k, m) =  mmap (substitute w m ) (f (match w k t) )
transformationApply w f t (k, m) =  mmap (substitute w m ) (match w k t)

transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply w f (k:ks) t
    | trans == Nothing = transformationsApply w f ks t
    | otherwise = trans
    where trans = transformationApply w f t k

-- frenchPresentation = ("My name is *", "Je m'appelle *")

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]

reflections =
    [ ("am",     "are"),
      ("was",    "were"),
      ("i",      "you"),
      ("i'm",    "you are"),
      ("i'd",    "you would"),
      ("i've",   "you have"),
      ("i'll",   "you will"),
      ("my",     "your"),
      ("me",     "you"),
      ("are",    "am"),
      ("you're", "i am"),
      ("you've", "i have"),
      ("you'll", "i will"),
      ("your",   "my"),
      ("yours",  "mine"),
      ("you",    "me")
    ]

reflect :: Phrase -> Phrase
reflect = map (try (flip lookup reflections) )

-- reflect ["i", "will", "never", "see", "my", "reflection", "in", "your", "eyes"]
