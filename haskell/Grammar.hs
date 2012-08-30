module Grammar
       where

import Data.List
import Data.Maybe
import Debug.Trace

{- Grammar
  Definitions of features, grammar, and output
  copy pasted, will neeed to improve
-}
data Feature = Sel String | Cat String | Neg String | Pos String deriving (Eq, Read, Show)

type LexItem = ([String],[Feature])
type Grammar = [([String],[Feature])]

mg0 = [
  ([],[Sel "V", Cat "C"]),
  ([],[Sel "V", Pos "wh", Cat "C"]),
  (["the"],[Sel "N", Cat "D"]),
  (["which"],[Sel "N", Cat "D", Neg "wh"]),
  (["king"],[Cat "N"]),
  (["queen"],[Cat "N"]),
  (["wine"],[Cat "N"]),
  (["beer"],[Cat "N"]),
  (["knows"],[Sel "C", Sel "D", Cat "V"]),
  (["says"],[Sel "C", Sel "D", Cat "V"]),
  (["prefers"],[Sel "D", Sel "D", Cat "V"]),
  (["drinks"],[Sel "D", Sel "D", Cat "V"])
  ]

mgxx = [ 
  ([],[Cat "T", Neg "r", Neg "l"]),
  ([],[Sel "T", Pos "r", Pos "l", Cat "T"]),
  (["a"],[Sel "T", Pos "r", Cat "A", Neg "r"]),
  (["b"],[Sel "T", Pos "r", Cat "B", Neg "r"]),
  (["a"],[Sel "A", Pos "l", Cat "T", Neg "l"]),
  (["b"],[Sel "B", Pos "l", Cat "T", Neg "l"])
  ]

btfyFeature :: Feature -> String
btfyFeature (Cat a) = a
btfyFeature (Sel a) = "=" ++ a
btfyFeature (Neg a) = "-" ++ a
btfyFeature (Pos a) = "+" ++ a

-- btfyFeature (Pos "wh") 

btfyLexItem :: LexItem -> String
btfyLexItem (ss,fs) =
  (foldl (\x -> (\y -> x ++ " " ++ y)) "" ss) ++ "::" ++
  (foldl (\y -> (\x -> y ++ " " ++ (btfyFeature x))) "" fs)

-- btfyLexItem (["drinks"],[Sel "D", Sel "D", Cat "V"])

spacedStr2 :: [String] -> String
spacedStr2 [] = "e"
spacedStr2 strs = foldl1 (\x y -> x ++ " " ++ y) strs

-- list of features into a string
stringFs :: [Feature] -> String
stringFs [] = ""
stringFs feats = foldl1 (\x y -> x ++ " " ++ y) (map btfyFeature feats)

-- stringFs (snd (head mg0))

{- write a function to list all the elements of a grammar, using btfyLexitem -}
btfyGrammar :: Grammar -> String
btfyGrammar [] = ""
btfyGrammar (h:t) = btfyLexItem(h) ++ "\n" ++ btfyGrammar(t) 

-- putStr (btfyGrammar(mg0))

data StrTree = T String [StrTree] deriving Show

t = T ">" [T "Mary" [],T "<" [T "praises" [],T "John" []]]

data LexTree =  EmptyTree | Node (Maybe Int,Maybe Int) [LexTree] | Leaf [String] deriving (Show, Read, Eq)

instance Ord LexTree where
  Node _ ts1 <= Node _ ts2 = length ts1 <= length ts2
  Leaf _ <= Node _ _ = True
  Node _ _ <= Leaf _ = False  
  Leaf str1 <= Leaf str2 = str1 <= str2
  EmptyTree <= _ = True
  _ <= EmptyTree = False

{-
  Utility function using built in elemIndex to be able to be used with map.
-}
elemIndex' :: (Eq a) => [a] -> a -> (Maybe Int)
elemIndex' list elem = elemIndex elem list

{-
  Map the rule (the strings of the features) to Maybe Int values using our mapped indexes.
-}
mapRule :: [String] -> [String] -> [Maybe Int]
mapRule mappedFStrs fStrs = map (elemIndex' mappedFStrs) fStrs

-- mapRule ["testcase","C","V"] ["V","C"]
-- Returns [Just 2, Just 1]

{-
  Pattern match for LexTree Maybe Int Features
-}
getNodeValue :: LexTree -> (Maybe Int, Maybe Int)
getNodeValue EmptyTree = (Nothing, Nothing)
getNodeValue (Leaf _) = (Nothing, Nothing)
getNodeValue (Node val _) = val

-- getNodeValue (Node (Just 1, Just 0) [])
-- Returns (Just 1, Just 0)

{-
  Takes a feature and returns the corresponding maybe int and the string associated with the feature.
-}
featToMIntString :: Feature -> (Maybe Int, String)
featToMIntString (Cat str) = (Just 0,str)
featToMIntString (Sel str) = (Just 1,str)
featToMIntString (Neg str) = (Just 2,str)
featToMIntString (Pos str) = (Just 3,str)

{-
  Take a label/word (LHS), list of mapped features from the rule (RHS), and a LexTree
  to update the LexTree and returns that updated LexTree
-}
extendLexTree :: [String] -> [(Maybe Int,Maybe Int)] -> LexTree -> LexTree
extendLexTree label [] EmptyTree = Node (Nothing,Nothing) [(Leaf label)] -- create tree
extendLexTree label [] (Node val branches) -- leaf the tree
  | elem (Leaf label) branches = Node val branches
  | otherwise = Node val ((Leaf label):branches)
extendLexTree label (n:ns) EmptyTree = Node (Nothing,Nothing) [extendLexTree label ns (Node n [])] -- create tree
extendLexTree label (n:ns) (Node val []) = Node val [extendLexTree label ns (Node n [])] -- extend the tree
extendLexTree label (n:ns) (Node val (b:bs)) -- decide to branch or extend depending if we find existing feature
  | n == (getNodeValue b) = Node val ((extendLexTree label ns b):bs) -- if found, then dive
  | otherwise = let (Node val2 bs2) = extendLexTree label (n:ns) (Node val bs)
  in (Node val (b:bs2))

{-
  Goes through grammar and constructs a list of all the features (uniquely) and fills the LexTree from it
-}
makeLexTree :: Grammar -> [String] -> LexTree -> ([String], LexTree)
makeLexTree [] mfs tr = (mfs, tr)
makeLexTree ((label,fs):rules) mfs tree = makeLexTree rules newMap (extendLexTree label ruleInMints tree)
  where mIntStrings = map featToMIntString (reverse fs)
        newMap = union mfs (reverse (map snd mIntStrings)) -- reverse is for debugging
        ruleInMints = zip (map fst mIntStrings) (mapRule newMap (map snd mIntStrings))

{-
  Returns the string of the feature according to our mapping
-}
fOfInts :: [String] -> (Maybe Int, Maybe Int) -> Feature
fOfInts feats (Just f, Just n)
  | length feats < n - 1 = error "fOfInts: invalid index"
  | f == 0 = Cat ((!!) feats n)
  | f == 1 = Sel ((!!) feats n)
  | f == 2 = Neg ((!!) feats n)
  | f == 3 = Pos ((!!) feats n)
  | otherwise = error ("fOfInts: invalid feature type: " ++ show f)

{-
  Returns the string of the feature according to our mapping
-}
mIntsToStr :: [String] -> (Maybe Int, Maybe Int) -> String
mIntsToStr _ (_, Nothing) = ""
mIntsToStr _ (Nothing, _) = ""
mIntsToStr feats (Just f, Just n)
  | f == 0 = btfyFeature (Cat ((!!) feats n))
  | f == 1 = btfyFeature (Sel ((!!) feats n))
  | f == 2 = btfyFeature (Neg ((!!) feats n))
  | f == 3 = btfyFeature (Pos ((!!) feats n))

{- 
  Converts our LexTree into a Tree for print output
-}
lexTreeToTree :: ([String], LexTree) -> StrTree
lexTreeToTree (feats,(Node val branches)) =
  T nodeString (map lexTreeToTree (map (\x -> (feats,x)) branches))
  where nodeString = mIntsToStr feats val
lexTreeToTree (feats,(Leaf labels)) = T (foldl (++) "" labels) []
lexTreeToTree (_,EmptyTree) = T "" []

{- convert the grammar to a tree -}
-- lexTreeToTree (makeLexTree mg0 [] EmptyTree)
{- write a pretty printer, port tktree from ocaml -}

ppTreeN :: Int -> StrTree -> String
ppTreeN n (T str branches) = "\n" ++ tabs ++
                             "T \"" ++ str ++ "\" [" ++ more ++ "]"
  where tabs = take n (cycle " ")
        more = concat (map (ppTreeN (n+4)) branches)

ppTree :: StrTree -> IO()
ppTree tree = putStr((ppTreeN 0 tree) ++ "\n")

-- ppTree (lexTreeToTree (makeLexTree mg0 [] EmptyTree))