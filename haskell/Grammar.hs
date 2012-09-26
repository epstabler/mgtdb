module Grammar
       where

import Data.Maybe
import Data.List
import qualified Data.Vector as V --can be gotten from hackage/cabal

{- Grammar
  Definitions and output methods of Features, Grammar
-}

data Feature = Sel String
             | Cat String
             | Neg String
             | Pos String
             deriving (Eq, Read, Show)

type LexItem = ( [String], [Feature] )
type Grammar = [ ([String], [Feature]) ]

data StrTree = T String [StrTree] deriving (Show, Eq)
data LexTree =  EmptyTree
              | Node (Int,Int) [LexTree]
              | Leaf [String]
              deriving (Show, Read, Eq)

type LexTrees = V.Vector [LexTree]

-- Sample Grammars
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

-- copy language with a and b
mgxx = [ 
  ([],[Cat "T", Neg "r", Neg "l"]),
  ([],[Sel "T", Pos "r", Pos "l", Cat "T"]),
  (["a"],[Sel "T", Pos "r", Cat "A", Neg "r"]),
  (["b"],[Sel "T", Pos "r", Cat "B", Neg "r"]),
  (["a"],[Sel "A", Pos "l", Cat "T", Neg "l"]),
  (["b"],[Sel "B", Pos "l", Cat "T", Neg "l"])
  ]

mg1 =[
  ([], [Sel "T", Cat "C"]),
  ([], [Sel "T", Pos "wh", Cat "C"]),
  ([], [Sel "v", Pos "k", Cat "T"]),
  ([], [Sel "V", Sel "D", Cat "v"]),
  (["eats"], [Sel "D", Pos "k", Cat "V"]),
  (["laughs"], [Cat "V"]),
  (["the"], [Sel "N", Cat "D", Neg "k"]),
  (["which"], [Sel "N", Cat "D", Neg "k", Neg "wh"]),
  (["king"], [Cat "N"]),
  (["pie"], [Cat "N"])
  ]

{- simple SOV grammar with wh movement
True Examples
recognize mg1 "C" 0.0000000001 ["the","king","laughs"]
recognize mg1 "C" 0.0000000001 ["the","king","the","pie","eats"]
recognize mg1 "C" 0.0000000001 ["which","pie","the","king","eats"]
False Examples
recognize mg1 "C" 0.0000000001 ["the","king","the","pie","laughs"]
recognize mg1 "C" 0.0000000001 ["the","king","pie","eats"]
recognize mg1 "C" 0.0000000001 ["which","pie","the","king","eats","the","pie"]
-}

mg2 =[
  ([],[Sel "T",Cat "C"]),
  ([],[Sel "T",Pos "wh",Cat "C"]),
  (["PRES"],[Sel "v",Pos "eppD",Cat "T"]),
  ([],[Sel "V",Sel "D",Cat "v"]),        -- "little v" introduces the subject
  (["PREFER"],[Sel "D",Cat "V"]),
  (["KNOW"],[Sel "C",Cat "V"]),
  (["LAUGH"],[Cat "V"]),
  (["which"],[Sel "Num",Cat "D",Neg "wh"]),
  (["SG"],[Sel "N",Cat "Num"]),
  (["PL"],[Sel "N",Cat "Num"]),
  (["KING"],[Cat "N"]),
  (["QUEEN"],[Cat "N"]),
  (["WINE"],[Cat "N"]),
  (["BEER"],[Cat "N"]),             -- Neg "D" should always be available for epp
  (["this"],[Sel "Num",Cat "D"]),   (["this"],[Sel "Num",Cat "D",Neg "eppD"]),
  (["these"],[Sel "Num",Cat "D"]),  (["these"],[Sel "Num",Cat "D",Neg "eppD"]),
  (["MARY"],[Cat "D"]),             (["MARY"],[Cat "D",Neg "eppD"]),
  (["JOHN"],[Cat "D"]),             (["JOHN"],[Cat "D",Neg "eppD"]),
  (["DENMARK"],[Cat "D"]),          (["DENMARK"],[Cat "D",Neg "eppD"]),
  (["SUNDAY"],[Cat "D"]),           (["SUNDAY"],[Cat "D",Neg "eppD"])
  ]

{-
    mg2 Examples
    let s = ["these","PL","KING","PRES","PREFER","this","SG","BEER"] in
    recognize mg2 "C" 0.0000000001 s
    --following are false (they require additional adjunction rules)
    let s = ["these","PL","KING","PRES","PREFER","this","SG","DARK","BEER"] in
    recognize mg2 "C" 0.0000000001 s
    let s = ["these","PL","KING","PRES","PREFER","this","SG","DARK","DARK","BEER"] in
    recognize mg2 "C" 0.0000000001 s
    let s = ["these","PL","KING","PRES","PREFER","this","SG","DARK","BEER","on","SUNDAY"] in
    recognize mg2 "C" 0.0000000001
-}

{-
  ============================== BEGIN FUNCTIONS ===============================
-}

{-
  Returns a printable and readable string of a Feature
-}
featureToStr :: Feature -> String
featureToStr feature = case feature of (Cat a) -> a
                                       (Sel a) -> "=" ++ a
                                       (Neg a) -> "-" ++ a
                                       (Pos a) -> "+" ++ a

-- featureToStr (Pos "wh") 

{-
  Returns a printable and readable string of a LexItem
-}
lexItemToStr :: LexItem -> String
lexItemToStr (ss,fs) =
  (foldl (\x -> (\y -> x ++ " " ++ y)) "" ss) ++ "::" ++
  (foldl (\y -> (\x -> y ++ " " ++ (featureToStr x))) "" fs)

-- lexItemToStr (["drinks"],[Sel "D", Sel "D", Cat "V"])

{-
  Strings the printable, readable,
  strings into a single string,
  which are separated by spaces.
-}
stringFs :: [Feature] -> String
stringFs [] = ""
stringFs feats = foldl1 (\x y -> x ++ " " ++ y) (map featureToStr feats)

-- stringFs [Sel "T", Pos "r", Pos "l", Cat "T"]

{-
  Delimits a list of strings and cats them into a single string.
    For our purposes,
    this returns "e" if the list of strings is empty.
-}
delimitStr :: String -> [String] -> String
delimitStr _ [] = "e"
delimitStr delim strs = foldl1 (\x y -> x ++ delim ++ y) strs

-- delimitStr " Bar! " ["Foo?","Foo?","Foobar."]
-- stringFs (snd (head mg0))

{-
  The grammar in a printable string.
-}
grammarToStr :: Grammar -> String
grammarToStr [] = ""
grammarToStr (h:t) = lexItemToStr(h) ++ "\n" ++ grammarToStr(t) 

-- putStr (grammarToStr(mg0))

{-
  Map the rule (the strings of the features) to Ints using our mapped indexes.
-}
mapRule :: [String] -> [String] -> [Int]
mapRule mappedFStrs fStrs =
  let elemIndex' list elem = fromMaybe (-1) (elemIndex elem list) in
  map (elemIndex' mappedFStrs) fStrs

-- mapRule ["testcase","C","V"] ["V","C"]
-- [Just 2, Just 1]

{-
  Pattern match for LexTree data
-}
getNodeValue :: LexTree -> (Int, Int)
getNodeValue (Node val _) = val
getNodeValue (Leaf _) = (-1,-1)
getNodeValue EmptyTree = (-1,-1)

-- getNodeValue (Node (Just 1, Just 0) [])
-- Returns (Just 1, Just 0)

{-
  Takes a feature and returns a tuple containing
  the corresponding int and the string associated with the feature.
-}
featToIntStr :: Feature -> (Int, String)
featToIntStr (Cat str) = (0,str)
featToIntStr (Sel str) = (1,str)
featToIntStr (Neg str) = (2,str)
featToIntStr (Pos str) = (3,str)

{-
  Takes our mapped strings and a feature
  and returns the string of the feature according to our mapped strings
-}
fOfInts :: [String] -> (Int, Int) -> Feature
fOfInts feats (f, n)
  | length feats < n - 1 = error "fOfInts: invalid index"
  | f == -1 = Cat "."
  | f == 0 = Cat (feats!!n)
  | f == 1 = Sel (feats!!n)
  | f == 2 = Neg (feats!!n)
  | f == 3 = Pos (feats!!n)
  | otherwise = error ("fOfInts: invalid feature type: " ++ show f)

{-
  Take a label/word (LHS),
  list of new mapped features from the rule (RHS),
  and an initial LexTree,
  
  and updates with the new mapped features and the label
  and returns that updated LexTree
  
  Note: extra lines are written for clarity
-}
extendLT :: [String] -> [(Int,Int)] -> LexTree -> LexTree
extendLT label [] EmptyTree = Node (-1,-1) [(Leaf label)] -- create leaf
extendLT label [] (Node val branches) -- leaf the tree
  | elem (Leaf label) branches = Node val branches
  | otherwise = Node val ((Leaf label):branches)
extendLT label (n:ns) EmptyTree =
  let branch = extendLT label ns (Node n []) in
  Node (-1,-1) [branch] -- create tree
extendLT label (n:ns) (Node val []) =
  let branch = extendLT label ns (Node n []) in 
  Node val [branch] -- extend the tree
extendLT label (n:ns) (Node val (b:bs))
  | n == (getNodeValue b) = -- if found, then descend tree
    let descend = extendLT label ns b in
    Node val (descend:bs)
  | otherwise =
    let (Node val2 bs2) = extendLT label (n:ns) (Node val bs) -- else branch
    in (Node val (b:bs2))

{-
  Goes through grammar and constructs a list of all the features (uniquely)
  and fills the LexTree from it by updating with each new rule (LexItem)
  also returns a mapping of strings for the Ints used in the LexTree
-}
makeLT :: Grammar -> [String] -> LexTree -> ([String], LexTree)
makeLT [] mfs tr = (mfs, tr)
makeLT ((label,fs):rs) mfs tree = makeLT rs newMap (extendLT label iRules tree)
  where iStrs  = map featToIntStr (reverse fs)
        strs     = map snd iStrs
        newMap   = union mfs (reverse strs) -- reverse for ordering
        iRules = zip (map fst iStrs) (mapRule newMap strs)

{-
  =========== Some convert to readable forms functions for trees. ==============
-}
{- 
  --lexTreeToTree
  Converts our LexTree into a Tree for print output
-}
lexTreeToTree :: ([String], LexTree) -> StrTree
lexTreeToTree (feats,(Node val branches)) =
  T nodeString (map lexTreeToTree (map (\x -> (feats,x)) branches))
  where nodeString = featureToStr (fOfInts feats val)
lexTreeToTree (feats,(Leaf labels)) = T (foldl (++) "" labels) []
lexTreeToTree (_,EmptyTree) = T "" []

{- convert the grammar to a tree -}
-- lexTreeToTree (makeLT mg0 [] EmptyTree)

{- a pretty printer -}
ppTreeN :: Int -> StrTree -> String
ppTreeN n (T str branches) = "\n" ++ tabs ++
                             "T \"" ++ str ++ "\" [" ++ more ++ "]"
  where tabs = take n (cycle " ")
        more = concat (map (ppTreeN (n+4)) branches)

ppTree :: StrTree -> IO()
ppTree tree = putStr((ppTreeN 0 tree) ++ "\n")

-- ppTree (lexTreeToTree (makeLT mg0 [] EmptyTree))

{-
  ========================= Implementation functions ===========================
-}
-- lextrees is a structure for faster processing with our original lextree
makeILPair :: LexTree -> (Int, [LexTree])
makeILPair (Node (_,v) bs) = (v,bs)
makeILPair _ = error "makeILPair: LexTree is not a Node"

-- this function can be rewritten later
filterPairs :: Int -> [(Int, [LexTree])] -> [LexTree]
filterPairs n [] = []
filterPairs n ((i, ts):more) =
  if i == n
  then ts
  else filterPairs n more

-- future need to store the type
makeLexTrees :: ([String], LexTree) -> LexTrees
makeLexTrees (strMap,(Node _ bs)) = 
  let size = length strMap in
  let idxLexTPairs = map makeILPair bs in
  V.fromList [ filterPairs x idxLexTPairs | x <- [0..(size-1)] ]
makeLexTrees (_,_) = V.empty