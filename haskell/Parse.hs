module Parse
       where
import Grammar
import Data.List
import qualified Data.Vector as V

type PMovers = V.Vector [LexTree]
type PCategory = ([LexTree], PMovers)
type PMvIndex = [Int]

type IFeature = (Int, Int)
type ILexItem = ([String], [IFeature])

data DNode = Nd PMvIndex
           | Ld (PMvIndex,ILexItem)
           deriving (Show, Eq)

type DTuple = ([IFeature], PMvIndex, V.Vector [IFeature])

data IDTree = Li ILexItem
            | Xi (IDTree, IDTree)
            | Oi IDTree
            deriving (Show, Eq)
            
data DTree = L LexItem
           | X (DTree, DTree)
           | O DTree
           deriving (Show, Eq)
               
type SepDNodes = ([[Int]], [([Int], ILexItem)])

splitDNodes :: SepDNodes -> [DNode] -> SepDNodes
splitDNodes (n,t) dns =
  case dns of
  [] -> (sort n, sort t) -- least to greatest
  (Nd i:ts) -> splitDNodes ((reverse i):n,t) ts
  (Ld (i,lex):ts) -> splitDNodes (n,((reverse i),lex):t) ts

child :: (Eq a) => ([a],[a]) -> Bool
child ([], _:[]) = True
child (x:xs, y:ys) = if x == y then child (xs,ys) else False
child _ = False

makeIDTree :: [Int] -> SepDNodes -> (IDTree, [[Int]], [([Int], ILexItem)])
makeIDTree parent splitDNs =
  case splitDNs of 
    (i0:nonterms0, terms0) | child (parent,i0) ->
      (
      let (child0,nts0,ts0) = makeIDTree i0 (nonterms0,terms0) in
      case (nts0,ts0) of
        (i1:nts1,ts1) | child (parent,i1) ->
          let (child2,nts2,ts2) = makeIDTree i1 (nts1,ts1) in
          (Xi (child0,child2), nts2, ts2)
        _ -> (Oi child0, nts0, ts0)
      )
    (nonterms0, (j,(s,f)):terms0) | (parent == j) ->
      (
      (Li (s,f), nonterms0, terms0)
      )
    _ -> error "makeIDTree failed"

dNodes2IDTree :: [DNode] -> IDTree
dNodes2IDTree dns = let (nts,ts) = splitDNodes ([],[]) dns in
                    let (dtree,nts_ck,ts_ck) =
                                      makeIDTree (head nts) ((tail nts),ts) in
                    if nts_ck == [] && ts_ck == []
                    then dtree
                    else error "dNodes2IDTree: invalid idtree"
                    
-- clean this pattern match up later...
makeDTreeFromIDTree :: [String] -> IDTree -> DTree
makeDTreeFromIDTree strs node = case node of
  (Li (ss,ifs)) -> L (ss, map (fOfInts strs) ifs)
  (Oi t)        -> O (makeDTreeFromIDTree strs t)
  (Xi (t1,t2))  -> X (makeDTreeFromIDTree strs t1, makeDTreeFromIDTree strs t2)