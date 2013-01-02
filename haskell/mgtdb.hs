{- file: mgtdb.hs 
  to exit ghci :quit 
  to load this file in ghci, :l mgtdb.hs 
-}
module MGTDB
       where
import Data.Maybe
import qualified Data.List as L -- for clarity
import qualified Data.Vector as V --can be gotten from hackage/cabal

{-
  Fast Priority Queue implementation from
  http://hackage.haskell.org/package/pqueue/
  version 1.2.0
  note these following libraries do not specify
  what will be in the case of an ordering tie
  -- if specific elements are needed then
  it will have to be enforced through insertion
  
  however it should not matter,
  for iq each index is unique and dq the parses are independent
-}
import qualified Data.PQueue.Prio.Min as PQMin
import qualified Data.PQueue.Prio.Max as PQMax

-- Our utility modules
import Grammar

{-
  ========== Data and Type Definitions ==========
-}
type Lexicon = (V.Vector String, LexTrees,[Int])
type Movers = V.Vector [LexTree]
type Category = ([LexTree], Movers)--[LexTree] contains children of head
type MvIdx = [Int]
type VMvIdx = V.Vector MvIdx
type IdxCat = (Category, (MvIdx, (VMvIdx)))
type Deriv = ([String], [IdxCat])

-- Queue Type Definitions
type IQ = PQMin.MinPQueue MvIdx IdxCat
type DQ = PQMax.MaxPQueue Float ([String],IQ)

{-
  ========== Function Definitions ==========
-}
subtreesOf :: LexTree -> [LexTree]
subtreesOf (Node _ bs) = bs
subtreesOf _ = error "error: subtreesOf called on a non-Node"

terminalsOf :: [LexTree] -> ([LexTree], [LexTree])
terminalsOf [] = ([],[])
terminalsOf (lt:ltx) =
  case lt of EmptyTree -> terminalsOf ltx
             (Leaf _) -> let (terms,nonterms) = terminalsOf ltx in
                         (lt:terms,nonterms)
             (Node _ _) -> let (terms,nonterms) = terminalsOf ltx in
                           (terms,lt:nonterms)

prefixT :: ([String],[String]) -> (Bool, [String])
prefixT ([], remainder) = (True, remainder)
prefixT ((x:xs),(y:ys)) = if x == y then prefixT (xs,ys) else (False, [])
prefixT _ = (False, [])

memberN :: Int -> [LexTree] -> (Bool, LexTree)
memberN f (lt:ltx) =
  case lt of (Leaf _) -> memberN f ltx
             (Node (_,g) _) -> if g == f then (True, lt) else memberN f ltx
memberN _ [] = (False, EmptyTree)

scan :: [String] -> [String] -> Movers -> VMvIdx -> [Deriv]
  -> [Deriv]
scan word input m mx sofar =
  if V.and (V.map L.null m) -- checks if all movers are empty
  then let (ok, remainder) = prefixT (word,input) in
    if ok
    then (remainder,[(([],m),([],mx))]):sofar
    else sofar
  else sofar

merge1 :: LexTrees -> [String] -> [LexTree] -> Int -> IdxCat -> Movers -> VMvIdx
  -> [Deriv] -> [Deriv]
merge1 lts input terms i ((_,m),(hx,mx)) empt_m empt_mx sofar =
  if terms /= []
  then
      (input, [((terms,empt_m),((hx++[0]),empt_mx)),
               ((((V.!) lts i),m),((hx++[1]),mx))
               ]):sofar
  else sofar

merge2 :: LexTrees -> [String] -> [LexTree] -> Int -> IdxCat -> Movers ->VMvIdx
  -> [Deriv] -> [Deriv]
merge2 lts input nonterms i ((_,m),(hx,mx)) empt_m empt_mx sofar =
  if nonterms /= []
  then
    ((input, [((nonterms,m),(hx++[1],mx)),
              ((((V.!) lts i),empt_m),(hx++[0],empt_mx))
              ]):sofar)
  else sofar

merge3 :: [String] -> [LexTree] -> Int -> IdxCat -> Int -> Int -> Movers
  -> VMvIdx -> [Deriv] -> [Deriv]
merge3 input terms i ((h,m),(hx,mx)) next stop empt_m empt_mx sofar =
  if terms /= [] && next < stop
  then
    let ic = ((h,m),(hx,mx)) in
    let continue = merge3 input terms i ic (next+1) stop empt_m empt_mx sofar in
    let (ok,matchingTree) = memberN i ((V.!) m next) in
    if ok
    then
      let ts  = subtreesOf matchingTree in
      let tsx = (V.!) mx next in
      let n = (V.//) m [(next,[])] in
      let nx = (V.//) mx [(next,[])] in
      (input,[((terms,empt_m),(hx,empt_mx)),
              ((ts,n),(tsx,nx))]):continue
    else continue
  else sofar

merge4 :: [String] -> [LexTree] -> Int -> IdxCat -> Int -> Int -> Movers
  -> VMvIdx -> [Deriv] -> [Deriv]
merge4 input nonterms i ((h,m),(hx,mx)) next stop empt_m empt_mx sofar =
  if nonterms /= [] && next < stop
  then
    let ic = ((h,m),(hx,mx)) in
    let continue =
                merge4 input nonterms i ic (next+1) stop empt_m empt_mx sofar in
    let (ok,matchingTree) = memberN i ((V.!) m next) in
    if ok
    then
      let ts = subtreesOf matchingTree in
      let tsx = (V.!) mx next in
      let n = (V.//) m [(next,[])] in
      let nx = (V.//) mx [(next,[])] in
      (input,[((nonterms,n),(hx,nx)),
              ((ts,empt_m),(tsx,empt_mx))]):continue
    else continue
  else sofar

move1 :: LexTrees -> [String] -> [LexTree] -> Int -> IdxCat -> [Deriv]
  -> [Deriv]
move1 lts input ts i ((_,m),(hx,mx)) sofar =
  if ((V.!) m i) == []
  then
    let n = (V.//) m [(i,((V.!) lts i))] in
    let nx = (V.//) mx [(i,(hx++[0]))] in
    (input,[((ts,n),((hx++[1]),nx))]):sofar
  else sofar

move2 :: [String] -> [LexTree] -> Int -> IdxCat -> Int -> Int -> [Deriv]
  -> [Deriv]
move2 input ts i ((h,m),(hx,mx)) next stop sofar =
  if next < stop
  then
    let continue = move2 input ts i ((h,m),(hx,mx)) (next+1) stop sofar in
    let (ok,matchingTree) = memberN i ((V.!) m next) in
    if ok
    then
      let (_,rootFeat) = getNodeValue matchingTree in
      if rootFeat == next || ((V.!) m rootFeat) == []
      then
        let mts = subtreesOf matchingTree in
        let mtsx = (V.!) mx next in
        let n = (V.//) m [(next,[]),(rootFeat,mts)] in
        let nx = (V.//) mx [(next,[]),(rootFeat,mtsx)] in
        (input,[((ts,n),(hx,nx))]):continue
      else continue
    else continue
  else sofar

expands :: Lexicon -> [String] -> IdxCat -> [Deriv] -> [Deriv]
expands _ _ (([],_),_) sofar = sofar
expands lex input ((((Leaf label):ls),m),(hx,mx)) sofar = -- our scan step
  let sc = scan label input m mx sofar in
  expands lex input ((ls,m),(hx,mx)) sc
expands (strs,lts,ints) input ((((Node (t,g) bs):gs),m),(hx,mx)) sofar
  | t == 1 =
    let (terms,nonterms) = terminalsOf bs in
    let r1 = merge1 lts input    terms g idxcat         empt_m empt_mx sofar in
    let r2 = merge2 lts input nonterms g idxcat         empt_m empt_mx r1    in
    let r3 = merge3     input    terms g idxcat 0 m_len empt_m empt_mx r2    in
    let r4 = merge4     input nonterms g idxcat 0 m_len empt_m empt_mx r3    in
    expands (strs,lts,ints) input ((gs,m),(hx,mx)) r4
  | t == 3 =
    let v1 = move1 lts input bs g idxcat         sofar in
    let v2 = move2     input bs g idxcat 0 m_len v1    in
    expands (strs,lts,ints) input ((gs,m),(hx,mx)) v2
  | otherwise = error "expands: erroneous selection"
      where h = (Node (t,g) bs):gs
            idxcat = ((h,m),(hx,mx))
            m_len = V.length m
            empt_m = V.replicate (V.length m) []
            empt_mx = V.replicate (V.length mx) []

-- for all elements in list
-- calculate priority of element
-- then push it into iq
pushICs :: IQ -> [IdxCat] -> IQ
pushICs q [] = q
pushICs q (ic:ics) =
  let new_priority = minIdx (snd ic) in
  pushICs (PQMin.insert new_priority ic q) ics
  where minIdx (i,ax) = V.foldl
                         (\x ->(\y -> if y == [] then x else min x y)) i ax

insertNewDerivs :: Float -> Float -> IQ -> DQ -> [Deriv] -> DQ
insertNewDerivs _ _ _ dq [] = dq
insertNewDerivs p new_p q dq ((input,ics):more) = case ics of
  [(([],_),_)] -> let newParse = (input,q) in
    insertNewDerivs p  new_p  q  (PQMax.insert p newParse dq) more
  _ -> let newParse = (input,(pushICs q ics)) in
    insertNewDerivs p  new_p  q  (PQMax.insert new_p newParse dq) more

derive :: Lexicon -> Float -> DQ -> Bool
derive lex min dq =
  if PQMax.null dq
  then False
  else
    let ((p,(input,iq)),pop_dq) = fromMaybe
                                  (error "derive: Extracting from empty dq!")
                                  (PQMax.maxViewWithKey dq) in
    if PQMin.null iq
    then
      if (input == [])
      then True
      else derive lex min pop_dq
    else
      let (ic,pop_iq) = fromMaybe
                        (error "derive: Extracting from empty iq!")
                        (PQMin.minView iq) in
      let xs = expands lex input ic [] in
      if L.null xs
      then
        derive lex min pop_dq
      else
        let new_p = p / (fromIntegral (L.length xs)) in
        if new_p > min
        then let new_parses = insertNewDerivs p new_p pop_iq pop_dq xs in
          derive lex min new_parses
        else derive lex min pop_dq

-- Find any derivation
recognize :: Grammar -> String -> Float -> [String] -> Bool
recognize gram start min input =
  let ltreeP    = makeLT gram [] EmptyTree in
  let strs      = V.fromList (fst ltreeP) in
  let lex_trees = makeLexTrees ltreeP in -- "lex_trees" is for faster lt access
  let startCat  = (0, (head (mapRule (fst ltreeP) [start]))) in
  let startCatX = snd startCat in
  if startCatX > (V.length lex_trees) - 1
  then
    error "recognize: index too large for lex_trees"
  else
    let h = (V.!) lex_trees startCatX in
    let n_strs = V.length strs in
    let m = V.replicate n_strs [] in
    let mx = V.replicate n_strs [] in
    let ic = ((h,m),([],mx)) in
    let iq = PQMin.singleton [] ic in
    let prob = 1.0 in
    let dq = PQMax.singleton prob (input,iq) in
    derive (strs,lex_trees,[]) min dq