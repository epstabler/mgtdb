{- file: mgtdbp-dev.hs 
  to exit ghci :quit 
  to load this file in ghci, :l mgtdb.hs 
-}
module MGTDBP_Dev
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
import Parse

{-
  ========== Data and Type Definitions ==========
-}
type Lexicon = (V.Vector String, LexTrees,[Int])
type Movers = V.Vector [LexTree]
type Category = ([LexTree], Movers)--[LexTree] contains children of head
type MvIdx = [Int]
type VMvIdx = V.Vector MvIdx
type VIntFeat = V.Vector [IFeature]

type IdxCat = (PCategory,
               (PMvIndex, (V.Vector PMvIndex)),
               DTuple)
type Deriv = ([String], [IdxCat])

type IQ = PQMin.MinPQueue MvIdx IdxCat
type DQ = PQMax.MaxPQueue Float ([String],IQ, [DNode])

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

scan :: [String] -> [String] -> Movers -> VMvIdx -> DTuple -> [Deriv]-> [Deriv]
scan word input m mx dt sofar =
  if V.and (V.map null m)
  then let (ok, _) = prefixT (word,input) in
    if ok
    then (word,[(([],m),([],mx),dt)]):sofar
    else sofar
  else sofar

merge1 :: LexTrees -> [String] -> [LexTree] -> Int -> IdxCat
          -> (Movers,VMvIdx,VIntFeat) -> [Deriv] -> [Deriv]
merge1 lts input terms i ((_,m),(hx,mx),(ifs,dx,mifs)) empt sofar =
  if terms /= []
  then
    let (empt_m,empt_mx,empt_mifs) = empt in
             ([], [((terms,empt_m),((hx++[0]),empt_mx),
                     ((1, i):ifs, 0:dx, empt_mifs)),
                  ((((V.!) lts i),m),((hx++[1]),mx),
                    ([(0, i)],1:dx,mifs))
                  ]):sofar
  else sofar

merge2 :: LexTrees -> [String] -> [LexTree] -> Int -> IdxCat
          -> (Movers,VMvIdx,VIntFeat) -> [Deriv] -> [Deriv]
merge2 lts input nonterms i ((_,m),(hx,mx),(ifs,dx,mifs)) empt sofar =
  if nonterms /= []
  then
    let (empt_m,empt_mx,empt_mifs) = empt in
        ([],[((nonterms,m), (hx++[1],mx),((1, i):ifs, 0:dx, mifs)),
             ((((V.!) lts i),empt_m),(hx++[0],empt_mx),([(0,i)],1:dx,empt_mifs))
            ]):sofar
  else sofar

merge3 :: [String] -> [LexTree] -> Int -> IdxCat -> Int -> Int
          -> (Movers,VMvIdx,VIntFeat) -> [Deriv] -> [Deriv]
merge3 input terms i ((h,m),(hx,mx),(ifs,dx,mifs)) next stop empt sofar =
  if terms /= [] && next < stop
  then
    let ic = ((h,m),(hx,mx),(ifs,dx,mifs)) in
    let continue = merge3 input terms i ic (next+1) stop empt sofar in
    let (ok,matchingTree) = memberN i ((V.!) m next) in
    if ok
    then
      let ts  = subtreesOf matchingTree in
      let tsx = (V.!) mx next in
      let ifs0 = (V.!) mifs next in
      let (empt_m,empt_mx,empt_mifs) = empt in
      let n = (V.//) m [(next,[])] in
      let nx = (V.//) mx [(next,[])] in
      let nifs = (V.//) mifs [(next,[])] in
      ([],[((terms,empt_m),(hx,empt_mx),((1, i):ifs,0:dx,empt_mifs)),
           ((ts,n),(tsx,nx),((0,i):ifs0,1:dx,nifs))
          ]):continue
    else continue
  else sofar

merge4 :: [String] -> [LexTree] -> Int -> IdxCat -> Int -> Int
          -> (Movers,VMvIdx,VIntFeat) -> [Deriv] -> [Deriv]
merge4 input nonterms i ((h,m),(hx,mx),(ifs,dx,mifs)) next stop empt sofar =
  if nonterms /= [] && next < stop
  then
    let ic = ((h,m),(hx,mx),(ifs,dx,mifs)) in
    let continue = 
            merge4 input nonterms i ic (next+1) stop empt sofar in
    let (ok,matchingTree) = memberN i ((V.!) m next) in
    if ok
    then
      let ts = subtreesOf matchingTree in
      let tsx = (V.!) mx next in
      let ifs0 = (V.!) mifs next in
      let (empt_m,empt_mx,empt_mifs) = empt in
      let n = (V.//) m [(next,[])] in
      let nx = (V.//) mx [(next,[])] in
      let nifs = (V.//) mifs [(next,[])] in
      ([],[((nonterms,n),(hx,nx),((1, i):ifs,0:dx,nifs)),
           ((ts,empt_m),(tsx,empt_mx),((0, i):ifs0,1:dx,empt_mifs))
          ]):continue
    else continue
  else sofar

move1 :: LexTrees -> [String] -> [LexTree] -> Int -> IdxCat -> [Deriv]
  -> [Deriv]
move1 lts input ts i ((_,m),(hx,mx),(ifs,dx,mifs)) sofar =
  if ((V.!) m i) == []
  then
    let n = (V.//) m [(i,((V.!) lts i))] in
    let nx = (V.//) mx [(i,(hx++[0]))] in
    let nifs = (V.//) mifs [(i,[(2, i)])] in
    ([],[((ts,n),((hx++[1]),nx),((3, i):ifs,0:dx,nifs))]):sofar
  else sofar

move2 :: [String] -> [LexTree] -> Int -> IdxCat -> Int -> Int -> [Deriv]
  -> [Deriv]
move2 input ts i ((h,m),(hx,mx),(ifs,dx,mifs)) next stop sofar =
  if next < stop
  then
    let idxcat = ((h,m),(hx,mx),(ifs,dx,mifs)) in
    let continue = move2 input ts i idxcat (next+1) stop sofar in
    let (ok,matchingTree) = memberN i ((V.!) m next) in
    if ok
    then
      let (_,rootFeat) = getNodeValue matchingTree in
      if rootFeat == next || ((V.!) m rootFeat) == []
      then
        let mts = subtreesOf matchingTree in
        let mtsx = (V.!) mx next in
        let ifs0 = (V.!) mifs next in
        let n = (V.//) m [(next,[]),(rootFeat, mts)] in
        let nx = (V.//) mx [(next,[]),(rootFeat, mtsx)] in
        let nifs = (V.//) mifs [(next,[]),(rootFeat, (2, i):ifs0)] in
        ([], [((ts,n),(hx,nx),((3, i):ifs,0:dx,nifs))]):continue
      else continue
    else continue
  else sofar

expands :: Lexicon -> [String] -> IdxCat -> [Deriv] -> [Deriv]
expands _ _ (([],_),_,_) sofar = sofar
expands lex input ((((Leaf label):ls),m),(hx,mx),dt) sofar =
  let sc = scan label input m mx dt sofar in
  expands lex input ((ls,m),(hx,mx),dt) sc
expands (strs,lts,ints) input ((((Node (t,g) bs):gs),m),(hx,mx),dt) sofar
  | t == 1 =
    let (terms,nonterms) = terminalsOf bs in
    let r1 = merge1 lts input    terms g idxcat         empt  sofar in
    let r2 = merge2 lts input nonterms g idxcat         empt  r1    in
    let r3 = merge3     input    terms g idxcat 0 m_len empt  r2    in
    let r4 = merge4     input nonterms g idxcat 0 m_len empt  r3    in
    expands (strs,lts,ints) input ((gs,m),(hx,mx),dt) r4
  | t == 3 =
    let v1 = move1 lts input bs g idxcat         sofar in
    let v2 = move2     input bs g idxcat 0 m_len v1    in
    expands (strs,lts,ints) input ((gs,m),(hx,mx),dt) v2
  | otherwise = error "expands: erroneous selection"
      where h = (Node (t,g) bs):gs
            (_,_,mifs) = dt
            idxcat = ((h,m),(hx,mx),dt)
            m_len = V.length m
            empt_m = V.replicate m_len []
            empt_mx = V.replicate (V.length mx) []
            empt_mifs = V.replicate (V.length mifs) []
            empt = (empt_m, empt_mx, empt_mifs)

-- for all elements in list
-- calculate priority of element
-- then push it into iq
pushICs :: IQ -> [IdxCat] -> IQ
pushICs q [] = q
pushICs q (ic:ics) =
  let new_priority = minIdx ((\x -> case x of (_,y,_) -> y) ic) in
  pushICs (PQMin.insert new_priority ic q) ics
  where minIdx (i,ax) = V.foldl
                         (\x ->(\y -> if y == [] then x else min x y)) i ax

insertNewParses :: [String] -> Float -> Float -> IQ -> DQ -> [DNode]
  -> [Deriv] -> DQ
insertNewParses _ _ _ _ dq _ [] = dq
insertNewParses input p new_p q dq dns ((w,ics):more) = case ics of
  [(([],_), _, (ifs,dx,_))] ->
    let (ok,remainder) = prefixT (w,input) in
    if ok
    then
      let newParse = (remainder,q,(Ld (dx,(w,ifs))):dns) in
      insertNewParses input p  new_p  q  (PQMax.insert p newParse dq) dns more
    else error "insertNewParses: bad parse detected"
  _ ->
    let new_iq = pushICs q ics in
    let new_dns = foldl (\l (_,_,(_,x,_)) -> (Nd x):l) dns ics in
    let newParse = (input,new_iq,new_dns) in
    insertNewParses input p  new_p  q  (PQMax.insert new_p newParse dq) dns more


derive :: Lexicon -> Float -> DQ -> [DNode]
derive lex min dq =
  if PQMax.null dq
  then error "Valid parse could not be found, sentence not accepted."
  else
    let ((p,(input,iq,dns)),pop_dq) = fromMaybe
                                  (error "derive: Extracting from empty dq!")
                                  (PQMax.maxViewWithKey dq) in
    if PQMin.null iq
    then
      if (input == [])
      then dns
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
        then
          derive lex min (insertNewParses input p new_p pop_iq pop_dq dns xs)
        else
          derive lex min pop_dq

parse :: Grammar -> String -> Float -> [String] -> DTree
parse gram start min input =
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
    -- initialize our parser structs
    let ifs = [startCat] in
    let dx = [] in
    let mifs = V.replicate n_strs [] in
    let dt = (ifs,dx,mifs) in
    let ic = ((h,m),([],mx),dt) in
    let iq = PQMin.singleton [] ic in
    let prob = 1.0 in
    let dq = PQMax.singleton prob (input,iq,[(Nd [])]) in
    makeDTreeFromIDTree (fst ltreeP)
                        (dNodes2IDTree (derive (strs,lex_trees,[]) min dq))

dt2t :: DTree -> StrTree
dt2t (L (string,fs)) =  T ("(" ++ (delimitStr " " string) ++
                        "," ++ (stringFs fs) ++ ")") []
dt2t (X (d1,d2)) = T "*" [dt2t d1,dt2t d2]
dt2t (O d) = T "o" [dt2t d]