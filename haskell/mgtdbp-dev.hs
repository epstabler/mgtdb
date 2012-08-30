{- file: mgtdb.hs 
  to exit ghci :quit 
  to load this file in ghci, :l mgtdb.hs 
-}


{- TODO: LOTS OF CLEANUP -}
module MGTDBP
       where
--import qualified Data.PSQueue as PQ
import Data.List
import Data.Maybe
import Debug.Trace
import Grammar
{- the following can and should be installed through cabal -}
--
import qualified Data.Vector as V
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

type LexTrees = V.Vector [LexTree]

type Lexicon = (V.Vector String, LexTrees,[Int])

--type movers = lexTree list array (* NB: same as lexArray *)
type Movers = V.Vector [LexTree]

--type cat = lexTree list * movers
--(* intuitively:lexTree list=children of head *)
type Category = ([LexTree], Movers)

--type ix = int list
type MvIndex = [Int]

type IFeature = (Maybe Int, Maybe Int)
type ILexItem = ([String], [IFeature])

-- type dnode = Nd of ix
--            | Ld of (ix * iLexItem);; (* for each step of each derivation *)
data DNode = Nd MvIndex | Ld (MvIndex,ILexItem) deriving (Show, Eq)
-- type dtuple = (ifeature list * ix * ifeature list array)
type DTuple = ([IFeature], MvIndex, V.Vector [IFeature])
{- type idtree =  Li of string list * ifeature list
                | Xi of idtree * idtree
                | Oi of idtree;;-}

data IDTree = Li ILexItem | Xi (IDTree, IDTree) | Oi IDTree deriving (Show, Eq)

data DTree = L LexItem | X (DTree, DTree) | O DTree deriving (Show, Eq)
{- type derivationTree = 
    L of string list * feature list
  | X of derivationTree * derivationTree
  | O of derivationTree;;
-}

--type iCat = cat * (ix * ix array) * dtuple
type IdxCat = (Category,
               (MvIndex, (V.Vector MvIndex)),
               DTuple)


type Parse = ([String], [IdxCat])

type IQ = PQMin.MinPQueue MvIndex IdxCat
type DQ = PQMax.MaxPQueue Float ([String],IQ, [DNode])

type SplitDNs = ([[Int]], [([Int], ILexItem)])

splitDNodes :: SplitDNs -> [DNode] -> SplitDNs
splitDNodes (n,t) dns = --trace ("split: " ++ show n ++ show t ++ show dns)
  (case dns of
  [] -> (sort n, sort t) -- least to greatest
  ((Nd i):ts) -> splitDNodes ((reverse i):n,t) ts
  ((Ld (i,lex)):ts) -> splitDNodes (n,(((reverse i),lex):t)) ts
  )

child :: (Eq a) => ([a],[a]) -> Bool
child ([], (_:[])) = True
child ((x:xs),(y:ys)) = if x == y then child (xs,ys) else False
child _ = False

makeIDTree :: [Int] -> SplitDNs -> (IDTree, [[Int]], [([Int], ILexItem)])
makeIDTree parent ([],(j,(s,f)):terms) = if parent == j
                                          then (Li (s,f),[],terms)
                                          else error "makeIDTree"
makeIDTree parent (i0:nonterms,(j,(s,f)):terms)
  | child (parent,i0)
                = let (child0,nts0,ts0) = makeIDTree i0 (nonterms,all_terms) in
                  case nts0 of (i1:nts) | child (parent,i1) -> let (child1,nts1,ts1) = makeIDTree i1 (nts,ts0) in
                                            ((Xi (child0,child1)),nts1,ts1)
                                        | otherwise -> ((Oi child0),nts0,ts0)
                               [] -> (Oi child0,nts0,ts0)
  | parent == j = (Li (s,f), all_nonterms, terms)
  | otherwise = error "makeIDTree"
  where all_nonterms = (i0:nonterms)
        all_terms = ((j,(s,f)):terms)
makeIDTree _ _ = error "makeIDTree"

dNodes2IDTree :: [DNode] -> IDTree
dNodes2IDTree dns = let (nts,ts) = splitDNodes ([],[]) dns in
                    let (dt,nts_ck,ts_ck) =
                                      makeIDTree (head nts) ((tail nts),ts) in
                    if nts_ck == [] && ts_ck == []
                    then dt
                    else error "dNodes2IDTree: invalid idtree"

makeILPair :: LexTree -> (Int, [LexTree])
makeILPair (Node (_,v) bs) = ((fromMaybe (-1) v),bs)
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

subtreesOf :: LexTree -> [LexTree]
subtreesOf (Node _ bs) = bs
subtreesOf _ = error "error: subtreesOf called on a non-Node"

terminalsOf :: [LexTree] -> ([LexTree],[LexTree])
terminalsOf [] = ([],[])
terminalsOf ((EmptyTree):moreLTs) = terminalsOf moreLTs
terminalsOf ((Leaf label):moreLTs) =
  let (terms,nonterms) = terminalsOf moreLTs in (((Leaf label):terms),nonterms)
terminalsOf ((Node i ns):moreLTs) =
  let (terms,nonterms) = terminalsOf moreLTs in (terms,((Node i ns):nonterms))

prefixT :: ([String],[String]) -> (Bool, [String])
prefixT ([], remainder) = (True, remainder)
prefixT ((x:xs),(y:ys))
  | x == y = prefixT (xs,ys)
  | otherwise = (False, [])
prefixT _ = (False, [])

memberN :: Maybe Int -> [LexTree] -> (Bool, LexTree)
memberN f ((Node (t,g) bs):more)
  | g == f = (True, (Node (t,g) bs))
  | otherwise = memberN f more
memberN f ((Leaf _):more) = memberN f more
memberN _ [] = (False, EmptyTree)

scan :: [String] -> [String] -> LexTrees
        -> V.Vector MvIndex -> DTuple -> [Parse]-> [Parse]
scan word input m mx dt sofar =
  if V.and (V.map null m)
  then let (ok, _) = prefixT (word,input) in
    if ok
    then (word,[(([],m),([],mx),dt)]):sofar
    else sofar
  else sofar

merge1 :: LexTrees -> [String] -> [LexTree] -> Int -> IdxCat -> [Parse]
  -> [Parse]
merge1 lts input terms i ((_,m),(hx,mx),(ifs,dx,mifs)) sofar =
  if terms /= []
  then
    if i > (V.length lts) - 1
    then error ("merge1: index too large for lts" ++ "\n" ++ show i)
    else
      let empty_m = V.replicate (V.length m) [] in
      let empty_mx = V.replicate (V.length mx) [] in
      let empty_mifs = V.replicate (V.length mifs) [] in
               ([], [((terms,empty_m),
                      ((hx++[0]),empty_mx),
                      ((Just 1, Just i):ifs, 0:dx, empty_mifs)),
                    ((((V.!) lts i),m),
                    ((hx++[1]),mx),
                    ([(Just 0, Just i)],1:dx,mifs))
                    ]):sofar
  else sofar

merge2 :: LexTrees -> [String] -> [LexTree] -> Int -> IdxCat
  -> [Parse] -> [Parse]
merge2 lts input nonterms i ((_,m),(hx,mx),(ifs,dx,mifs)) sofar =
  if nonterms /= []
  then
   if i > (V.length lts) - 1
     then error "merge2: index too large for lts"
     else 
       let empty_m = V.replicate (V.length m) [] in
       let empty_mx = V.replicate (V.length mx) [] in
       let empty_mifs = V.replicate (V.length mifs) [] in
          ([],[((nonterms,m), (hx++[1],mx),
                ((Just 1, Just i):ifs, 0:dx, mifs)),
               ((((V.!) lts i),empty_m),(hx++[0],empty_mx),
                ([(Just 0, Just i)],1:dx,empty_mifs))
              ]):sofar
  else sofar

merge3 :: [String] -> [LexTree] -> Int -> IdxCat -> Int -> Int -> [Parse]
  -> [Parse]
merge3 input terms i ((h,m),(hx,mx),(ifs,dx,mifs)) next stop sofar =
  if terms /= [] && next < stop
  then
    let continue = merge3 input terms i ((h,m),
                                  (hx,mx),(ifs,dx,mifs)) (next+1) stop sofar in
    if next > (V.length m) - 1 || next > (V.length mx) - 1
    then error "merge3: index too large for m or mx"
    else
      let (ok,matchingTree) = memberN (Just i) ((V.!) m next) in
      if ok
      then
        let ts  = subtreesOf matchingTree in
        let tsx = (V.!) mx next in
        let ifs0 = (V.!) mifs next in
        let empty_m = V.replicate (V.length m) [] in
        let empty_mx = V.replicate (V.length mx) [] in
        let empty_mifs = V.replicate (V.length mifs) [] in
        let n = (V.//) m [(next,[])] in
        let nx = (V.//) mx [(next,[])] in
        let nifs = (V.//) mifs [(next,[])] in
        ([],[((terms,empty_m),(hx,empty_mx),
              ((Just 1, Just i):ifs,0:dx,empty_mifs)),
             ((ts,n),(tsx,nx),((Just 0,Just i):ifs0,1:dx,nifs))
            ]):continue
      else continue
  else sofar

merge4 :: [String] -> [LexTree] -> Int -> IdxCat -> Int -> Int -> [Parse]
  -> [Parse]
merge4 input nonterms i ((h,m),(hx,mx),(ifs,dx,mifs)) next stop sofar =
  if nonterms /= [] && next < stop
  then
    let continue = 
            merge4 input nonterms i ((h,m),
                                  (hx,mx),(ifs,dx,mifs)) (next+1) stop sofar in
    if next > (V.length m) - 1 || next > (V.length mx) - 1
    then error "merge4: index too large for m or mx"
    else
      let (ok,matchingTree) = memberN (Just i) ((V.!) m next) in
      if ok
      then
        let ts = subtreesOf matchingTree in
        let tsx = (V.!) mx next in
        let ifs0 = (V.!) mifs next in
        let empty_m = V.replicate (V.length m) [] in
        let empty_mx = V.replicate (V.length mx) [] in
        let empty_mifs = V.replicate (V.length mifs) [] in
        let n = (V.//) m [(next,[])] in
        let nx = (V.//) mx [(next,[])] in
        let nifs = (V.//) mifs [(next,[])] in
        ([],[((nonterms,n),(hx,nx),((Just 1, Just i):ifs,0:dx,nifs)),
             ((ts,empty_m),(tsx,empty_mx),
              ((Just 0, Just i):ifs0,1:dx,empty_mifs))
            ]):continue
      else continue
  else sofar

move1 :: LexTrees -> [String] -> [LexTree] -> Int -> IdxCat -> [Parse]
  -> [Parse]
move1 lts input ts i ((_,m),(hx,mx),(ifs,dx,mifs)) sofar =
  if i > (V.length lts) || i > (V.length m) - 1
  then error "move1: index too large for lts or m"
  else
    if ((V.!) m i) == []
    then
      let n = (V.//) m [(i,((V.!) lts i))] in
      let nx = (V.//) mx [(i,(hx++[0]))] in
      let nifs = (V.//) mifs [(i,[(Just 2, Just i)])] in
      ([],[((ts,n),((hx++[1]),nx),((Just 3, Just i):ifs,0:dx,nifs))]):sofar
    else sofar

move2 :: [String] -> [LexTree] -> Int -> IdxCat -> Int -> Int -> [Parse]
  -> [Parse]
move2 input ts i ((h,m),(hx,mx),(ifs,dx,mifs)) next stop sofar =
  if next < stop
  then
    if next > (V.length m) - 1 || next > (V.length mx) - 1
    then error "move2: index too large for m or mx"
    else
      let continue = move2 input ts i ((h,m),
                                  (hx,mx),(ifs,dx,mifs)) (next+1) stop sofar in
      let (ok,matchingTree) = memberN (Just i) ((V.!) m next) in
      let (mRootType,mRootF) = getNodeValue matchingTree in
      let rootType = fromMaybe (-1) mRootType in
      let rootF = fromMaybe (-1) mRootF in
      if rootF > (V.length m) - 1
      then error "move2: index too large for m (rootF)"
      else
        if ok && (rootF == next || ((V.!) m rootF) == [])
        then
          let mts = subtreesOf matchingTree in
          let mtsx = (V.!) mx next in
          let ifs0 = (V.!) mifs next in
          let n = (V.//) m [(next,[]),(rootF,mts)] in
          let nx = (V.//) mx [(next,[]),(rootF,mtsx)] in
          let nifs = (V.//) mifs [(next,[]),(rootF,(Just 2, Just i):ifs0)] in
          ([],[((ts,n),(hx,nx),((Just 3, Just i):ifs,0:dx,nifs))]):continue
        else continue
  else sofar

expands :: Lexicon -> [String] -> IdxCat -> [Parse] -> [Parse]
expands _ _ (([],_),_,_) sofar = sofar
expands lex input ((((Leaf label):ls),m),(hx,mx),dt) sofar =
  let sc = scan label input m mx dt sofar in
  expands lex input ((ls,m),(hx,mx),dt) sc
expands (strs,lts,ints) input ((((Node (t,g) bs):gs),m),(hx,mx),dt) sofar
  | t == (Just 1) = let (terms,nonterms) = terminalsOf bs in
    let g0 = fromMaybe (error "expands: bad index") g in
    let r1 = merge1 lts input terms g0 ((h,m),(hx,mx),dt) sofar in
    let r2 = merge2 lts input nonterms g0 ((h,m),(hx,mx),dt) r1 in
    let r3 = merge3 input terms g0 ((h,m),(hx,mx),dt) 0 (V.length m) r2 in
    let r4 = merge4 input nonterms g0 ((h,m),(hx,mx),dt) 0 (V.length m) r3 in
    expands (strs,lts,ints) input ((gs,m),(hx,mx),dt) r4
  | t == (Just 3) =
    let g0 = fromMaybe (error "expands: bad index") g in
    let v1 = move1 lts input bs g0 ((h,m),(hx,mx),dt) sofar in
    let v2 = move2 input bs g0 ((h,m),(hx,mx),dt) 0 (V.length m) v1 in
    expands (strs,lts,ints) input ((gs,m),(hx,mx),dt) v2
  | otherwise = error "expands: erroneous selection"
      where h = (Node (t,g) bs):gs

minIndex :: (MvIndex, V.Vector MvIndex) -> MvIndex
minIndex (i,ax) = V.foldl (\x -> (\y -> if y == [] then x else min x y)) i ax

-- for all elements in list
-- calculate priority of element
-- then push it into iq
pushICs :: IQ -> [IdxCat] -> IQ
pushICs q [] = q
pushICs q (ic:ics) =
  let new_priority = minIndex ((\x -> case x of (_,y,_) -> y) ic) in
  pushICs (PQMin.insert new_priority ic q) ics


-- try to improve pattern match, looks tightly hard to read
insertNewParses :: [String] -> Float -> Float -> IQ -> DQ -> [DNode]
  -> [Parse] -> DQ
insertNewParses _ _ _ _ dq _ [] = dq
insertNewParses input p new_p q dq dns ((w,[(([],_),_,(ifs,dx,_))]):more) =
  let (ok,remainder) = prefixT (w,input) in
  if ok
  then let newParse = (remainder,q,(Ld (dx,(w,ifs))):dns) in
    insertNewParses input p  new_p  q  (PQMax.insert p newParse dq) dns more
  else error "insertNewParses: bad parse detected"
insertNewParses input p new_p q dq dns (([],ics):more) =
  let new_iq = pushICs q ics in
  let new_dns = foldl (\l (_,_,(_,x,_)) -> (Nd x):l) dns ics in
  let newParse = (input,new_iq,new_dns) in
  insertNewParses input p  new_p  q  (PQMax.insert new_p newParse dq) dns more


derive :: Lexicon -> Float -> DQ -> [DNode]
derive lex min dq =
  if PQMax.null dq
  then error "Valid parse could not be found, sentence not accepted."
  else
    let ((p,(input,iq,dns)),new_dq) = fromMaybe
                                  (error "derive: Extracting from empty dq!")
                                  (PQMax.maxViewWithKey dq) in
    if (PQMin.null iq)
    then if (input == [])
      then dns
      else derive lex min new_dq
    else
      let (ic,new_iq) = fromMaybe
                        (error "derive: Extracting from empty iq!")
                        (PQMin.minView iq) in
      let xs = expands lex input ic [] in
      if length xs == 0 then derive lex min new_dq
      else
        let new_p = p / (fromIntegral (length xs)) in
        if new_p > min
        then
          derive lex min (insertNewParses input p new_p new_iq new_dq dns xs)
        else
          derive lex min new_dq

-- clean this pattern match up later...
makeDTreeFromIDTree :: [String] -> IDTree -> DTree
makeDTreeFromIDTree strs (Li (ss,ifs)) = L (ss,map (fOfInts strs) ifs)
makeDTreeFromIDTree strs (Oi t) = O (makeDTreeFromIDTree strs t)
makeDTreeFromIDTree strs (Xi (t1,t2)) = X (makeDTreeFromIDTree strs t1,
                                           makeDTreeFromIDTree strs t2)

parse :: Grammar -> String -> Float -> [String] -> DTree
parse gram start min input =
  let ltPair = makeLexTree gram [] EmptyTree in
  let strs = V.fromList (fst ltPair) in
  let lts = makeLexTrees ltPair in
  let startCat = (Just 0, (elemIndex' (fst ltPair) start)) in
  let startIntCat = fromMaybe
                    (error ("recognize: negative index; strs: " ++ show strs))
                    (snd startCat) in
  if startIntCat > (V.length lts) -1
  then error "recognize: index too large for lts"
  else
    let h = (V.!) lts startIntCat in
    let m = V.replicate (V.length strs) [] in
    let mx = V.replicate (V.length strs) [] in
    let ifs = [startCat] in
    let dx = [] in
    let mifs = V.replicate (V.length strs) [] in
    let dt = (ifs,dx,mifs) in
    let ic = ((h,m),([],mx),dt) in
    let iq = PQMin.singleton [] ic in
    let prob = 1.0 in
    let dq = PQMax.singleton prob (input,iq,[(Nd [])]) in
    makeDTreeFromIDTree (fst ltPair)
                        (dNodes2IDTree (derive (strs,lts,[]) min dq))

dt2t :: DTree -> StrTree
dt2t (L (string,fs)) =  T ("(" ++ (spacedStr2 string) ++ "," ++ (stringFs fs) ++ ")") []
dt2t (X (d1,d2)) = T "*" [dt2t d1,dt2t d2]
dt2t (O d) = T "o" [dt2t d]