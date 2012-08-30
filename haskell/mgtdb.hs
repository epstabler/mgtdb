{- file: mgtdb.hs 
  to exit ghci :quit 
  to load this file in ghci, :l mgtdb.hs 
-}


{- TODO: LOTS OF CLEANUP -}
module MGTDB
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

--type iCat = cat * (ix * ix array)
--(* cat, indices of head, movers, respectively *)
type IdxCat = (Category, (MvIndex, (V.Vector MvIndex)))

type Parse = ([String], [IdxCat])

type IQ = PQMin.MinPQueue MvIndex IdxCat
type DQ = PQMax.MaxPQueue Float ([String],IQ)

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

scan :: [String] -> [String] -> LexTrees -> V.Vector MvIndex -> [Parse]
  -> [Parse]
scan word input m mx sofar =
  if V.and (V.map null m)
  then let (ok, remainder) = prefixT (word,input) in
    if ok
    then (remainder,[(([],m),([],mx))]):sofar
    else sofar
  else sofar

merge1 :: LexTrees -> [String] -> [LexTree] -> Int -> IdxCat -> [Parse]
  -> [Parse]
merge1 lts input terms i ((_,m),(hx,mx)) sofar =
  if terms /= []
  then
    if i > (V.length lts) - 1
    then error ("merge1: index too large for lts" ++ "\n" ++ show i)
    else
      let empty_m = V.replicate (V.length m) [] in
      let empty_mx = V.replicate (V.length mx) [] in
               (input, [((terms,empty_m),((hx++[0]),empty_mx)),
               ((((V.!) lts i),m),((hx++[1]),mx))]):sofar
  else sofar

merge2 :: LexTrees -> [String] -> [LexTree] -> Int -> IdxCat
  -> [Parse] -> [Parse]
merge2 lts input nonterms i ((_,m),(hx,mx)) sofar =
  if nonterms /= []
  then
   if i > (V.length lts) - 1
     then error "merge2: index too large for lts"
     else 
       let empty_m = V.replicate (V.length m) [] in
       let empty_mx = V.replicate (V.length mx) [] in
          ((input,[((nonterms,m),(hx++[1],mx)),
          ((((V.!) lts i),empty_m),(hx++[0],empty_mx))]):sofar)
  else sofar

merge3 :: [String] -> [LexTree] -> Int -> IdxCat -> Int -> Int -> [Parse]
  -> [Parse]
merge3 input terms i ((h,m),(hx,mx)) next stop sofar =
  if terms /= [] && next < stop
  then
    let continue = merge3 input terms i ((h,m),(hx,mx)) (next+1) stop sofar in
    if next > (V.length m) - 1 || next > (V.length mx) - 1
    then error "merge3: index too large for m or mx"
    else
      let (ok,matchingTree) = memberN (Just i) ((V.!) m next) in
      if ok
      then
        let ts  = subtreesOf matchingTree in
        let tsx = (V.!) mx next in
        let empty_m = V.replicate (V.length m) [] in
        let empty_mx = V.replicate (V.length mx) [] in
        let n = (V.//) m [(next,[])] in
        let nx = (V.//) mx [(next,[])] in
        (input,[((terms,empty_m),(hx,empty_mx)),
                ((ts,n),(tsx,nx))]):continue
      else continue
  else sofar

merge4 :: [String] -> [LexTree] -> Int -> IdxCat -> Int -> Int -> [Parse]
  -> [Parse]
merge4 input nonterms i ((h,m),(hx,mx)) next stop sofar =
  if nonterms /= [] && next < stop
  then
    let continue = 
            merge4 input nonterms i ((h,m),(hx,mx)) (next+1) stop sofar in
    if next > (V.length m) - 1 || next > (V.length mx) - 1
    then error "merge4: index too large for m or mx"
    else
      let (ok,matchingTree) = memberN (Just i) ((V.!) m next) in
      if ok
      then
        let ts = subtreesOf matchingTree in
        let tsx = (V.!) mx next in
        let empty_m = V.replicate (V.length m) [] in
        let empty_mx = V.replicate (V.length mx) [] in
        let n = (V.//) m [(next,[])] in
        let nx = (V.//) mx [(next,[])] in
        (input,[((nonterms,n),(hx,nx)),
                ((ts,empty_m),(tsx,empty_mx))]):continue
      else continue
  else sofar

move1 :: LexTrees -> [String] -> [LexTree] -> Int -> IdxCat -> [Parse]
  -> [Parse]
move1 lts input ts i ((_,m),(hx,mx)) sofar =
  if i > (V.length lts) || i > (V.length m) - 1
  then error "move1: index too large for lts or m"
  else
    if ((V.!) m i) == []
    then
      let n = (V.//) m [(i,((V.!) lts i))] in
      let nx = (V.//) mx [(i,(hx++[0]))] in
      (input,[((ts,n),((hx++[1]),nx))]):sofar
    else sofar

move2 :: [String] -> [LexTree] -> Int -> IdxCat -> Int -> Int -> [Parse]
  -> [Parse]
move2 input ts i ((h,m),(hx,mx)) next stop sofar =
  if next < stop
  then
    if next > (V.length m) - 1 || next > (V.length mx) - 1
    then error "move2: index too large for m or mx"
    else
      let continue = move2 input ts i ((h,m),(hx,mx)) (next+1) stop sofar in
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
          let n = (V.//) m [(next,[]),(rootF,mts)] in
          let nx = (V.//) mx [(next,[]),(rootF,mtsx)] in
          (input,[((ts,n),(hx,nx))]):continue
        else continue
  else sofar

expands :: Lexicon -> [String] -> IdxCat -> [Parse] -> [Parse]
expands _ _ (([],_),_) sofar = sofar
expands lex input ((((Leaf label):ls),m),(hx,mx)) sofar =
  let sc = scan label input m mx sofar in
  expands lex input ((ls,m),(hx,mx)) sc
expands (strs,lts,ints) input ((((Node (t,g) bs):gs),m),(hx,mx)) sofar
  | t == (Just 1) = let (terms,nonterms) = terminalsOf bs in
    let g0 = fromMaybe (error "expands: bad index") g in
    let r1 = merge1 lts input terms g0 ((h,m),(hx,mx)) sofar in
    let r2 = merge2 lts input nonterms g0 ((h,m),(hx,mx)) r1 in
    let r3 = merge3 input terms g0 ((h,m),(hx,mx)) 0 (V.length m) r2 in
    let r4 = merge4 input nonterms g0 ((h,m),(hx,mx)) 0 (V.length m) r3 in
    expands (strs,lts,ints) input ((gs,m),(hx,mx)) r4
  | t == (Just 3) =
    let g0 = fromMaybe (error "expands: bad index") g in
    let v1 = move1 lts input bs g0 ((h,m),(hx,mx)) sofar in
    let v2 = move2 input bs g0 ((h,m),(hx,mx)) 0 (V.length m) v1 in
    expands (strs,lts,ints) input ((gs,m),(hx,mx)) v2
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
  let new_priority = minIndex (snd ic) in
  pushICs (PQMin.insert new_priority ic q) ics

-- try to improve pattern match, looks tightly hard to read
insertNewParses :: Float -> Float -> IQ -> DQ -> [Parse] -> DQ
insertNewParses _ _ _ dq [] = dq
insertNewParses p new_p q dq ((input,[(([],_),_)]):more) =
  let newParse = (input,q) in
  insertNewParses p  new_p  q  (PQMax.insert p newParse dq) more
insertNewParses p new_p q dq ((input,ics):more) =
  let newParse = (input,(pushICs q ics)) in
  insertNewParses p  new_p  q  (PQMax.insert new_p newParse dq) more

derive :: Lexicon -> Float -> DQ -> Bool
derive lex min dq =
  if PQMax.null dq
  then False
  else
    let ((p,(input,iq)),new_dq) = fromMaybe
                                  (error "derive: Extracting from empty dq!")
                                  (PQMax.maxViewWithKey dq) in
    if (PQMin.null iq)
    then if (input == [])
      then True
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
        then derive lex min (insertNewParses p new_p new_iq new_dq xs)
        else derive lex min new_dq
    
recognize :: Grammar -> String -> Float -> [String] -> Bool
recognize gram start min input =
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
    let ic = ((h,m),([],mx)) in
    let iq = PQMin.singleton [] ic in
    let prob = 1.0 in
    let dq = PQMax.singleton prob (input,iq) in
    derive (strs,lts,[]) min dq