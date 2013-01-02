"""
file: mgtdb.py
      minimalist grammar top-down beam recognizer
"""
import heapq
import time

"""
Example grammar, in a format fairly easy for humans
"""
mg0 = [ ([],[('sel','V'),('cat','C')]),
        ([],[('sel','V'),('pos','wh'),('cat','C')]),
        (['the'],[('sel','N'),('cat','D')]), 
        (['which'],[('sel','N'),('cat','D'),('neg','wh')]), 
        (['king'],[('cat','N')]),
        (['queen'],[('cat','N')]),
        (['wine'],[('cat','N')]),
        (['beer'],[('cat','N')]),
        (['drinks'],[('sel','D'),('sel','D'),('cat','V')]),
        (['prefers'],[('sel','D'),('sel','D'),('cat','V')]),
        (['knows'],[('sel','C'),('sel','D'),('cat','V')]),
        (['says'],[('sel','C'),('sel','D'),('cat','V')])
        ]
"""
   We represent the lexicon as a tree,
   where the tree is a list, where the first element is the root,
   and the rest is the list of the subtrees.
"""
def ensureMember(e,l):
    if e in l:
        return l
    else:
        return l.append(e)

def stringValsOfG(g): # the string values of the features
    sofar = []
    for (ss,fs) in g:
        for (ftype,fval) in fs:
            ensureMember(fval,sofar)
    return sofar

def listNth(e,l): # return (first) position of e in l
    return l.index(e)

def intsOfF(sA,(ftype,fval)): # convert string feature to integer pair
    if ftype=='cat':
        return (0,listNth(fval,sA))
    elif ftype=='sel':
        return (1,listNth(fval,sA))
    elif ftype=='neg':
        return (2,listNth(fval,sA))
    elif ftype=='pos':
        return (3,listNth(fval,sA))
    else:
        raise RuntimeError('error: intsOfF')

"""
To make building the tree straightforward,
  we reverse features lists and convert them to integers first
"""
def revItem (sl, (ss,fs)):
    safe_fs = fs[:] # make a copy
    if len(fs)>0:
        rifs = [intsOfF(sl,f) for f in reversed(safe_fs)]
        return (ss,rifs)
    else:
        return (ss,fs)

def findRoot(f,trees): # find subtree with root matching f, if there is one
    for i,t in enumerate(trees):
        if (isinstance(t,list) and len(t)>0 and t[0]==f):
            return i
    return -1

def revItemIntoLexTrees(lst, (ss,fs)):
     for f in fs:
         i = findRoot(f,lst)
         if i>=0:
             lst = lst[i]
         else:
             lst.append([f])
             lst = lst[-1]
     lst.append(ss)

def gIntoLexTreeList(sA,g):
    lexTrees = []
    for ri in [revItem(sA,i) for i in g]:
        revItemIntoLexTrees(lexTrees,ri)
    return lexTrees

"""
We put those lexTrees in order, so that the subtree
with root type (i,j) appears in lA[j] and has feature type tA[j]=i.
"""
def gIntoLexArrayTypeArray(sA,g):
    lst = gIntoLexTreeList(sA,g)
    lA = [[]]*len(sA)
    tA = [0]*len(sA)
    for t in lst:
        (i,j)=t[0]
        lA[j]=t[1:]
        tA[j]=i
    return (lA,tA)

"""
minIndex should only check indices of filled mover positions.
No mover has an empty index, so we can ignore them.
"""
def minIndex(((h,m),(hx,mx))):
    min = hx
    for x in mx:
        if x<>[] and x<min:
            min=x
    return min

"""
For the queue of parses, we put the probability first, and negate it,
  (-probability,input,iq)
so the most probable element will be the most negative, and hence the
minimal element popped at each step.
"""
def emptyListArray(m):
    result = True
    for e in m:
        if e<>[]:
            result = False
    return result

def terminalsOf(ts):
    terms=[]
    nonterms=[]
    for t in ts:
        if len(t)>0 and isinstance(t[0],tuple):
            nonterms.append(t)
        else:
            terms.append(t)
    return (terms,nonterms)

""" 
prefixT(w,input) returns (True,n) if input[:n]=w
 else returns (False,0) if w not a prefix of input
""" 
def prefixT(w,lst):
    if w==lst[0:len(w)]:
        return (True,len(w))
    else:
        return (False,0)

""" 
 memberFval(i,lst) returns (True,t) if t is an element of lst such that
   root of t is (type,i); else (False,[) if no match
""" 
def memberFval(i,lst):
    for pos,e in enumerate(lst):
        if e[0][1]==i:
            return (True,lst[pos])
    return (False,[])

def scan(w,inpt,m,mx,sofar):
    if emptyListArray(sofar):
        (ok,remainderInt) = prefixT(w,inpt)
        if ok:
            exp = (inpt[remainderInt:],[(([],m),([],mx))])
            sofar.append(exp)

def merge1(lA,inpt,terms,i,((h,m),(hx,mx)),sofar):
    if terms <> []:
        new_head_index=hx[:]
        new_head_index.append(0)
        new_comp_index=hx[:]
        new_comp_index.append(1)
        empty_m = [[]]*len(m)
        empty_mx = [[]]*len(mx)
        ic1 = ((terms,empty_m),(new_head_index,empty_mx))
        ic2 = ((lA[i],m),(new_comp_index,mx)) # movers to complement only
        exp = (inpt,[ic1,ic2])
        sofar.append(exp)

def merge2(lA,inpt,nonterms,i,((h,m),(hx,mx)),sofar):
    if nonterms <> []:
        new_head_index=hx[:]
        new_head_index.append(1)
        new_comp_index=hx[:]
        new_comp_index.append(0)
        empty_m = [[]]*len(m)
        empty_mx = [[]]*len(mx)
        ic1 = ((nonterms,m),(new_head_index,mx)) # movers to head
        ic2 = ((lA[i],empty_m),(new_comp_index,empty_mx)) # no spec movers
        exp = (inpt,[ic1,ic2])
        sofar.append(exp)

def merge3(inpt,terms,i,((h,m),(hx,mx)),sofar):
    if terms <> []:
        for nxt in range(len(m)):
            (ok,matchingTree) = memberFval(i,m[nxt])
            if ok:
                ts = matchingTree[1:]
                tsx = mx[nxt]
                empty_m = [[]]*len(m)
                empty_mx = [[]]*len(mx)      
                n = m[:]
                nx = mx[:]
                n[nxt] = [] # we used the "next" licensee, so now empty
                nx[nxt] = []
                ic1 = ((terms,empty_m),(hx,empty_mx))
                ic2 = ((ts,n),(tsx,nx)) # movers passed to complement
                exp = (inpt,[ic1,ic2])
                sofar.append(exp)

def merge4(inpt,nonterms,i,((h,m),(hx,mx)),sofar):
    if nonterms <> []:
        for nxt in range(len(m)):
            (ok,matchingTree) = memberFval(i,m[nxt])
            if ok:
                ts = matchingTree[1:]
                tsx = mx[nxt]
                empty_m = [[]]*len(m)
                empty_mx = [[]]*len(mx)                
                n = m[:]
                nx = mx[:]
                n[nxt] = [] # we used the "next" licensee, so now empty
                nx[nxt] = []
                ic1 = ((nonterms,empty_m),(hx,empty_mx))
                ic2 = ((ts,n),(tsx,nx)) # movers passed to complement
                exp = (inpt,[ic1,ic2])
                sofar.append(exp)

def move1(lA,inpt,ts,i,((h,m),(hx,mx)),sofar):
    if m[i] == []:  # SMC
        n = m[:]
        nx = mx[:]
        n[i] = lA[i]
        nx[i] = hx[:]
        nx[i].append(0)
        new_head_index=hx[:]
        new_head_index.append(1)
        ic1 = ((ts,n),(new_head_index,nx))
        exp = (inpt,[ic1])
        sofar.append(exp)

def move2(inpt,ts,i,((h,m),(hx,mx)),sofar):
    for nxt in range(len(m)):
        (ok,matchingTree) = memberFval(i,m[nxt])
        if ok:
            rootF = matchingTree[0][1] # value of rootLabel
            if rootF==nxt or m[rootF]==[]: # SMC
                mts = matchingTree[1:]
                mtsx = mx[nxt]
                n = m[:]
                nx = mx[:]
                n[nxt] = [] # we used the "next" licensee, so now empty
                nx[nxt] = []
                n[rootF] = mts
                nx[rootF] = mtsx
                ic1 = ((ts,n),(hx,nx))
                exp = (inpt,[ic1])
                sofar.append(exp)

def exps((sA,lA,tA),inpt,((h,m),(hx,mx)),sofar):
    for t in h:
        if len(t)>0 and isinstance(t[0],tuple):
            if t[0][0] == 1: # feature type 1 is 'sel'
                i = t[0][1] # set i to feature value
                (terms,nonterms)= terminalsOf(t[1:])
                merge1(lA,inpt,terms,i,((h,m),(hx,mx)),sofar)
                merge2(lA,inpt,nonterms,i,((h,m),(hx,mx)),sofar)
                merge3(inpt,terms,i,((h,m),(hx,mx)),sofar)
                merge4(inpt,nonterms,i,((h,m),(hx,mx)),sofar)
            elif t[0][0] == 3: # feature type 3 is 'pos'
                i = t[0][1] # set i to feature value
                ts = t[1:]
                move1(lA,inpt,ts,i,((h,m),(hx,mx)),sofar)
                move2(inpt,ts,i,((h,m),(hx,mx)),sofar)
            else:
                raise RuntimeError('exps')
        else:
            scan(t,inpt,m,mx,sofar)

def insertNewParses(p,new_p,q,dq,exps):
    for exp in exps:
        # scan is a special case, identifiable by empty head
        # (inpt,[(([],m),([],mx))]) <-- we check for that empty head
        if exp[1][0][0][0]==[]:
            inpt = exp[0]
            newParse = (p,inpt,q)
            heapq.heappush(dq,newParse)
        else: # put indexed categories ics onto iq with new_p
            inpt = exp[0]
            ics = exp[1]
            safe_q=q[:]
            for ic in ics:
                newIndex=minIndex(ic)
                heapq.heappush(safe_q,(newIndex,ic))
            newParse = (new_p,inpt,safe_q)
            heapq.heappush(dq,newParse)

def derive(lexArrays,minP,dq): # eliminate the recursion here?
    p = 1.0
    while len(dq) > 0:
#        printDQ(lexArrays,dq)
        (p,inpt,iq) = heapq.heappop(dq)
#        print 'new loop through derive...'
        print '# of parses in beam=',len(dq)+1,', p(best parse)=',(-1 * p)
        if len(iq)==0 and len(inpt)==0:
            return True  # success!
        elif len(iq)>0:
            prediction = heapq.heappop(iq)
            ic = prediction[1]
            sofar = []
            exps(lexArrays,inpt,ic,sofar)
            if len(sofar)==0:
                return derive(lexArrays,minP,dq)
            else:
                new_p = p / float(len(sofar))
                if new_p < minP:
                    insertNewParses(p,new_p,iq,dq,sofar)
                else:
                    print 'improbable parses discarded'
    return False # failure!

def recognize(lex,start,minP,inpt): # initialize and begin
    sA = stringValsOfG(lex)
    (lA,tA) = gIntoLexArrayTypeArray(sA,lex)
    startInt = intsOfF(sA,('cat',start))[1]
    h = lA[startInt]
    m = [[]]*len(sA)
    mx = [[]]*len(sA)
    ic = ((h,m),([],mx))
    iq = [([],ic)]
    heapq.heapify(iq)
    dq = [(-1.0,inpt,iq)]
    heapq.heapify(dq)
    t0 = time.time()
    result = derive((sA,lA,tA),minP,dq)
    print time.time() - t0, "seconds:", result
"""
inpt0=['the','king','knows','which','wine','the','queen','prefers']
recognize(mg0,'C',0.001,inpt0)

inpt0=['the','king','knows','which','queen','prefers','the','wine']
recognize(mg0,'C',0.001,inpt0)

inpt0=['the','queen','says','the','king','knows','which','queen','prefers','the','wine']
recognize(mg0,'C',0.001,inpt0)
"""
