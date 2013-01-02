"""
file: mgtdb-dev.py
      minimalist grammar top-down beam recognizer - 
        development file with examples and print routines
"""
import sys
from nltk.util import in_idle
from nltk.tree import Tree
from nltk.draw import *
import heapq

"""
   An NLTK tree:
t=Tree(1, [2, Tree(3, [4]), 5])
   We can display this tree with:
t.draw()
   Alternatively:
TreeView(t)
   TreeView will stay alive after the call, returning control to the python process.

   Compare also:
print(t)
type(t)
Tree.leaves(t)
Tree.height(t)
for i in Tree.subtrees(t): print i
t.flatten()
print t

   Another way to write a tree that has the same display is this:
t=Tree(1, [Tree(2,[]), Tree(3,[Tree(4,[])]), Tree(5,[])])
   Then we get all 5 subtrees, as expected:
for x in t.subtrees(): print x
"""

"""
   We can also represent trees with lists, where the first element is the root,
   and the rest is the list of the subtrees.

   For example, a simple 3 node tree, with 3 subtrees, is defined by
x = [1,[2],[3,[4]],[5]]

   First, a pretty printer for these list trees:
"""
def pptreeN(n,t): # pretty print t indented n spaces
    if isinstance(t,list) and len(t)>0:
        sys.stdout.write('\n'+' '*n+'[')
        print(t[0]), # print root and return
        if len(t[1:])>0:
            sys.stdout.write(',') # comma if more coming
        for i,subtree in enumerate(t[1:]):  # then print subtrees indented by 4
            pptreeN(n+4,subtree)
            if i<len(t[1:])-1:
                sys.stdout.write(',') # comma if more coming
        sys.stdout.write(']')
    else:
        sys.stdout.write('\n'+' '*n)
        print(t),

def pptree(t):
    if len(t)==0: # catch special case of empty tree, lacking even a root
        sys.stdout.write('\n[]\n')
    else:
        pptreeN(0,t)
        sys.stdout.write('\n')

"""
example: 
pptree([1, 2, [3, 4], [5, 6]])
pptree(['TP', ['DP', ['John']], ['VP', ['V',['praises']], ['DP', ['Mary']]]])

I have intensionally written this prettyprinter so that the
prettyprinted form is a well-formed term.
"""

"""
   We can convert a list tree to an NLTK tree with the following:
"""
def list2nltktree(listtree):
    if listtree==[]:
        return []
    else:
        subtrees=[list2nltktree(e) for e in listtree[1:]]
        return Tree(listtree[0],subtrees)

"""
   With that definition, the following commands produce the same results 
   as our first example:

t = [1,[2],[3,[4]],[5]]

list2nltktree(t).draw()

for e in list2nltktree(t).subtrees(): print e
"""

"""
OK, we now begin implementing the beam recognizer. 
We will number the categories so that we have only integer comparisons
at runtime, and we can use those integers as position indices.

The human readable form of the grammar:
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

mg1 = [ ([],[('sel','V'),('cat','C')]), # = mg0 but without wh features, so no move
        (['the'],[('sel','N'),('cat','D')]), 
        (['king'],[('cat','N')]),
        (['queen'],[('cat','N')]),
        (['wine'],[('cat','N')]),
        (['beer'],[('cat','N')]),
        (['drinks'],[('sel','D'),('sel','D'),('cat','V')]),
        (['prefers'],[('sel','D'),('sel','D'),('cat','V')]),
        (['knows'],[('sel','C'),('sel','D'),('cat','V')]),
        (['says'],[('sel','C'),('sel','D'),('cat','V')])
        ]

mg2 = [ ([],[('sel','V'),('cat','C')]), # = mg1 but without specs, so no merge2
        (['the'],[('sel','N'),('cat','D')]), 
        (['king'],[('cat','N')]),
        (['queen'],[('cat','N')]),
        (['wine'],[('cat','N')]),
        (['beer'],[('cat','N')]),
        (['drinks'],[('sel','D'),('cat','V')]),
        (['prefers'],[('sel','D'),('cat','V')]),
        (['knows'],[('sel','C'),('cat','V')]),
        (['says'],[('sel','C'),('cat','V')])
        ]

"""
It will be good practice to print out those grammars in slightly
more readable forms, using the following functions:
"""
def btfyFtype(t):
    if t=='cat':
        return ''
    elif t=='sel':
        return '='
    elif t=='neg':
        return '-'
    elif t=='pos':
        return '+'
    else:
        raise RuntimeError('btfyType('+str(t)+')')

def btfyFeat((ftype,f)):
    result = btfyFtype(ftype) + f
    return result

def btfyLexItem((s,fs)):
    fstrings = []
    for f in fs:
        fstrings.append(btfyFeat(f))
    result = ' '.join(s) + '::' + ' '.join(fstrings)
    return result

def showGrammar(g):
    for item in g:
        print(btfyLexItem(item))
"""
example: showGrammar(mg0)
"""

def ensureMember(e,l):
    if e in l:
        return l
    else:
        return l.append(e)
"""
example: ensureMember(2,[1,3])
example: ensureMember(2,[1,2])
"""

def stringValsOfG(g): # the string values of the features
    sofar = []
    for (ss,fs) in g:
        for (ftype,fval) in fs:
            ensureMember(fval,sofar)
    return sofar

# example
sA0 = stringValsOfG(mg0)  
sA2 = stringValsOfG(mg2)  

def listNth(e,l): # return (first) position of e in l
    return l.index(e)

def intsOfF(sA,(ftype,fval)): # convert string representation of feature to integer pair
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
intsOfF(sA0,('sel','N'))
"""

def fOfInts(sA,(itype,ival)): # convert integer representation back to string pair
    if itype==0:
        return ('cat',sA[ival])
    elif itype==1:
        return ('sel',sA[ival])
    elif itype==2:
        return ('neg',sA[ival])
    elif itype==3:
        return ('pos',sA[ival])
    else:
        raise RuntimeError('error: fOfInts')
"""
fOfInts(sA0,(1,1))
btfyFeat(fOfInts(sA0,(1,1)))
"""

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
"""
examples:
item0=(['hi'],[])
revItem(sA0,item0)
item0=(['the'],[('sel','N'),('cat','D')])
revItem(sA0,item0)

We don't need these, but easy to define.
def rootLabel(t):
    t[0]
def subtreesOf(t):
    t[1:]
"""

# some functions for print out
def lexTree2stringTree(sA,t):
    if len(t)>0 and isinstance(t[0],tuple):
        root = btfyFeat(fOfInts(sA,t[0]))
        subtrees = lexTrees2stringTrees(sA,t[1:])
        return [root]+subtrees
    else:
        return t
"""
example (using lA0 which is defined below):
intsOfF(sA0,('cat','D'))
lA0[0]
lexTree2stringTree(sA0,[intsOfF(sA0,('cat','D'))]+lA0[0])
"""
def lexTrees2stringTrees(sA,ts):
    return [lexTree2stringTree(sA,t) for t in ts]

# to get trees in the array, insert the root feature determined by the index        
def lexArrays2lexTrees((sA,lA,tA)):
    return [ ([(tA[i],i)]+lA[i]) for i in range(len(sA)) ]
"""
example: lexArrays2lexTrees((sA0,lA0,tA0))
"""

def lexArrays2stringTrees((sA,lA,tA)):
    return lexTrees2stringTrees(sA,lexArrays2lexTrees((sA,lA,tA)))
"""
example:
lexArrays2stringTrees((sA0,lA0,tA0))
"""

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
"""
lexTreeList0 = []
item0=(['the'],[('sel','N'),('cat','D')])
revItem0 = revItem(sA0,item0)
revItemIntoLexTrees(lexTreeList0,revItem0)

item1=(['a'],[('sel','N'),('cat','D')])
revItem1 = revItem(sA0,item1)
revItemIntoLexTrees(lexTreeList0,revItem1)

item2=(['who'],[('sel','N'),('cat','D'),('neg','wh')])
revItem2 = revItem(sA0,item2)
revItemIntoLexTrees(lexTreeList0,revItem2)

item3=(['says'],[('sel','C'),('sel','D'),('cat','V')])
revItem3 = revItem(sA0,item3)
revItemIntoLexTrees(lexTreeList0,revItem3)
"""

def gIntoLexTreeList(sA,g):
    lexTrees = []
    for ri in [revItem(sA,i) for i in g]:
        revItemIntoLexTrees(lexTrees,ri)
    return lexTrees

ts0 = gIntoLexTreeList(sA0,mg0)
t0 = ['.']+ts0
ts2 = gIntoLexTreeList(sA2,mg2)
t2 = ['.']+ts2
"""
example:
ts0 = gIntoLexTreeList(sA0,mg0)
t0 = ['.']+ts0

list2nltktree(t0).draw()

That tree has the integer representation of the categories,
but now it's easy to "beautify" the features:
"""
def list2btfytree(sA,listtree):
    if listtree==[]:
        return []
    else:
        subtrees=[list2btfytree(sA,e) for e in listtree[1:]]
        if isinstance(listtree[0],tuple):
            root = btfyFeat(fOfInts(sA,listtree[0]))
        else:
            root = listtree[0]
        return Tree(root,subtrees)
"""
list2btfytree(sA0,t0)
list2btfytree(sA0,t0).draw()
TreeView(list2btfytree(sA0,t0))
['V', 'C', 'wh', 'N', 'D']
"""

"""
Let's put those lexTrees in order, so that the subtree
with root type (i,j) appears in lA[j] and has feature type tA[j]=i.
Since lists are arrays in python, these lists will let us
get these values with simple O(1) lookup.
"""
def gIntoLexArrayTypeArray(sA,g):
    lst = gIntoLexTreeList(sA,g)
    lexArray = [[]]*len(sA)
    typeArray = [0]*len(sA)
    for t in lst:
        # print t
        (i,j)=t[0]
        lexArray[j]=t[1:]
        typeArray[j]=i
    return (lexArray,typeArray)

"""
example:
"""
(lA0,tA0) = gIntoLexArrayTypeArray(sA0,mg0)
lexArrays0 = (sA0,lA0,tA0)

(lA2,tA2) = gIntoLexArrayTypeArray(sA2,mg2)
lexArrays2 = (sA2,lA2,tA2)

"""
check: OK
t2 = ['.']+lexArrays2stringTrees((sA2,lA2,tA2))
TreeView(list2nltktree(st2))
"""

def btfyIndex(lst):
    return ''.join([str(i) for i in lst])

def printCatI((ts,ixs)):
    for (t,i) in zip(ts,ixs):
        pptree(t)
        print i,',',

def nonEmptyMover((tree,index)): # a tree with just a root is "empty"
    return len(tree) > 1

def deleteEmpties((movertrees,moverIndices)):
    result = zip(*filter(nonEmptyMover,zip(movertrees,moverIndices)))
    if result==[]:
        return ([],[])
    else:
        return result

def printIcat((sA,lA,tA),((h,mA),(ih,iA))):
    ihS = btfyIndex(ih)
    iASs = [btfyIndex(i) for i in iA]
    hTs = lexTrees2stringTrees(sA,h)  # nb: h is a tree list
    mTs = lexArrays2stringTrees((sA,mA,tA)) # mA an array of tree lists, roots given by index
    for t in hTs:
        pptree(t)
        print ihS,',',
        printCatI(deleteEmpties((mTs,iASs)))

"""
example:
def testic0():
    h=lA0[0]
    m=[[]]*len(sA0)
    mx=[[]]*len(sA0)
    hx=[0,1,1]
    m[2]=lA0[2]
    mx[2]=[0,0]
    return ((h,m),(hx,mx))

ic0 = testic0()
printIcat((sA0,lA0,tA0),ic0)
"""

"""
Now consider minheaps. Lists also have a min function

l1=[4,1,3,2]
min(l1)

But insert/extract_min are O(n) on lists, and much faster for heaps.

l1=[4,1,3,2]
heapq.heapify(l1)

The usual length and access-by-position still works:

len(l1)
l1[0]
l1[1]

More importantly, we can pop the minimum element, and
push new elements:

heapq.heappop(l1)
len(l1)

safe_l1 = l1[:]
len(safe_l1)

heapq.heappop(l1)
len(l1)
len(safe_l1)

heapq.heappop(safe_l1)

heapq.heappush(l1,0)
heapq.heappop(l1)
heapq.heappop(l1)
heapq.heappop(l1)
heapq.heappop(l1)
"""

"""
Since heapq is not set up to let us define what counts as "least",
we will put (minIndex,ic) pairs onto the heap, so popq will give
us the predicted indexed category ic with the smallest index,
using the standard ordering.

minIndex should only check indices of filled mover positions.
No mover has an empty index, so we can ignore them.
"""
def minIndex(((h,m),(hx,mx))):
    min = hx
    for x in mx:
        if x<>[] and x<min:
            min=x
    return min

def printIQ(lexArrays,iq):
    for (i,ic) in iq:
        printIcat(lexArrays,ic)
        print '... end ic'
"""
example:
exactly as before, but now with (min_index,ic) elements, using ic0 defined above:

iq0 = [(minIndex(ic0),ic0)]
heapq.heapify(iq0)
len(iq0)
printIQ(lexArrays0,iq0)

heapq.heappush(iq0,(minIndex(ic0),ic0))
len(iq0)
printIQ(lexArrays0,iq0)
"""

"""
For the queue of parses, we put the probability first, and negate it,
  (-probability,input,iq)
so the most probable element will be the most negative, and hence the
minimal element popped at each step.
"""
def printDQ(lexArrays,dq):
    for (p,input,iq) in dq:
        print p,': ',' '.join(input)
        printIQ(lexArrays,iq)
        print ' ---- end parse'
"""
dq0=[(-0.1,['this','is','the','input'],iq0)]
heapq.heapify(dq0)
printDQ(lexArrays0,dq0)

heapq.heappush(dq0,(-0.1,['this','is','the','input'],iq0))
len(dq0)
printDQ(lexArrays0,dq0)
""" 

def emptyListArray(m):
    result = True
    for e in m:
        if e<>[]:
            result = False
    return result
""" 
example:
emptyListArray([[],[],[]])
emptyListArray([[],[],[1]])
""" 

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
example: terminalsOf(lA0[0])
""" 

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
#        print "scan?", ok, "scanning", inpt[0:remainderInt]
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
        ic1 = ((terms,empty_m),(new_head_index,empty_mx)) # no movers from lexical head, obviously
        ic2 = ((lA[i],m),(new_comp_index,mx)) # movers from complement only
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
        ic1 = ((nonterms,m),(new_head_index,mx)) # movers from head
        ic2 = ((lA[i],empty_m),(new_comp_index,empty_mx)) # no movers from spec
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
#    print "move2?",
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
    return derive((sA,lA,tA),minP,dq)
"""
  first examples use only scan
inpt0=['wine']
recognize(mg2,'N',0.01,inpt0)

  first examples use only scan, merge1
inpt0=['the','wine']
recognize(mg2,'D',0.01,inpt0)

inpt0=['prefers','the','wine']
recognize(mg2,'V',0.01,inpt0)

inpt0=['prefers','the','wine']
recognize(mg2,'C',0.01,inpt0)

inpt0=['knows','prefers','the','wine']
recognize(mg2,'C',0.01,inpt0)

inpt0=['says','knows','prefers','the','wine']
recognize(mg2,'C',0.01,inpt0)

inpt0=['knows','says','knows','prefers','the','wine']
recognize(mg2,'C',0.01,inpt0)

  first examples use only scan, merge1, merge2
inpt0=['the','queen','prefers','the','wine']
recognize(mg1,'C',0.001,inpt0)

inpt0=['the','king','knows','the','queen','prefers','the','wine']
recognize(mg1,'C',0.001,inpt0)

  these examples use merge1, merge2, merge3, move1
inpt0=['which','wine','the','wine','prefers']
recognize(mg0,'C',0.001,inpt0)

inpt0=['which','wine','prefers','the','wine']
recognize(mg0,'C',0.001,inpt0)

inpt0=['which','wine','the','queen','prefers']
recognize(mg0,'C',0.001,inpt0)

inpt0=['the','king','knows','which','wine','the','queen','prefers']
recognize(mg0,'C',0.001,inpt0)

  these examples use merge1, merge2, merge4, move1
inpt0=['the','king','knows','which','queen','prefers','the','wine']
recognize(mg0,'C',0.001,inpt0)

inpt0=['the','queen','says','the','king','knows','which','queen','prefers','the','wine']
recognize(mg0,'C',0.001,inpt0)
"""
