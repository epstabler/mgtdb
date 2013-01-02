"""
file: mgtdbp.py
      minimalist grammar top-down beam parser, development version.

   This is a working, development version, with print routines and examples.

   Using NLTK installed under Python 2.7, you can start the read-parse loop by
   typing, in a terminal window:
         python mgtdbp.py grammar startCategory minimumProbability
   For example:
         python mgtdbp.py mg0 C 0.0001
   (For line mode editing, I start the loop with: rlwrap python mgtdbp.py)
   The loop is started by the command at the bottom of this file.
   Python will prompt you for a sentence to parse with the grammar specified
   in that line.
   If the grammar is mg0, for example, you could type one of:
            the king prefers the beer
            which queen says the king knows which wine the queen prefers
   Then you will be given a prompt to which you can type
            h
   to get a list of the options.

   This file extends mgtdb-dev.py to a parser, by keeping a tree in each
   partial analysis. Note that, although this is a TD parser, derivation
   nodes are not expanded left-to-right in the standard, so we record 
   the left-right order of each node with indices (similar to indexing of
   predicted cats).
     To each indexed category (iCat) we add its own dtree node index,
        and we also add a list of the features checked in its own 
         projection so far.
     To each derivation (der) we add its list of indexed category dtree node
     indices. In each step of the derivation, we extend the parents node index,
        putting the results into the derivation list, and in the 
        respective children.

   So each indexed category ic = (i,c,dt) where dt is a "dtuple", that is:
       (Fs checked so far, index of current node, array of Fs moving elements).
   Here, dtuples are never checked during the parse, but they could be used
   to influence probability assignments at each step.

   For the moment, we compute just the most probable parse, 
        using a uniform distribution at each choice point,
        returning the derivation (as a "dnode list", or else error,
        instead of just true or false as the recognizer does.
   TODO: implement more sophisticated pruning rule (cf Roark) and more
     sophisticated determination of which trees should be returned.

   * For cats that lack subtrees in lex, tA is not set. This does not matter,
     but we could probably get rid of tA altogether.
   * sA sets all features, but lA only needs non-empty lexTrees.
   * We might speed things up by numbering all subtrees of the lexArray,
     and using int list arrays to encode the whole lexTree.

Comments welcome: stabler@ucla.edu
"""
import sys
from nltk.util import in_idle
from nltk.tree import Tree
from nltk.draw import *
import heapq
import time

"""
   We represent trees with lists, where the first element is the root,
   and the rest is the list of the subtrees.

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
    if isinstance(listtree,tuple): # special case for MG lexical items at leaves
        return (' '.join(listtree[0]) + ' :: ' + ' '.join(listtree[1]))
    elif isinstance(listtree,str): # special case for strings at leaves
        return listtree
    elif isinstance(listtree,list) and listtree==[]:
        return []
    elif isinstance(listtree,list):
        subtrees=[list2nltktree(e) for e in listtree[1:]]
        if subtrees == []:
            return listtree[0]
        else:
            return Tree(listtree[0],subtrees)
    else:
        raise RuntimeError('list2nltktree')

"""
With an NLTK tree, we can use NLTK tree display:

list2nltktree(t).draw()
TreeView(t)
"""

"""
OK, we now begin implementing the beam parser. 
We will number the categories so that we have only integer comparisons
at runtime, and we can use those integers as position indices.

The human readable form of the grammar:
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
        raise RuntimeError('btfyFtype('+str(t)+')')

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

"""
Now we begin building the lexical representation of the grammar
  as a tree (actually, as a list of trees)
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
"""
example
sA0 = stringValsOfG(mg0)  
sA2 = stringValsOfG(mg2)  
"""

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

def fOfInts(sA,(itype,ival)): # convert int representation back to string pair
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
To make building the tree representation of the grammar straightforward,
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
"""
example:
ts0 = gIntoLexTreeList(sA0,mg0)
t0 = ['.']+ts0
ts2 = gIntoLexTreeList(sA2,mg2)
t2 = ['.']+ts2

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
(lA0,tA0) = gIntoLexArrayTypeArray(sA0,mg0)
lexArrays0 = (sA0,lA0,tA0)

(lA2,tA2) = gIntoLexArrayTypeArray(sA2,mg2)
lexArrays2 = (sA2,lA2,tA2)

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

def printIcat((sA,lA,tA),((h,mA),(ih,iA),dt)):
    ihS = btfyIndex(ih)
    iASs = [btfyIndex(i) for i in iA]
    hTs = lexTrees2stringTrees(sA,h)  # nb: h is a tree list
    mTs = lexArrays2stringTrees((sA,mA,tA)) # mA an array of tree lists
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
    dt = ([],[],[])
    return ((h,m),(hx,mx),dt)

ic0 = testic0()
printIcat((sA0,lA0,tA0),ic0,dt)
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
def minIndex(((h,m),(hx,mx),dt)):
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
exactly as before, but now with (min_index,ic) elements,
using ic0 defined above:

iq0 = [(minIndex(ic0),ic0)]
heapq.heapify(iq0)
len(iq0)
printIQ(lexArrays0,iq0)

heapq.heappush(iq0,(minIndex(ic0),ic0))
len(iq0)
printIQ(lexArrays0,iq0)
"""

def printIQ(lexArrays,iq):
    for (i,ic,d) in iq:  # d is "dtuple" for this indexed category
        printIcat(lexArrays,ic)
        print '... end ic'
"""
For the queue of parses, we put the probability first, and negate it,
  (-probability,input,iq)
so the most probable element will be the most negative, and hence the
minimal element popped at each step.
"""

"""
Our last preparatory steps are the utilities for building the 
derivation tree. Gorn adresses for each node of the derivation tree
are stored during the parse, together with the lexical labels of the
leaves. From these, we can compute the standard form of the derivation tree.
"""
def splitDnodes(nontermsSofar,termsSoFar,dns):
    for dn in dns:
        if isinstance(dn,tuple):  # a terminal is a pair (node,terminal string list)
            termsSoFar.append(dn)
        else:
            nontermsSofar.append(dn) # node is an integer list

def child(n1,n2): # boolean: is n1 a prefix of n2? If so: n2 is a child of n1
    return n1 == n2[0:len(n1)]

# build the derivation tree that has parent as its root, and return it
def buildIDtreeFromDnodes(parent,nodes,terminals,t):
    if len(terminals)>0 and min(terminals)[0]==parent:
        leaf = heapq.heappop(terminals)
        t.append(leaf[1])
    elif len(nodes)>0 and child(parent,min(nodes)):
        root = heapq.heappop(nodes)
        t.append(['.'])  # place-holder
        t = t[-1]
        child0 = buildIDtreeFromDnodes(root,nodes,terminals,t)
        if len(nodes)>0 and child(parent,min(nodes)):
            t[0]='*'  # replace place-holder
            root1 = heapq.heappop(nodes)
            child1 = buildIDtreeFromDnodes(root1,nodes,terminals,t)
        else:
            t[0]='o'  # replace place-holder
    else:
        raise RuntimeError('buildIDtreeFromDnodes: error')
    return t
"""
example:
t000 = []
root00 = []
nts00 = [[0],[0,0],[0,1],[1],[1,0]]
heapq.heapify(nts00)
ts00 = [([0,0],[('the',[(0,0)])]), ([0,1],[('bird',[(0,0)])]), ([1,0],[('sings',[(0,0)])])]
heapq.heapify(ts00)
t00 = []
t000 = buildIDtreeFromDnodes(root00,nts00,ts00,t00)
pptree(t000)
"""

def dNodes2idtree(dns):
    nonterms = []
    terms = []
    splitDnodes(nonterms,terms,dns)
    if len(nonterms) == 0:
        raise RuntimeError('buildIDtreeFromDnodes: error')
    else:
        heapq.heapify(nonterms)
        heapq.heapify(terms)
        root = heapq.heappop(nonterms)
        t = buildIDtreeFromDnodes(root,nonterms,terms,[])
        if len(terms)<>0 or len(nonterms)<>0:
            print 'dNodes2idtree error: unused derivation steps'
            print 'terms=',terms
            print 'nonterms=',nonterms
        return t

"""
Now to the guts of the parser.
For the queue of parses, we put the probability first, and negate it,
  (-probability,input,iq)
so the most probable element will be the most negative, and hence the
minimal element popped at each step.
"""
def printDQ(lexArrays,dq):
    for (p,inpt,iq,dns) in dq: # dns is the dnode list
        print p,': ',' '.join(inpt)
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
Check to make sure the mover array (or any array)
consists only of empty lists
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
Divide the list of trees ts into the terminals (lexical elements)
and the nonterminals
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

"""
Now the parsing steps: scan, merge1/2/3/4, move1/2
"""
def scan(w,inpt,m,mx,dt,sofar):
    if emptyListArray(sofar):
        (ok,remainderInt) = prefixT(w,inpt)
        if ok:
            exp = (w,[(([],m),([],mx),dt)])  # unlike recognizer, we return w here
            sofar.append(exp)

# merge a (non-moving) complement
def merge1(lA,inpt,terms,i,((h,m),(hx,mx),dt),sofar): # dt=(ifs,dx,mifs)
    if terms <> []:
        new_head_index=hx[:]
        new_head_index.append(0)
        new_comp_index=hx[:]
        new_comp_index.append(1)
        empty_m = [[]]*len(m)
        empty_mx = [[]]*len(mx)
        ifs = dt[0][:] # copy
        ifs.append((1,i)) # extend ifs
        dx = dt[1][:] # copy
        dx.append(0) # extend dx
        empty_mifs = [[]]*len(m)
        dt1 = (ifs,dx,empty_mifs)
        ic1 = ((terms,empty_m),(new_head_index,empty_mx),dt1) # no movers to lexical head
        new_ifs = [(0,i)]
        new_dx = dt[1][:] # copy
        new_dx.append(1) # extend new_dx
        mifs = dt[2][:] # copy
        dt2 = (new_ifs,new_dx,mifs) # movers to complement
        ic2 = ((lA[i],m),(new_comp_index,mx),dt2) # movers to complement only
        exp = ([],[ic1,ic2])
        sofar.append(exp)

# merge a (non-moving) specifier
def merge2(lA,inpt,nonterms,i,((h,m),(hx,mx),dt),sofar): # dt=(ifs,dx,mifs)
    if nonterms <> []:
        new_head_index=hx[:]
        new_head_index.append(1)
        new_comp_index=hx[:]
        new_comp_index.append(0)
        empty_m = [[]]*len(m)
        empty_mx = [[]]*len(mx)
        ifs = dt[0][:] # copy
        ifs.append((1,i)) # extend ifs
        dx = dt[1][:] # copy
        dx.append(0) # extend dx
        mifs = dt[2][:] # copy
        dt1 = (ifs,dx,mifs)
        ic1 = ((nonterms,m),(new_head_index,mx),dt1) # movers to head
        new_ifs = [(0,i)]
        new_dx = dt[1][:] # copy
        new_dx.append(1) # extend new_dx
        empty_mifs = [[]]*len(m)
        dt2 = (new_ifs,new_dx,empty_mifs)
        ic2 = ((lA[i],empty_m),(new_comp_index,empty_mx),dt2) # no movers to spec
        exp = ([],[ic1,ic2])
        sofar.append(exp)

# merge a (moving) complement
def merge3(inpt,terms,i,((h,m),(hx,mx),dt),sofar):
    if terms <> []:
        for nxt in range(len(m)):
            (ok,matchingTree) = memberFval(i,m[nxt])
            if ok:
                ts = matchingTree[1:]
                tsx = mx[nxt]
                ifs0 = dt[2][nxt][:]
                empty_m = [[]]*len(m)
                empty_mx = [[]]*len(mx)      
                empty_mifs = [[]]*len(m)
                n = m[:]
                nx = mx[:]
                nifs = dt[2][:] # copy
                n[nxt] = [] # we used the "next" licensee, so now empty
                nx[nxt] = []
                nifs[nxt] = []
                ifs = dt[0][:] # copy
                ifs.append((1,i)) # extend ifs with (sel i)
                dx = dt[1][:] # copy
                dx.append(0) # extend dx
                dt1 = (ifs,dx,empty_mifs)
                ic1 = ((terms,empty_m),(hx,empty_mx),dt1)
                ifs0.append((0,i)) # add (cat i) feature
                new_dx = dt[1][:] # copy
                new_dx.append(1) # extend new_dx
                dt2 = (ifs0,new_dx,nifs) # movers to complement
                ic2 = ((ts,n),(tsx,nx),dt2) # movers passed to complement
                exp = ([],[ic1,ic2])
                sofar.append(exp)

# merge a (moving) specifier
def merge4(inpt,nonterms,i,((h,m),(hx,mx),dt),sofar):
    if nonterms <> []:
        for nxt in range(len(m)):
            (ok,matchingTree) = memberFval(i,m[nxt])
            if ok:
                ts = matchingTree[1:]
                tsx = mx[nxt]
                ifs0 = dt[2][nxt][:] # copy
                empty_m = [[]]*len(m)
                empty_mx = [[]]*len(mx)                
                empty_mifs = [[]]*len(m)
                n = m[:]
                nx = mx[:]
                nifs = dt[2][:]
                n[nxt] = [] # we used the "next" licensee, so now empty
                nx[nxt] = []
                nifs[nxt] = []
                ifs = dt[0][:] # copy
                ifs.append((1,i)) # extend ifs
                dx = dt[1][:] # copy
                dx.append(0) # extend dx
                dt1 = (ifs,dx,nifs)
                ic1 = ((nonterms,n),(hx,nx),dt1)
                ifs0.append((0,i))
                new_dx = dt[1][:] # copy
                new_dx.append(1) # extend new_dx
                dt2 = (ifs0,new_dx,empty_mifs)
                ic2 = ((ts,empty_m),(tsx,empty_mx),dt2) # movers passed to complement
                exp = ([],[ic1,ic2])
                sofar.append(exp)

def move1(lA,inpt,ts,i,((h,m),(hx,mx),dt),sofar):
    if m[i] == []:  # SMC
        n = m[:]
        nx = mx[:]
        n[i] = lA[i]
        nx[i] = hx[:]
        nx[i].append(0)
        new_head_index=hx[:]
        new_head_index.append(1)
        ifs = dt[0][:] # copy
        ifs.append((3,i)) # extend ifs with (pos i)
        dx = dt[1][:] # copy
        dx.append(0) # extend dx
        mifs = dt[2][:] # copy
        mifs[i] = [(2,i)] # begin new mover with (neg i)
        dt1 = (ifs,dx,mifs)
        ic1 = ((ts,n),(new_head_index,nx),dt1)
        exp = ([],[ic1])
        sofar.append(exp)

def move2(inpt,ts,i,((h,m),(hx,mx),dt),sofar):
    for nxt in range(len(m)):
        (ok,matchingTree) = memberFval(i,m[nxt])
        if ok:
            rootF = matchingTree[0][1] # value of rootLabel
            if rootF==nxt or m[rootF]==[]: # SMC
                mts = matchingTree[1:][:]
                mtsx = mx[nxt][:]
                ifs0 = dt[2][nxt][:]
                n = m[:]
                nx = mx[:]
                nifs = dt[2][:]
                n[nxt] = [] # we used the "next" licensee, so now empty
                nx[nxt] = []
                nifs[nxt] = []
                n[rootF] = mts
                nx[rootF] = mtsx
                ifs0.append((2,i)) # extend prev ifs of mover with (neg i)
                nifs[rootF] = ifs0
                ifs = dt[0][:]
                ifs.append((3,i)) # extend ifs with (pos i)
                dx = dt[1][:]
                dx.append(0) # extend dx
                dt1 = (ifs,dx,nifs)
                ic1 = ((ts,n),(hx,nx),dt1)
                exp = ([],[ic1])
                sofar.append(exp)
"""
The following function collects all expansion of the leftmost
indexed category ((h,m),(hx,mx),dt) popped off the iq of predictions
that pops off dq -- the most probable derivation
"""
def exps((sA,lA,tA),inpt,((h,m),(hx,mx),dt),sofar):
    for t in h:
        if len(t)>0 and isinstance(t[0],tuple):
            if t[0][0] == 1: # feature type 1 is 'sel'
                i = t[0][1] # set i to feature value
                (terms,nonterms)= terminalsOf(t[1:])
                merge1(lA,inpt,terms,i,((h,m),(hx,mx),dt),sofar)
                merge2(lA,inpt,nonterms,i,((h,m),(hx,mx),dt),sofar)
                merge3(inpt,terms,i,((h,m),(hx,mx),dt),sofar)
                merge4(inpt,nonterms,i,((h,m),(hx,mx),dt),sofar)
            elif t[0][0] == 3: # feature type 3 is 'pos'
                i = t[0][1] # set i to feature value
                ts = t[1:]
                move1(lA,inpt,ts,i,((h,m),(hx,mx),dt),sofar)
                move2(inpt,ts,i,((h,m),(hx,mx),dt),sofar)
            else:
                raise RuntimeError('exps')
        else:
            scan(t,inpt,m,mx,dt,sofar)

# unlike recognizer, we pass in inpt, because now scan returns w
#  and all other rules return [] as first element of exp pair
def insertNewParses(inpt,p,new_p,q,dq,dns0,exps):
    for exp in exps:
        # scan is a special case, identifiable by empty head
        # (w,[(([],m),([],mx),(ifs,dx,mifs))]) <-- we check for that empty head
        if exp[1][0][0][0]==[]:
            dns = dns0[:]
            w = exp[0]
            ifs = exp[1][0][2][0][:] # copy
            ifs.reverse()
            dx = exp[1][0][2][1]
            dns.append((dx,(w,ifs)))
            (ok,remainderInt) = prefixT(w,inpt) # unlike recognizer, we compute remainder again
            newParse = (p,inpt[remainderInt:],q,dns)
            heapq.heappush(dq,newParse)
        else: # put indexed categories ics onto iq with new_p
            ics = exp[1]
            safe_q=q[:]
            dns = dns0[:]
            for ic in ics: # ic = ((h,m),(hx,mx),(ifs,dx,mifs))
                dx = ic[2][1]
                dns.append(dx)
                newIndex=minIndex(ic)
                heapq.heappush(safe_q,(newIndex,ic))
            newParse = (new_p,inpt,safe_q,dns)
            heapq.heappush(dq,newParse)

"""
This is the core function: it pops the most probable parse,
pops the leftmost prediction from that parse,
expands that prediction in all possible ways,
pushes the new parses into the queue, and repeats,
until success or failure.
"""
def derive(lexArrays,minP,dq): # modify this to return dq, so alternative parses can be found
    p = 1.0
#    while len(dq) > 0 and len(dq) < 2000:
    while len(dq) > 0:
#        printDQ(lexArrays,dq)
        (p,inpt,iq,dns) = heapq.heappop(dq)
        print '# of parses in beam=',len(dq)+1,', p(best parse)=',(-1 * p)
        if len(iq)==0 and len(inpt)==0:
            print 'parse found'
            return dns  # success!
        elif len(iq) > 0:
            prediction = heapq.heappop(iq)
            ic = prediction[1]
            sofar = []
            exps(lexArrays,inpt,ic,sofar)
            if len(sofar) > 0:
                new_p = p / float(len(sofar))
                if new_p < minP:
                    insertNewParses(inpt,p,new_p,iq,dq,dns,sofar)
                else:
                    print 'improbable parses discarded'
    print 'no parse found'
    return [[],([],(['no parse'],[]))]  # failure!

"""
Convert the integer representation of features back to
the string representations for the derivation tree
"""
def idtree2dtree(sA,t):
    if t[0]=='*':
        dt0 = idtree2dtree(sA,t[1])
        dt1 = idtree2dtree(sA,t[2])
        return ['*',dt0,dt1]
    elif t[0]=='o':
        dt0 = idtree2dtree(sA,t[1])
        return ['o',dt0]
    elif isinstance(t,list) and len(t)==1:
        dt0 = idtree2dtree(sA,t[0])
        return dt0
    else: # leaf t has the form (w,ifs)
        return (t[0],[fOfInts(sA,f) for f in t[1]])

"""
Convert the derivation tree with string features
into a "beautified' derivation tree for display
"""
def dt2t(t):
    if t[0]=='*':
        dt0 = dt2t(t[1])
        dt1 = dt2t(t[2])
        return ['*',dt0,dt1]
    elif t[0]=='o':
        dt0 = dt2t(t[1])
        return ['o',dt0]
    else: # leaf t has the form (w,ifs)
        return (t[0],[btfyFeat(f) for f in t[1]])

"""
Initialize and start the parse
"""
def parse(lex,start,minP,inpt): # initialize and begin
    sA = stringValsOfG(lex)
    (lA,tA) = gIntoLexArrayTypeArray(sA,lex)
    startInt = intsOfF(sA,('cat',start))[1]
    h = lA[startInt]
    m = [[]]*len(sA)
    mx = [[]]*len(sA)
    ifs = [(0,startInt)]    # for derivation tree, (int) features checked so far
    dx = []                 # for derivation tree, Gorn address of the current node (int list)
    mifs = [[]]*len(sA)     # for derivation tree, (int) features checked by each mover
    dt = (ifs,dx,mifs)      # dt = dtuple for derivation tree
    ic = ((h,m),([],mx),dt)
    iq = [([],ic)]
    heapq.heapify(iq)
    dq = [(-1.0,inpt,iq,[[]])]
    heapq.heapify(dq)
    t0 = time.time()
    dns = derive((sA,lA,tA),minP,dq)
#    print 'dns=',dns
    t1 = time.time()
    idtree = dNodes2idtree(dns)
#    print 'idtree =',idtree
#    print 'calling idtree2dtree(sA,',idtree,')'
    dt = idtree2dtree(sA,idtree)
    print t1 - t0, "seconds"
    return dt

"""
  these examples use merge1, merge2, merge3, move1
inpt0=['which','wine','the','queen','prefers']
parse(mg0,'C',0.001,inpt0)

inpt0=['which','wine','prefers','the','wine']
parse(mg0,'C',0.001,inpt0)

inpt0=['which','wine','the','queen','prefers']
parse(mg0,'C',0.001,inpt0)

inpt0=['the','king','knows','which','wine','the','queen','prefers']
parse(mg0,'C',0.001,inpt0)

  these examples use merge1, merge2, merge4, move1
inpt0=['the','king','knows','which','queen','prefers','the','wine']
parse(mg0,'C',0.001,inpt0)

the queen says the king knows which queen prefers the wine
inpt0=['the','queen','says','the','king','knows','which','queen','prefers','the','wine']
parse(mg0,'C',0.001,inpt0)

inpt0=['a','a']
parse(mgxx,'T',0.001,inpt0)
"""

"""
Now we define some read-parse loops
"""
def go1(lex,start,minP): # initialize and begin
    sA = stringValsOfG(lex)
    (lA,tA) = gIntoLexArrayTypeArray(sA,lex)
    gA = (sA,lA,tA)
    startInt = intsOfF(sA,('cat',start))[1]
    h = lA[startInt]
    m = [[]]*len(sA)
    mx = [[]]*len(sA)
    ifs = [(0,startInt)]    # for derivation tree
    dx = []                 # for derivation tree
    mifs = [[]]*len(sA)     # for derivation tree
    dt = (ifs,dx,mifs)      # for derivation tree
    ic = ((h,m),([],mx),dt) # dt = dtuple for derivation tree
    iq = [([],ic)]
    heapq.heapify(iq)
    goLoop(lex,(sA,lA,tA),iq,minP)

def goLoop(g,gA,iq,minP):
    new_iq = iq[:]
    ans = raw_input(': ')
    inpt = ans.split()
    print 'inpt =',inpt
    dq = [(-1.0,inpt,new_iq,[[]])]
    heapq.heapify(dq)
    go2(g,gA,iq,minP,dq)

def go2(g,gA,iq,minP,dq):
    t0 = time.time()
    dns = derive(gA,minP,dq)
    t1 = time.time()
    idtree = dNodes2idtree(dns)
    dt = idtree2dtree(gA[0],idtree)
    print t1 - t0, "seconds"
    ans = ''
    while ans != 'quit':
        ans = str(raw_input('(h for help): '))
        if len(ans)>0 and ans[0] == 'h':
            hlp()
        elif len(ans)>0 and ans[0] == 'd':
            TreeView(list2nltktree(dt2t(dt)))
        elif len(ans)>0 and ans[0] == 's':
            TreeView(list2nltktree(st2t(dt2st(dt))))
        elif len(ans)>0 and ans[0] == 'b':
            TreeView(list2nltktree(bt2t(dt2bt(dt))))
        elif len(ans)>0 and ans[0] == 'x':
            TreeView(list2nltktree(dt2xb(dt)))
        elif len(ans)>0 and ans[0] == 'l':
            TreeView(list2nltktree(['.']+lexArrays2stringTrees(gA)))
        elif len(ans)>0 and ans[0] == 'n':
            goLoop(g,gA,iq,minP)
        elif len(ans)>0 and ans[0] == ';':
            go2(g,gA,iq,minP,dq)
        elif len(ans)>1 and ans[:2] == 'pd':
            pptree(dt2t(dt))
        elif len(ans)>1 and ans[:2] == 'ps':
            pptree(st2t(dt2st(dt)))
        elif len(ans)>1 and ans[:2] == 'pb':
            pptree(bt2t(dt2bt(dt)))
        elif len(ans)>1 and ans[:2] == 'px':
            pptree(dt2xb(dt))
        elif len(ans)>1 and ans[:2] == 'pg':
            print
            showGrammar(g)
            print
        elif len(ans)>1 and ans[:2] == 'pl':
            pptree(['.']+lexArrays2stringTrees(gA))
        elif len(ans)>0 and ans[0] == 'q':
            sys.exit()
#            ans = 'quit'
    sys.exit()

def hlp():
    print '  d for derivation tree'
    print '  s for state tree'
    print '  b for bare tree'
    print "  x for X' tree"
    print '  l show the grammar as a tree'
    print '  ; to search for another parse of this same input'
    print '  n to type in next input'
    print '  q for quit'
    print
    print 'or for line mode interaction:'
    print '  pd for pretty-printed derivation tree'
    print '  ps for pretty-printed state tree'
    print '  pb for pretty-printed bare tree'
    print "  px for pretty-printed X' tree"
    print '  pl pretty-print the grammar as a tree'
    print '  pg print the grammar as a list of items'
    print

"""
convert derivation tree to state tree
"""
# remember:
# fs[0] = head features, fs[0][0] = 1st feature of head, fs[0][0][0] = type of 1st feature
def merge_check(fs0,fs1):
    if fs0[0][0][0] == 'sel' and fs1[0][0][0] == 'cat' and fs0[0][0][1] == fs1[0][0][1]:
        newfs = [fs0[0][1:]] # remaining head1 features
        if fs1[0][1:] <> []: # any remaining features from head2? If so, they're movers
            newfs.append(fs1[0][1:])
        newfs.extend(fs0[1:]) # add movers1
        newfs.extend(fs1[1:]) # add movers2
        return newfs
    else:
        raise RuntimeError('merge_check error')

def getMover(f,moverFeatureLists):
    mover = []
    remainder = []
    for moverFeatureList in moverFeatureLists:
        if moverFeatureList[0][1] == f:
            if mover == []:  # OK if we did not already find an f
                mover = [moverFeatureList[1:]] # put remainder into singleton list
            else: # if we find 2 f's, there is an SMC violation
                raise RuntimeError('SMC violation in move_check')
        else: # put others back into remainder list
            remainder.extend(moverFeatureList)
    if mover == []:
        raise RuntimeError('getMover error: no mover found')
    else:
        return (mover,remainder)
    
def move_check(fs0):
    newfs = [fs0[0][1:]] # remaining head1 features
    (mover,remainder) = getMover(fs0[0][0][1],fs0[1:])
    if remainder<>[]: # put features of other movers back into list, if any
        newfs.append(remainder)
    if mover[0]<>[]: # if this mover is movign again, put it into list too
        newfs.append(mover[0]) # extend movers1 with movers2
    return newfs

# if we want just the state: dt2s
def dt2s(dt):
    if isinstance(dt,tuple):
        return [dt[1]]  # the state of leaf (w,fs) is [fs]; root of tree [[fs]]
    elif dt[0]=='*':      # all other states result from feature checking
        fs0 = dt2s(dt[1])
        fs1 = dt2s(dt[2])
        fs = merge_check(fs0,fs1)
        return fs
    elif dt[0]=='o':
        fs0 = dt2s(dt[1])
        fs = move_check(fs0)
        return fs

def dt2st(dt):
    if isinstance(dt,tuple):
        return [[dt[1]]]  # the state of leaf (w,fs) is [fs]; root of tree [[fs]]
    elif dt[0]=='*':      # all other states result from feature checking
        t0 = dt2st(dt[1])
        t1 = dt2st(dt[2])
        fs = merge_check(t0[0],t1[0])
        return [fs,t0,t1]
    elif dt[0]=='o':
        t0 = dt2st(dt[1])
        fs = move_check(t0[0])
        return [fs,t0]

def st2t(st):
    if len(st)==3: # merge
        sfs = ','.join([' '.join([btfyFeat(f) for f in fs]) for fs in st[0]])
        t0 = st2t(st[1])
        t1 = st2t(st[2])
        return [sfs,t0,t1]
    elif len(st)==2: # move
        sfs = ','.join([' '.join([btfyFeat(f) for f in fs]) for fs in st[0]])
        t0 = st2t(st[1])
        return [sfs,t0]
    else: # len(st)==1: # leaf
        sfs = ','.join([' '.join([btfyFeat(f) for f in fs]) for fs in st[0]])
        return [sfs]

"""
convert derivation tree to bare tree -
  we modify dt2s, adding the bare trees and the list of moving trees.
"""
def dt2bt(dt):
    (state,bt,movers) = dt2sbt(dt)
    if movers <> []:
        raise RuntimeError('dt2bt error')
    else:
        return bt

# remember:
# fs[0] = head features, fs[0][0] = 1st feature of head, fs[0][0][0] = type of 1st feature
def merge_checkBT(fs0,fs1,bt1,m):
    if fs0[0][0][0] == 'sel' and fs1[0][0][0] == 'cat' and fs0[0][0][1] == fs1[0][0][1]:
        newfs = [fs0[0][1:][:]] # copy remaining head1 features
        newfs.extend(fs0[1:][:]) # add movers1
        newfs.extend(fs1[1:][:]) # add movers2
        if fs1[0][1:] <> []: # any remaining features from head2? If so, new mover
            bt1m = ('',[]) # trace
            newfs.append(fs1[0][1:])
            m.append((fs1[0][1:],bt1))
        else:
            bt1m = bt1
        return (newfs,bt1m)
    else:
        raise RuntimeError('merge_check error')

def getMoverBT(f,moverFeatureLists,treeList):
    mover = []
    remainder = []
    remainderTrees = []
    for fs in moverFeatureLists:
        if fs[0][1] == f:
            if mover == []:  # OK if we did not already find an f
                mover = [fs[1:]] # put remainder into singleton list
            else: # if we find 2 f's, there is an SMC violation
                raise RuntimeError('SMC violation in move_check')
        else:
            remainder.extend(fs)
    for (fs,t) in treeList:
        if fs[0][1] == f: # return copy of moving tree
            moverTree = t[:]
        else: # add copy of any other trees
            remainderTrees.append((fs,t[:]))
    return (mover,moverTree,remainder,remainderTrees)
    
def move_checkBT(fs0,m):
    newfs = [fs0[0][1:]] # remaining head1 features
    (mover,moverTree,remainder,remainderTrees) = getMoverBT(fs0[0][0][1],fs0[1:],m)
    if remainder<>[]: # put other mover features back into list, if any
        newfs.append(remainder)
    if mover[0]<>[]: # if this mover is moving again, put it into list too
        newfs.append(mover[0]) # extend movers1 with movers2
        mt = ('',[]) # trace
        remainderTrees.append((mover[0],moverTree))
    else:
        mt = moverTree
    return (newfs,mt,remainderTrees)

def dt2sbt(dt): # compute (state,bt,moving) triple
    if isinstance(dt,tuple):
        return ([dt[1]],dt,[])  # the state of leaf (w,fs) is [fs]; root of tree [[fs]]
    elif dt[0]=='*':         # all other states result from feature checking
        (fs0,bt0,m0) = dt2sbt(dt[1])
        (fs1,bt1,m1) = dt2sbt(dt[2])
        m = m0
        m.extend(m1)
        (fs,bt1m) = merge_checkBT(fs0,fs1,bt1,m) # may add additional mover to m
        if isinstance(bt0,tuple):
            bt = ['<',bt0,bt1m]
        else:
            bt = ['>',bt1m,bt0]
        return (fs,bt,m)
    elif dt[0]=='o':
        (fs0,bt0,m0) = dt2sbt(dt[1])
        (fs,mt,m) = move_checkBT(fs0,m0)
        bt = ['>',mt,bt0]
        return (fs,bt,m)

def bt2t(bt):
    if isinstance(bt,tuple): # leaf
        w = ' '.join(bt[0])
        sfs = ' '.join([btfyFeat(f) for f in bt[1]])
        item = '::'.join([w,sfs])
        return item
    elif len(bt)==3: # merge
        t0 = bt2t(bt[1])
        t1 = bt2t(bt[2])
        return [bt[0],t0,t1]
    elif len(bt)==2: # move
        t0 = bt2t(bt[1])
        return [bt[0],t0]
    else:
        raise RuntimeError('bt2t')

"""
convert derivation tree to X-bar tree -
  similar to the bare tree conversion
"""
def dt2xb(dt):
#   (state,xb,movers,cat,lexical,cntr) = dt2sxb(dt,0)
    tple = dt2sxb(dt,0)
    xb = tple[1]
    movers = tple[2]
    cat = tple[3]
    scat = cat+'P'
    xb[0] = scat
    if movers <> []:
        raise RuntimeError('dt2xb error')
    else:
        return xb

# remember:
# fs[0] = head features, fs[0][0] = 1st feature of head, fs[0][0][0] = type of 1st feature
def merge_checkXB(fs0,fs1,xb1,m,cat,cntr):
    if fs0[0][0][0] == 'sel' and fs1[0][0][0] == 'cat' and fs0[0][0][1] == fs1[0][0][1]:
        newfs = [fs0[0][1:][:]] # copy remaining head1 features
        newfs.extend(fs0[1:][:]) # add movers1
        newfs.extend(fs1[1:][:]) # add movers2
        if fs1[0][1:] <> []: # any remaining features from head2? If so, new mover
            scat = cat+'P('+str(cntr)+')'
            cntr0 = cntr + 1
            xb1m = ([scat],[]) # trace
            newfs.append(fs1[0][1:])
            xb1[0] = scat
            m.append((fs1[0][1:],xb1))
        else:
            cntr0 = cntr
            xb1m = xb1
        return (newfs,xb1m,cntr0)
    else:
        raise RuntimeError('merge_check error')

def getMoverXB(f,moverFeatureLists,treeList):
    mover = []
    remainder = []
    remainderTrees = []
    for fs in moverFeatureLists:
        if fs[0][1] == f:
            if mover == []:  # OK if we did not already find an f
                mover = [fs[1:]] # put remainder into singleton list
            else: # if we find 2 f's, there is an SMC violation
                raise RuntimeError('SMC violation in move_check')
        else:
            remainder.extend(fs)
    for (fs,t) in treeList:
        if fs[0][1] == f: # return copy of moving tree
            moverTree = t[:]
        else: # add copy of any other trees
            remainderTrees.append((fs,t[:]))
    return (mover,moverTree,remainder,remainderTrees)
    
def move_checkXB(fs0,m):
    newfs = [fs0[0][1:]] # remaining head1 features
    (mover,moverTree,remainder,remainderTrees) = getMoverXB(fs0[0][0][1],fs0[1:],m)
    if remainder<>[]: # put other mover features back into list, if any
        newfs.append(remainder)
    if mover[0]<>[]: # if this mover is moving again, put it into list too
        newfs.append(mover[0]) # extend movers1 with movers2
        mt = ('',[]) # trace
        remainderTrees.append((mover[0],moverTree))
    else:
        mt = moverTree
    return (newfs,mt,remainderTrees)

def catOf(fs):
    for (ftype,f) in fs:
        if ftype == 'cat':
            return f[:]

def dt2sxb(dt,cntr): # compute (state,xb,moving) triple
    if isinstance(dt,tuple):
        cat = catOf(dt[1])
        return ([dt[1]],[cat,dt[0]],[],cat,True,cntr)
    elif dt[0]=='*':         # all other states result from feature checking
        (fs0,xb0,m0,cat0,lexical0,cntr0) = dt2sxb(dt[1],cntr)
        (fs1,xb1,m1,cat1,lexical1,cntr1) = dt2sxb(dt[2],cntr0)
        m = m0
        m.extend(m1)
        (fs,xb1m,cntr2) = merge_checkXB(fs0,fs1,xb1,m,cat1,cntr1) # may add additional mover to m
        hcat = cat0+"'"
        scat = cat1+'P'
        if isinstance(xb1m,list): # a trace is a pair
            xb1m[0] = scat
        if lexical0:
            xb = [hcat,xb0,xb1m]
        else:
            xb = [hcat,xb1m,xb0]
        return (fs,xb,m,cat0,False,cntr2)
    elif dt[0]=='o':
        (fs0,xb0,m0,cat0,lexical0,cntr0) = dt2sxb(dt[1],cntr)
        (fs,mt,m) = move_checkXB(fs0,m0)
        hcat = cat0+"'"
        xb = [cat0,mt,xb0]
        return (fs,xb,m,cat0,False,cntr0)

"""
http://docs.python.org/library/functions.html#__import__
 'Direct use of __import__() is rare, except in cases where you want to
  import a module whose name is only known at runtime.'
"""
if len(sys.argv)<>4:
    print '\nFound',len(sys.argv)-1,'parameters but',
    print '3 parameters are required:'
    print '    mgtdbp.py grammar startCategory minimumProbability\n'
    print 'For example:'
    print '    python mgtdbp.py mg0 C 0.0001'
    print 'or:'
    print '    python mgtdbp.py mgxx T 0.0000001\n'
    print 'For line editing, you could use:'
    print '    rlwrap python mgtdbp.py mg0 C 0.0001'
    print 'or, if you have ipython:'
    print '    ipython mgtdbp.py mg0 C 0.0001\n'
    sys.exit()

# from the grammar specified in sys.argv[1], import *
grammar=__import__(sys.argv[1], globals(), locals(), ['*'])

# with mgxx, p=0.000000001 is small enough for: a b b b b a b b b b
# with mg0, p=0.0001 is small enough for:
# which king says which queen knows which king says which wine the queen prefers

go1(grammar.g,sys.argv[2],-1 * float(sys.argv[3]))
