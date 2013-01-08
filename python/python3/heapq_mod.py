"""This code has been taken from the heapq source code available at

http://docs.python.org/release/3.1.5/_sources/library/heapq.txt

Modified to compare only the first element of both items being compared in
the heap, in the function 'siftdown'. This functionality is highly specific
to the needs of the derivation queue in the mgtdbp file. Functions that have
not been used have been removed for efficiency's sake. Some of the comments
from the original code remain for debugging purposes, and general knowledge.
Modified by:  Erik Arrieta || Latest version: 9/12/12
Modified by:  Ed Stabler || Latest version: 1/5/13
"""

"""Heap queue algorithm (a.k.a. priority queue).
      
     3 Heaps are arrays for which a[k] <= a[2*k+1] and a[k] <= a[2*k+2] for
     4 all k, counting elements from 0.  For the sake of comparison,
     5 non-existing elements are considered to be infinite.  The interesting
     6 property of a heap is that a[0] is always its smallest element.
     7 
     8 Usage:
     9 
    10 heap = []            # creates an empty heap
    11 heappush(heap, item) # pushes a new item on the heap
    12 item = heappop(heap) # pops the smallest item from the heap
    13 item = heap[0]       # smallest item on the heap without popping it
    14 heapify(x)           # transforms list into a heap, in-place, in linear time
    15 item = heapreplace(heap, item) # pops and returns smallest item, and adds
    16                                # new item; the heap size is unchanged
    17 
    18 Our API differs from textbook heap algorithms as follows:
    19 
    20 - We use 0-based indexing.  This makes the relationship between the
    21   index for a node and the indexes for its children slightly less
    22   obvious, but is more suitable since Python uses 0-based indexing.
    23 
    24 - Our heappop() method returns the smallest item, not the largest.
    25 
    26 These two make it possible to view the heap as a regular Python list
    27 without surprises: heap[0] is the smallest item, and heap.sort()
    28 maintains the heap invariant!
    29 """

# Original code by Kevin O'Connor, augmented by Tim Peters and Raymond Hettinger

__all__ = ['heappush', 'heappop', 'heapify']

from itertools import islice, repeat, count, tee, chain
import bisect

def heappush(heap, item):
    """Push item onto heap, maintaining the heap invariant."""
    heap.append(item)
    _siftdown(heap, 0, len(heap)-1)

def heappop(heap):
    """Pop the smallest item off the heap, maintaining the heap invariant."""
    lastelt = heap.pop()    # raises appropriate IndexError if heap is empty
    if heap:
        returnitem = heap[0]
        heap[0] = lastelt
        _siftup(heap, 0)
    else:
        returnitem = lastelt
    return returnitem

def heapify(x):
    """Transform list into a heap, in-place, in O(len(x)) time."""
    n = len(x)
    # Transform bottom-up.  The largest index there's any point to looking at
    # is the largest with a child index in-range, so must have 2*i + 1 < n,
    # or i < (n-1)/2.  If n is even = 2*j, this is (2*j-1)/2 = j-1/2 so
    # j-1 is the largest, which is n//2 - 1.  If n is odd = 2*j+1, this is
    # (2*j+1-1)/2 = j so j-1 is the largest, and that's again n//2-1.
    for i in reversed(range(n//2)):
        _siftup(x, i)

def _siftdown(heap, startpos, pos):
    newitem = heap[pos]
    # Follow the path to the root, moving parents down until finding a place
    # newitem fits.
    while pos > startpos:
        parentpos = (pos - 1) >> 1
        parent = heap[parentpos]
        if (newitem[0] == parent[0]):   #added EA
            break
        if newitem < parent:
            heap[pos] = parent
            pos = parentpos
            continue
        break
    heap[pos] = newitem
''' 
   250 # The child indices of heap index pos are already heaps, and we want to make
   251 # a heap at index pos too.  We do this by bubbling the smaller child of
   252 # pos up (and so on with that child's children, etc) until hitting a leaf,
   253 # then using _siftdown to move the oddball originally at index pos into place.
   254 #
   255 # We *could* break out of the loop as soon as we find a pos where newitem <=
   256 # both its children, but turns out that's not a good idea, and despite that
   257 # many books write the algorithm that way.  During a heap pop, the last array
   258 # element is sifted in, and that tends to be large, so that comparing it
   259 # against values starting from the root usually doesn't pay (= usually doesn't
   260 # get us out of the loop early).  See Knuth, Volume 3, where this is
   261 # explained and quantified in an exercise.
   262 #
   263 # Cutting the # of comparisons is important, since these routines have no
   264 # way to extract "the priority" from an array element, so that intelligence
   265 # is likely to be hiding in custom comparison methods, or in array elements
   266 # storing (priority, record) tuples.  Comparisons are thus potentially
   267 # expensive.
   268 #
   269 # On random arrays of length 1000, making this change cut the number of
   270 # comparisons made by heapify() a little, and those made by exhaustive
   271 # heappop() a lot, in accord with theory.  Here are typical results from 3
   272 # runs (3 just to demonstrate how small the variance is):
   273 #
   274 # Compares needed by heapify     Compares needed by 1000 heappops
   275 # --------------------------     --------------------------------
   276 # 1837 cut to 1663               14996 cut to 8680
   277 # 1855 cut to 1659               14966 cut to 8678
   278 # 1847 cut to 1660               15024 cut to 8703
   279 #
   280 # Building the heap by using heappush() 1000 times instead required
   281 # 2198, 2148, and 2219 compares:  heapify() is more efficient, when
   282 # you can use it.
   283 #
   284 # The total compares needed by list.sort() on the same lists were 8627,
   285 # 8627, and 8632 (this should be compared to the sum of heapify() and
   286 # heappop() compares):  list.sort() is (unsurprisingly!) more efficient
   287 # for sorting.
'''
def _siftup(heap, pos):
    endpos = len(heap)
    startpos = pos
    newitem = heap[pos]
    # Bubble up the smaller child until hitting a leaf.
    childpos = 2*pos + 1    # leftmost child position
    while childpos < endpos:
        # Set childpos to index of smaller child.
        rightpos = childpos + 1
#       if rightpos < endpos and not heap[childpos] < heap[rightpos]: # ES catch tuple case here
        if rightpos < endpos: # ES
            if isinstance(heap[childpos],tuple) and isinstance(heap[rightpos],tuple): # ES
                if not heap[childpos][0] < heap[rightpos][0]: # ES
                    childpos = rightpos
            elif not heap[childpos] < heap[rightpos]: # ES
                childpos = rightpos
        # Move the smaller child up.
        heap[pos] = heap[childpos]
        pos = childpos
        childpos = 2*pos + 1
    # The leaf at pos is empty now.  Put newitem there, and bubble it up
    # to its final resting place (by sifting its parents down).
    heap[pos] = newitem
    _siftdown(heap, startpos, pos)
