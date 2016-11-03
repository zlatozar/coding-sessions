#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Priority queues p.162

# ___________________________________________________________
#                                                      NOTES

# In HEAP_EXTRACT_MAX we remove the biggest A[0] and the interesting part is that the
# smallest element is assigned. We do that to keep the structure without gaps and easily
# to heapify the resulted array.

# ___________________________________________________________
#                                             IMPLEMENTATION

import sys
from max_heap import PARENT, MAX_HEAPIFY, BUILD_MAX_HEAP

def HEAP_MAXIMUM(A):
    return A[0]

def HEAP_EXTRACT_MAX(A):
    heap_size = len(A) - 1

    # guards for empty A
    if heap_size < 0:
        raise IndexError("Heap underflow")

    max = A[0]
    A[0] = A[heap_size]         # !!!
    del A[-1]
    MAX_HEAPIFY(A, 0, heap_size - 1)

    return max

def HEAP_INCREASE_KEY(A, i, key):
    if key < A[i]:
        raise ValueError('New key is small than current key')

    A[i] = key
    while i > 0 and A[PARENT(i)] < A[i]:
        A[i], A[PARENT(i)] = A[PARENT(i)], A[i]
        i = PARENT(i)

def MAX_HEAP_INSERT(A, key):
    A.append(-sys.maxint)
    heap_size = len(A) - 1
    HEAP_INCREASE_KEY(A, heap_size, key)

# Exercise 6.5-8 p. 166
def MAX_HEAP_DELETE(A, i):
    heap_size = len(A) - 1

    if heap_size < 0:
        raise IndexError("Heap underflow")

    # Move deleted key to the root is the key idea!
    while i > 0:
        A[i], A[PARENT(i)] = A[PARENT(i)], A[i]
        i = PARENT(i)

    A[0] = A[heap_size]
    del A[-1]
    BUILD_MAX_HEAP(A)

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':

    L = [4, 1, 3, 2, 16, 9, 10, 14, 8, 7]
    print '\narray:           ', L

    BUILD_MAX_HEAP(L)
    print 'max_heap_array:  ', L
    print 'extracted max:   ', HEAP_EXTRACT_MAX(L)
    print 'without max:     ', L

    # Exercise 6.5-2 p. 165
    L = [15, 13, 9, 5, 12, 8, 7, 4, 0, 6, 2, 1]

    BUILD_MAX_HEAP(L)
    print '\nmax_heap_array:  ', L, 'and insert 10'

    MAX_HEAP_INSERT(L, 10)
    print 'after insert:    ', L, 'and remove 10 (index 2)'
    assert L == [15, 13, 10, 5, 12, 9, 7, 4, 0, 6, 2, 1, 8]

    MAX_HEAP_DELETE(L, 2)
    print 'after delete:    ', L , 'heap is the same after insert/delete'
    assert L == [15, 13, 9, 5, 12, 8, 7, 4, 0, 6, 2, 1]
