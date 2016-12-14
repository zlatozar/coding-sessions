#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Exercise 6.5-3 p. 165

# ___________________________________________________________
#                                             IMPLEMENTATION

import sys
from min_heap import PARENT, MIN_HEAPIFY, BUILD_MIN_HEAP

def HEAP_MINIMUM(A):
    return A[0]

def HEAP_EXTRACT_MIN(A):
    heap_size = len(A) - 1

    if heap_size < 1:
        raise IndexError("Heap underflow")

    min = A[0]
    A[0] = A[heap_size]
    del A[-1]
    MIN_HEAPIFY(A, 0, heap_size - 1)

    return min

def HEAP_DECREASE_KEY(A, i, key):
    if key > A[i]:
        raise ValueError("New key is larger than current key")

    A[i] = key
    while i > 0 and A[PARENT(i)] > A[i]:
        A[i], A[PARENT(i)] = A[PARENT(i)], A[i]
        i = PARENT(i)

def MIN_HEAP_INSERT(A, key):
    A.append(sys.maxint)
    heap_size = len(A) - 1
    HEAP_DECREASE_KEY(A, heap_size, key)

def MIN_HEAP_DELETE(A, i):
    heap_size = len(A) - 1

    if heap_size < 0:
        raise IndexError("Heap underflow")

    # Move deleted key to the root is the key idea!
    while i > 0:
        A[i], A[PARENT(i)] = A[PARENT(i)], A[i]
        i = PARENT(i)

    A[0] = A[heap_size]
    del A[-1]
    BUILD_MIN_HEAP(A)

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':

    L = [4, 1, 3, 2, 16, 9, 10, 14, 8, 7]
    print '\narray:           ', L

    BUILD_MIN_HEAP(L)
    print 'min_heap_array:  ', L
    print 'extracted min:   ', HEAP_EXTRACT_MIN(L)
    print 'without min:     ', L

    L = [15, 13, 9, 5, 12, 8, 7, 4, 0, 6, 2, 1]

    BUILD_MIN_HEAP(L)
    print '\nmin_heap_array:  ', L, 'and insert 3'

    MIN_HEAP_INSERT(L, 3)
    print 'after insert:    ', L, 'and remove 3 (index 5)'
    assert L == [0, 2, 1, 4, 6, 3, 7, 13, 5, 15, 12, 9, 8]

    MIN_HEAP_DELETE(L, 5)
    print 'after delete:    ', L , 'heap is the same after insert/delete'
    assert L == [0, 2, 1, 4, 6, 8, 7, 13, 5, 15, 12, 9]
