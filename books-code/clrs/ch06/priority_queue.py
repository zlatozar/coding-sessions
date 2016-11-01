#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Priority queues p.162

# ___________________________________________________________
#                                                      NOTES


# ___________________________________________________________
#                                             IMPLEMENTATION

import sys
from heap_sort import PARENT, MAX_HEAPIFY, BUILD_MAX_HEAP

def HEAP_MAXIMUM(A):
    return A[0]

def HEAP_EXTRACT_MAX(A):
    heap_size = len(A) - 1

    # guards for empty A
    if heap_size < 0:
        raise IndexError("Heap underflow")

    max = A[0]
    A[0] = A[heap_size]         # ???
    heap_size = heap_size - 1
    MAX_HEAPIFY(A, 0, heap_size)

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

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':

    L = [4, 1, 3, 2, 16, 9, 10, 14, 8, 7]
    print '\narray:       ', L

    BUILD_MAX_HEAP(L)
    print 'heap_array:  ', L
    print 'max:         ', HEAP_EXTRACT_MAX(L)

    # Exercise 6.5-2 p. 165
    L = [15, 13, 9, 5, 12, 8, 7, 4, 0, 6, 2, 1]

    BUILD_MAX_HEAP(L)
    print '\nheap_array:       ', L, ' and insert 10'

    MAX_HEAP_INSERT(L, 10)
    print 'after insert:', L
    assert L==[15, 13, 10, 5, 12, 9, 7, 4, 0, 6, 2, 1, 8]
