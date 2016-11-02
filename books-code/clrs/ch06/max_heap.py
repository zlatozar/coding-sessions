#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Heaps p.151

# ___________________________________________________________
#                                                      NOTES

# Important facts based on heap property

# 1. First element of the heap is the biggest one
# 2. First half of the heap are roots, second one contains only leaves

# ___________________________________________________________
#                                             IMPLEMENTATION

def PARENT(i):
    return (i - 1) // 2

def LEFT(i):
    return 2*i + 1

def RIGHT(i):
    return 2*i + 2

# A[i] "float down" in the max-heap so that the sub-tree rooted
# at index i obeys the max-heap property.

def MAX_HEAPIFY(A, i, size):
    left = LEFT(i)
    right = RIGHT(i)

    if left <= size and A[left] > A[i]:
        largest = left
    else:
        largest = i

    if right <= size and A[right] > A[largest]:
        largest = right

    if largest != i:
        # exchange
        A[i], A[largest] = A[largest], A[i]
        MAX_HEAPIFY(A, largest, size)

def BUILD_MAX_HEAP(A):
    heap_size = len(A)
    for i in range(heap_size // 2, -1, -1):
        MAX_HEAPIFY(A, i, heap_size - 1)

# ___________________________________________________________
#                                                       TEST
