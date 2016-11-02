#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Exercise 6.5-3 p. 165

# ___________________________________________________________
#                                                      NOTES

# Opposite to max heap

# ___________________________________________________________
#                                             IMPLEMENTATION

def LEFT(i):
    return 2*i + 1

def RIGHT(i):
    return 2*i + 2

def HEAP_EXTRACT_MIN(A):
    return A[0]

# Exercise 6.2-2 p.156
def MIN_HEAPIFY(A, i, size):
    left = LEFT(i)
    right = RIGHT(i)

    if left <= size and A[left] < A[i]:
        smallest = left
    else:
        smallest = i

    if right <= size and A[right] < A[smallest]:
        smallest = right

    if smallest != i:
        A[i], A[smallest] = A[smallest], A[i]
        MIN_HEAPIFY(A, smallest, size)

def BUILD_MIN_HEAP(A):
    heap_size = len(A)
    for i in range(heap_size // 2, -1, -1):
        MIN_HEAPIFY(A, i, heap_size - 1)

# ___________________________________________________________
#                                                       TEST
