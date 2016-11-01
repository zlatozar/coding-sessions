#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Heapsort p.151

# ___________________________________________________________
#                                                      NOTES

# In heapsort implementation we use two very important facts based on heap property

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

# A[i] "float down" in the max-heap so that
# the sub-tree rooted at index i obeys the max-heap property.
def MAX_HEAPIFY(A, i, size):
    left = LEFT(i)
    right = RIGHT(i)

    largest = i
    if left <= size and A[left] > A[largest]:
        largest = left

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

# Since the maximum element of the array is stored at the root A[0] we can put it into its
# correct final position by exchanging it with A[n]. In next step we take shorter segment
# as reduce previous one from both sides and apply again the rule - A[0] at the end and reorder.

def HEAPSORT(A):
    BUILD_MAX_HEAP(A)
    size = len(A) - 1
    for i in range(size, 0, -1):
        A[0], A[i] = A[i], A[0]
        size = size - 1
        MAX_HEAPIFY(A, 0, size)
    return A

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':
    import random

    L = [random.randint(1, 50) for _ in range(10)]

    print L
    print HEAPSORT(L)
