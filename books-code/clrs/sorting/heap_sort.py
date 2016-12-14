#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Chapter 6: Heapsort p.159

# ___________________________________________________________
#                                                      NOTES

# Since the maximum element of the array is stored at the root A[0] we can put it into its
# correct final position by exchanging it with A[n]. In next step we take shorter segment
# as reduce previous one from both sides and apply again the rule - A[0] at the end and reorder.

# ___________________________________________________________
#                                             IMPLEMENTATION

from max_heap import BUILD_MAX_HEAP, MAX_HEAPIFY

def HEAPSORT(A):
    BUILD_MAX_HEAP(A)
    size = len(A) - 1

    for i in range(size, 0, -1):
        A[0], A[i] = A[i], A[0]
        size = size - 1
        MAX_HEAPIFY(A, 0, size)

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':
    import random

    L = [random.randint(1, 50) for _ in range(10)]

    print L
    HEAPSORT(L)
    print L
