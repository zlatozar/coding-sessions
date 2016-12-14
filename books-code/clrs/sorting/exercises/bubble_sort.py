#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Exercise 2-2 p. 40

# ___________________________________________________________
#                                                      NOTES

# Bubble sort, is a simple stable sorting algorithm that repeatedly steps through the list
# to be sorted, compares each pair of adjacent items and swaps them if they are in the
# wrong order. The pass through the list is repeated until no swaps are needed, which
# indicates that the list is sorted.

# In almost-sorted arrays and fix it with just linear complexity O(2n)!

# ___________________________________________________________
#                                             IMPLEMENTATION

def BUBBLESORT(A):
    for i in range(0, len(A)):
        for j in range(len(A) - 1, i, -1):
            if A[j] < A[j - 1]:
                A[j], A[j - 1] = A[j - 1], A[j]
    return A

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':
    import random

    L = [random.randint(1, 50) for _ in range(10)]

    print L
    print BUBBLESORT(L)
