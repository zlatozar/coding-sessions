#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Exercise 2.2-2 p. 29

# Consider sorting N numbers in an array A by first finding the smallest element of A and
# exchanging it with the element in A[1]. Then find the second smallest element of A, and
# exchange it with A[2]. Continue in this manner for the first N - 1 elements of A.

# ___________________________________________________________
#                                             IMPLEMENTATION

def SELECTION_SORT(A):

    n = len(A)
    for j in range(0, n):
        smallest = j
        for i in range(j + 1, n):
            if A[i] < A[smallest]:
                smallest = i
        A[j], A[smallest] = A[smallest], A[j]

    return A

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':
    import random

    L = [random.randint(1, 50) for _ in range(10)]

    print L
    print SELECTION_SORT(L)
