#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Radix sort p.197

# ___________________________________________________________
#                                                      NOTES

# In radix sort numbers must have equal numbers. The interesting part is how to unify them
# in the stable sort algorithm.

# ___________________________________________________________
#                                             IMPLEMENTATION

# the digit represented by exp.
def COUNTING_SORT(A, d):

    B = [0] * len(A)
    C = [0] * (10)

    for i in range(0, len(A)):
        idx = (A[i] / d) % 10
        C[idx] = C[idx] + 1

    for i in range(1, 10):
        C[i] = C[i - 1] + C[i]

    for j in range(len(A) - 1, -1, -1):
        idx = (A[j] / d) % 10
        B[C[idx] - 1] = A[j]
        C[idx] = C[idx] - 1

    i = 0
    for i in range(0,len(A)):
        A[i] = B[i]

def RADIX_SORT(A):

    key = max(A)

    # Do counting sort for every digit. Note that instead
    # of passing digit number, exp is passed. exp is 10^i
    # where i is current digit number
    exp = 1
    while key/exp > 0:
        COUNTING_SORT(A, exp)
        exp = exp * 10

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':
    import random

    L = [random.randint(1, 50) for _ in range(10)]

    print L
    RADIX_SORT(L)
    print L
