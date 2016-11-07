#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Radix sort p.197

# ___________________________________________________________
#                                                      NOTES

# It is radix sort because this algorithm compares numbers for a given radix starting from
# the least significant digit. The interesting part is how to arrange this. We need two formulas.

# How to iterate through every numbers digit? Of course biggest is the leading.
#
# key = max(A)
# while key / radix > 0:
#     COUNTING_SORT(A, radix)
#     radix = radix * 10
#

# How to select digit using given radix?
# idx = (A[j] / radix) % 10 e.g 123 / 10 = 12, 12 % 10 = 2

# ___________________________________________________________
#                                             IMPLEMENTATION

def COUNTING_SORT(A, radix):

    B = [0] * len(A)
    C = [0] * (10)    # !!!

    for j in range(0, len(A)):
        idx = (A[j] / radix) % 10
        C[idx] = C[idx] + 1

    for i in range(1, 10):
        C[i] = C[i - 1] + C[i]

    for j in range(len(A) - 1, -1, -1):
        idx = (A[j] / radix) % 10
        B[C[idx] - 1] = A[j]
        C[idx] = C[idx] - 1

    # additional step
    for i in range(0, len(A)):
        A[i] = B[i]

def RADIX_SORT(A):

    radix = 1
    key = max(A)

    # Do counting sort for every digit. Note that instead
    # of passing digit number, 'radix' is passed. 'radix' is [1, 10, 100...10*n]

    while key / radix > 0:
        COUNTING_SORT(A, radix)
        radix = radix * 10

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':
    import random

    L = [random.randint(1, 50) for _ in range(10)]

    print L
    RADIX_SORT(L)
    print L
