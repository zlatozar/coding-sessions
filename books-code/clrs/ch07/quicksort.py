#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Quicksort p.170

# ___________________________________________________________
#                                                      NOTES

# The key to the algorithm is the PARTITION procedure, which rearranges the subarray
# A[p..r] in place. The running time of QUICKSORT is dominated by the time spent in the
# PARTITION procedure.

# Code highlights:

# Why to choose tail recursive algorithms?

# ___________________________________________________________
#                                             IMPLEMENTATION

from partition import PARTITION

def QUICKSORT(A):

    def SORT(A, p, r):
        if p < r:
            q = PARTITION(A, p, r)
            SORT(A, p, q - 1)
            SORT(A, q + 1, r)

    SORT(A, 0, len(A) - 1)

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':
    import random

    L = [random.randint(1, 50) for _ in range(10)]

    print L
    QUICKSORT(L)
    print L
