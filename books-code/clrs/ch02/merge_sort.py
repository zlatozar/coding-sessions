#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Merge Sort p.31-34

# ___________________________________________________________
#                                                      NOTES

# Interesting part here is the merging. Here is how we proceed.
#
# Divide the array in two piles. Place at the end of each pile a sentinel, which contains
# a special value. In this way we simplify our code! Assume that we work with numbers so
# use 'sys.maxint' as the sentinel value. When we reach sentinel the rest of other pile
# could be added to A. This merge is possible(easy) because L and R contains already
# sorted elements.

# ___________________________________________________________
#                                             IMPLEMENTATION

import sys

# Linear merge
def MERGE(A, start, mid, end):

    n1 = mid - start + 1
    n2 = end - mid
    L = []
    R = []

    # divide in two pails
    for i in range(0, n1):
        L.append(A[start + i])

    for j in range(0, n2):
        R.append(A[mid + j + 1])

    # add sentinel
    L.append(sys.maxint)
    R.append(sys.maxint)

    i = 0
    j = 0
    for k in range(start, end + 1):
        if L[i] <= R[j]:
            A[k] = L[i]
            i = i + 1
        else:
            A[k] = R[j]
            j = j + 1

def MERGE_SORT(A):

    def SORT(A, start, end):
        # guards for empty A
        if start < end:
            mid = (start + end) // 2
            SORT(A, start, mid)
            SORT(A, mid + 1, end)
            MERGE(A, start, mid, end)

    SORT(A, 0, len(A) - 1)
    return A

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':
    import random

    L = [random.randint(1, 50) for _ in range(10)]

    print L
    print MERGE_SORT(L)
