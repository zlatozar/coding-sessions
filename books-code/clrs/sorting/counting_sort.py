#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Chapter 8: Counting sort p.194

# ___________________________________________________________
#                                                      NOTES

# We require two other arrays: the array B[1..n] holds the sorted output, and the array
# C[0..k] provides temporary working storage.

# It is counting sorting because we count all elements: C[2] = 3, it means there are 3
# items that are equal to 2. len(C) = key + 1 because there is no bigger than 'key'. Last
# element of C counts max(A).
#
# In next step we modify the C (a.k.a count array) by adding the previous counts (first
# element is not included because it has no previous). C[i] now contains the number of
# elements less than or equal to i.
#
# Third step is the most difficult one.
#
# We take the A[j] value and have to decide where to place it in the resulting array B.
# Fortunately C[A[j]] tell us how many elements are before(or equal) to A[j] so if we
# place it in this position C[A[j]] - 1 (we count from 0) there will be enough space for
# others. Of course decrease the counter because this one will be placed and if there is
# duplicates will be placed before this one

# Code highlights:

# C = [0] * (k + 1) is a way to represent array C[0..k] filled with zeros.

# ___________________________________________________________
#                                             IMPLEMENTATION


def COUNTING_SORT(A):

    key = max(A)                 # 0(n)
    B = [0] * len(A)
    C = [0] * (key + 1)          # store the count of each unique element

    for j in range(0, len(A)):
        C[A[j]] = C[A[j]] + 1
    # C[i] now contains the number of elements equal to i.

    for i in range(1, key + 1):
        C[i] = C[i - 1] + C[i]
    # C[i] now contains the number of elements less than or equal to i.

    for j in range(len(A) - 1, -1, -1):
        B[C[A[j]] - 1] = A[j]
        C[A[j]] = C[A[j]] - 1

    return B

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':
    import random

    L = [random.randint(1, 50) for _ in range(10)]

    print L
    print COUNTING_SORT(L)
