#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Chapter 9: Selection in expected time p.216

# ___________________________________________________________
#                                                      NOTES

# Efficient way of computing the i-th minimum element of an array (without sorting it).

# The idea for the randomized algorithm is to notice that, after the partitioning step we
# can tell which subarray has the item we are looking for, just by looking at their sizes.
# So, we only need to recursively examine one subarray, not two.

# For instance, if we are looking for the 87th smallest element in our array, and after
# partitioning the "LESS" subarray (of elements less than the pivot) has size 200, then we
# just need to find the 87th smallest element in LESS.
#
# On the other hand, if the "LESS" subarray has size 40, then we just need to find
# the 87 − 40 − 1 = 46th smallest element in GREATER.
#
# And if the "LESS" subarray has size exactly 86 then we just return the pivot.

# ___________________________________________________________
#                                                    HELPERS

def PARTITION(A, low, high):

    pivot = A[high]
    i = low - 1

    for j in range(low, high):
        if A[j] <= pivot:
            i = i + 1
            A[i], A[j] = A[j], A[i]

    A[i + 1], A[high] = A[high], A[i + 1]
    return i + 1

# The idea is not to select always last one but randomly. We know how to partition using
# last one (PARTITION function) as pivot so choose one randomly swap it with the last one
# and apply again PARTITION

import random

def RANDOMIZED_PARTITION(A, low, high):
    i = random.randint(low, high)
    A[high], A[i] = A[i], A[high]

    return PARTITION(A, low, high)

# ___________________________________________________________
#                                             IMPLEMENTATION

def RANDOMIZED_SELECT(A, low, high, i):

    if low == high:
        return A[low]

    pivot = RANDOMIZED_PARTITION(A, low, high)
    less_size = pivot - low + 1

    if i == less_size:
        return A[pivot]

    else:
        if i < less_size:
            return RANDOMIZED_SELECT(A, low, pivot - 1, i)
        else:
            return RANDOMIZED_SELECT(A, pivot + 1, high, i - less_size)  # !!!

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':

    L = [1, 3, 5, 2, 4, 6, 8, 7, 11, 10, 9]

    print 'Find %sth smallest element in %s' % (3, L)
    print RANDOMIZED_SELECT(L, 0, len(L) - 1, 3)
