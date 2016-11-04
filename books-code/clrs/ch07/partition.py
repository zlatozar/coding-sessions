#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# ___________________________________________________________
#                                                      NOTES

# Here is the explanation of the partitioning:
#
# We need a border to divide the array in two groups SMALL and BIG and that is the role of 'i'.
#
# If it current element is smaller we increment 'i' (make room for the new element)
# and swap - add in SMALL group.  If it is bigger we do nothing. Finally we place the
# pivot as the first element in BIG group. Now you understand why we choose last element A[r]
# as pivot.

# Code highlights:

# Note that initial value of 'i' is 'low - 1';

# ___________________________________________________________
#                                             IMPLEMENTATION

def PARTITION(A, low, high):

    pivot = A[high]
    i = low - 1

    for j in range(low, high):
        if A[j] <= pivot:
            i = i + 1
            A[i], A[j] = A[j], A[i]

    # swap pivot so it lies between the two partitions
    A[i + 1], A[high] = A[high], A[i + 1]
    return i + 1

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':
    L = [13, 19, 9, 5, 12, 8, 7, 4, 21, 2, 6, 11]

    print 'Before partition: %s'% L
    q = PARTITION(L, 0, 11)
    print 'q = %s, A[q] = %s' % (q, L[q])
    print 'After partition:  %s' % L
