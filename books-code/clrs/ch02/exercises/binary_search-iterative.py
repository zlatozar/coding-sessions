#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Exercise 2.5-3 p. 39

# ___________________________________________________________
#                                             IMPLEMENTATION

def ITERATIVE_BINARY_SEARCH(A, v):

    def BINARY_SEARCH(A, v, low, high):

        while low <= high:
            mid = (low + high) // 2
            if v == A[mid]:
                return mid

            if A[mid] < v:
                low = mid + 1
            else:
                high = mid - 1
        return None

    return BINARY_SEARCH(A, v, 0, len(A) - 1)

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':

    # Works only if array is sorted
    L = [-25, -23, -22, -16, -7, -5, -4, -3, -3, 7, 12, 13, 15, 18, 20, 20]
    v = 15

    print "In array ", L, "find index of number:", 15
    print "Index of searched number is: %s" % ITERATIVE_BINARY_SEARCH(L, v)
