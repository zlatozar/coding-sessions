#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# ___________________________________________________________
#                                                      NOTES

# In recursion we have to narrow the array for smaller we shrink upper bound 'mid - 1'
# for bigger lower 'mid + 1' (closer to high)

# ___________________________________________________________
#                                             IMPLEMENTATION

def RECURSIVE_BINARY_SEARCH(A, v):

    def BINARY_SEARCH(A, v, low, high):
        mid = (low + high) // 2

        # when len(A) is empty
        if low > high:
            return None

        if v == A[mid]:
            return mid

        if v < A[mid]:
            return BINARY_SEARCH(A, v, low, mid - 1)
        else:
            return BINARY_SEARCH(A, v, mid + 1, high)

    return BINARY_SEARCH(A, v, 0, len(A) - 1)

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':

    # Works only if array is sorted
    L = [-25, -23, -22, -16, -7, -5, -4, -3, -3, 7, 12, 13, 15, 18, 20, 20, 40]
    v = 15

    print "In array ", L, "find index of number:", 15
    print "Index of searched number is: %s" % RECURSIVE_BINARY_SEARCH(L, v)
