#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Programming pearls. Chapter 8: Algorithm design techniques p.80

# ___________________________________________________________
#                                             IMPLEMENTATION

def max_subarray(A):

    # 0 <= low <= high < len(A)
    def find_max_subarray(low, high):

        if low > high:
            return 0

        if low == high:
            return max(0, A[low])

        mid = (low + high) / 2

        # Find max crossing to left
        lmax = 0
        sum  = 0

        for i in range(mid, 0, -1):
            sum += A[i]
            lmax = max(lmax, sum)

        # Find max crossing to right
        rmax = 0
        sum = 0
        for i in range(mid + 1, high):
            sum += A[i]
            rmax = max(rmax, sum);

        return max(lmax + rmax,                            # crossing
                   max(find_max_subarray(low, mid),        # left
                       find_max_subarray(mid + 1, high)))  # right

    return find_max_subarray(0, len(A) - 1)

# ___________________________________________________________
#                                                       TEST

#                                  |max subarray=43|
A = [13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7]
print 'A=%s' % A
print 'Sum of max sub-array is: %s\n' % max_subarray(A)
