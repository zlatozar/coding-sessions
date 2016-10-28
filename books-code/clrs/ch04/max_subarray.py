#!/usr/bin/python

# Chapter 4: Divide and Conquer p.71

import sys

def FIND_MAX_CROSSING_SUBARRAY(A, low, mid, high):
    print "Indices: [low:%s, mid:%s, high:%s]" % (low, mid, high)

    left_sum = -sys.maxint
    max_l_idx = 0

    # left
    sum = 0
    for i in range(mid, low - 1, -1):
        sum = sum + A[i]
        if sum > left_sum:
            left_sum = sum
            max_l_idx = i

    # right
    right_sum = -sys.maxint
    max_r_idx = 0

    sum = 0
    for i in range(mid + 1, high + 1):
        sum = sum + A[i]
        if sum > right_sum:
            right_sum = sum
            max_r_idx = i

    return [max_l_idx, max_r_idx, right_sum + left_sum]

def FIND_MAXIMUM_SUBARRAY(A, low, high):

    if high == low:
        return [low, high, A[low]]

    else:
        mid = (low + high) / 2

        [left_low, left_high, left_sum]   = FIND_MAXIMUM_SUBARRAY(A, low, mid)
        [right_low, right_high, righ_sum] = FIND_MAXIMUM_SUBARRAY(A, mid + 1, high)

        [cross_low, cross_high, cross_sum] = FIND_MAX_CROSSING_SUBARRAY(A, low, mid, high)

        print 'l_sum: %s, r_sum: %s, cross_sum: %s\n' % (left_sum, righ_sum, cross_sum)

        if left_sum >= righ_sum and left_sum >= cross_sum:
            return [left_low, left_high, left_sum]

        elif righ_sum >= left_sum and righ_sum >= cross_sum:
            return [right_low, right_high, righ_sum]

        else:
            return [cross_low, cross_high, cross_sum]

def max_subarray(A):
    result = FIND_MAXIMUM_SUBARRAY(A, 0, len(A) - 1)
    print 'A=%s' % A
    print 'Indices of max sub-array: (low: %s, high: %s)' % (result[0], result[1])
    return result[2]

# test
#                                  |max subarray=43|
A = [13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7]
print 'Sum of max sub-array is: %s\n' % max_subarray(A)
