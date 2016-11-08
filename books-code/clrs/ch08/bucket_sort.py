#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Bucket sort p.200

# ___________________________________________________________
#                                                      NOTES

# At first algorithm divides the input array into buckets using hash function. How to find proper
# hash function? If we sort integers it will be easy to use ranges e.g 0..19, 20..39, 40..60 etc.
#
# Each bucket contains some range of input elements (the elements should be uniformly
# distributed to ensure optimal division among buckets). In the second phase the bucket
# sort each bucket using some other sorting algorithm. Finally the algorithm merges all
# the ordered buckets. Because every bucket contains different range of element values,
# bucket sort simply copies the elements of each bucket into the output array (concatenates the buckets).

# ___________________________________________________________
#                                                    HELPERS

def INSERTION_SORT(A):

    for i in range(1, len(A)):
        key = A[i]
        j = i - 1

        while (j >= 0) and (A[j] > key):
            A[j + 1] = A[j]
            j = j - 1
        A[j + 1] = key

    return A

# ___________________________________________________________
#                                             IMPLEMENTATION

def HASH(i):
    return i // 20

def BUCKET_SORT(A):

    n = len(A)

    small = HASH(min(A))
    big = HASH(max(A))

    buckets = []
    for i in range(small, big + 1):
        buckets.append([])

    # Distribute input values into buckets
    for i in range(0, n):
        buckets[HASH(A[i])].append(A[i])

    # Sort every bucket and place it back
    for i in range(small, big + 1):
        buckets[i] = INSERTION_SORT(buckets[i])

    B = []
    print 'Sorted buckets: %s' % buckets
    for i in range(small, big + 1):
        B = B + buckets[i]

    return B

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':
    import random

    L = [random.randint(1, 50) for _ in range(10)]

    print L
    print BUCKET_SORT(L)
