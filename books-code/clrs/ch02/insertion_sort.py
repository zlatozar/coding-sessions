#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Chapter 2: Getting started p.18

# ___________________________________________________________
#                                                      NOTES

# We start with the second one and traverse previous(sorted one) one for suitable place
# to insert it. If we find such place we have to shift all. Do we have place to shift?
# We could have if we imagine that current element (for which we search proper place) is
# removed from the array and there is gap. Now int is not a problem to shift.

# The numbers that we wish to sort are also known as the keys.

# ___________________________________________________________
#                                             IMPLEMENTATION

def INSERTION_SORT(A):

    for i in range(1, len(A)):
        key = A[i]             # 'remove' current element
        j = i - 1              # start traverse previous starting from biggest one

        while (j >= 0) and (A[j] > key):
            A[j + 1] = A[j]
            j = j - 1
        A[j + 1] = key

    return A

# ___________________________________________________________
#                                                       TEST

import random

L = [random.randint(1, 50) for _ in range(10)]

print L
print INSERTION_SORT(L)
