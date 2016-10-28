#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Chapter 2: Getting started p.18

def INSERTION_SORT(A):

    for i in range(1, len(A)):
        val = A[i]
        j = i - 1

        while (j >= 0) and (A[j] > val):
            A[j + 1] = A[j]
            j = j - 1
        A[j + 1] = val

    return A

# ___________________________________________________________
#                                                       TEST

import random

L = [random.randint(1, 50) for _ in range(10)]

print L
print INSERTION_SORT(L)
