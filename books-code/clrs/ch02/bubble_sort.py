#!/usr/bin/env python
# -*- coding: utf-8 -*-

def BUBBLESORT(A):
    for i in range(0, len(A)):
        for j in range(len(A) - 1, i, -1):
            if A[j] < A[j - 1]:
                A[j], A[j - 1] = A[j - 1], A[j]
    return A

# ___________________________________________________________
#                                                       TEST

import random

L = [random.randint(1, 50) for _ in range(10)]

print L
print BUBBLESORT(L)
