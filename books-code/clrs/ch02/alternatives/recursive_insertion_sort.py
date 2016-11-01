#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# ___________________________________________________________
#                                             IMPLEMENTATION

def INSERT(x, L):
    if [] == L:      return [x]
    elif x <= L[0]:  return [x] + L
    else:            return [L[0]] + INSERT(x, L[1:])

def INSERTION_SORT(L):
    if [] == L:  return []
    else:        return INSERT(L[0], INSERTION_SORT(L[1:]))

# ___________________________________________________________
#                                                       TEST

import random

L = [random.randint(1, 50) for _ in range(10)]

print L
print INSERTION_SORT(L)
