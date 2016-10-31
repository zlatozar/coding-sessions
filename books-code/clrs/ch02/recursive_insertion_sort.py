#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# ___________________________________________________________
#                                             IMPLEMENTATION

def insert(x, L):
    if [] == L:      return [x]
    elif x <= L[0]:  return [x] + L
    else:            return [L[0]] + insert(x, L[1:])

def insertion_sort(L):
    if [] == L:  return []
    else:        return insert(L[0], insertion_sort(L[1:]))

# ___________________________________________________________
#                                                       TEST

import random

L = [random.randint(1, 50) for _ in range(10)]

print L
print insertion_sort(L)
