#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# ___________________________________________________________
#                                                      NOTES

# Define recursively how to insert smaller element in a sorted array.
# If you now how to insert smaller element could you define sorting algorithm?

# Code highlights:

# Note how code is formatted.

# ___________________________________________________________
#                                             IMPLEMENTATION

def INSERT_SMALLER(x, L):
    if [] == L:      return [x]
    elif x <= L[0]:  return [x] + L
    else:            return [L[0]] + INSERT_SMALLER(x, L[1:])

def INSERTION_SORT(L):
    if [] == L:  return []
    else:        return INSERT_SMALLER(L[0], INSERTION_SORT(L[1:]))

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':
    import random

    L = [random.randint(1, 50) for _ in range(10)]

    print L
    print INSERTION_SORT(L)
