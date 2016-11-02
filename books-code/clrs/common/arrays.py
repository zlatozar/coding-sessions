#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

def exchange(A, i, j):
    tmp = A[i]
    A[i] = A[j]
    A[j] = tmp

def delete_last(A):
    del A[-1]

def array_is_sorted(A):

    for index in range(len(A)):
        next = index + 1

        if next == len(A):
            return True

        elif A[index] > A[next]:
            return False
