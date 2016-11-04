#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

def exchange(A, i, j):
    tmp = A[i]
    A[i] = A[j]
    A[j] = tmp

def delete_last(A):
    del A[-1]

def is_sorted_array(A):

    for index in range(len(A)):
        next = index + 1

        if next == len(A):
            return True

        elif A[index] > A[next]:
            return False

def cross_product(A1, A2):
    answer = []
    for a in A1:
        for b in A2:
            answer.append((a, b))
    return answer
