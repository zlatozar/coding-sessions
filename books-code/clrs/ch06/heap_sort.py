#!/usr/bin/python

# Chapter 6: Heapsort

class heap(list):
    def __init__(self, *args):
        list.__init__(self, *args)
        self.heap_size = len(*args)
        self.length = 0

def PARENT(i):
    return i // 2

def LEFT(i):
    return 2*i + 1


def RIGHT(i):
    return 2*i + 2

def MAX_HEAPIFY(A, i, size):
    left = LEFT(i)
    right = RIGHT(i)

    largest = i
    if left <= size and A[left] > A[largest]:
        largest = left
    # else:
    #     largest = i

    if right <= size and A[right] > A[largest]:
        largest = right

    if largest != i:
        # exchange
        A[i], A[largest] = A[largest], A[i]
        MAX_HEAPIFY(A, largest, size)

def BUILD_MAX_HEAP(A):
    n = len(A)
    for i in range(n // 2, -1, -1):
        MAX_HEAPIFY(A, i, len(A) - 1)


def HEAPSORT(A):
    BUILD_MAX_HEAP(A)
    size = len(A) - 1
    for i in range(len(A) - 1, 0, -1):
        A[0], A[i] = A[i], A[0]
        size -= 1
        MAX_HEAPIFY(A, 0, size)

A = [4, 1, 3, 2, 16, 9, 10, 14, 8, 7]
print A
HEAPSORT(A)
print A
assert A == [1, 2, 3, 4, 7, 8, 9, 10, 14, 16]
