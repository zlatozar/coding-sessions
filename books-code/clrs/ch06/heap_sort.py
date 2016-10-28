#!/usr/bin/python

# Chapter 6: Heapsort

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

    if right <= size and A[right] > A[largest]:
        largest = right

    if largest != i:
        # exchange
        A[i], A[largest] = A[largest], A[i]
        MAX_HEAPIFY(A, largest, size)

def BUILD_MAX_HEAP(A):
    heap_size = len(A)
    for i in range(heap_size // 2, -1, -1):
        MAX_HEAPIFY(A, i, heap_size - 1)


def HEAPSORT(A):
    BUILD_MAX_HEAP(A)
    size = len(A) - 1
    for i in range(size, 0, -1):
        A[0], A[i] = A[i], A[0]
        size = size - 1
        MAX_HEAPIFY(A, 0, size)
    return A

# test
import random

L = [random.randint(1, 50) for _ in range(10)]

print L
print HEAPSORT(L)
