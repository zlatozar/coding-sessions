# -*- coding: utf-8 -*-

# ___________________________________________________________
#                                                     ARRAYS

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

# ___________________________________________________________
#                                                      HEAPS

def check_max_heap(A):
    def check(heap, i):
        if i >= len(heap):
            return True

        if LEFT(i) < len(heap) and heap[i] < heap[LEFT(i)]:
            print 'Parent %s has invalid LEFT child %s' % (heap[i], heap[LEFT(i)])
            return False

        if RIGHT(i) < len(heap) and heap[i] < heap[RIGHT(i)]:
            print 'Parent %s has invalid RIGHT child %s' % (heap[i], heap[RIGHT(i)])
            return False

        return check(heap, LEFT(i)) and check(heap, RIGHT(i))
    check(A, 0)

def check_min_heap(A):
    def check(heap, i):
        if i >= len(heap):
            return True

        if LEFT(i) < len(heap) and heap[i] > heap[LEFT(i)]:
            print 'Parent %s has invalid LEFT child %s' % (heap[i], heap[LEFT(i)])
            return False

        if RIGHT(i) < len(heap) and heap[i] > heap[RIGHT(i)]:
            print 'Parent %s has invalid RIGHT child %s' % (heap[i], heap[RIGHT(i)])
            return False

        return check_min_heap(heap, LEFT(i)) and check_min_heap(heap, RIGHT(i))
    check(A, 0)
