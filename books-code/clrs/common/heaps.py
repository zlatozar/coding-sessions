#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

def LEFT(i):
    return 2*i + 1

def RIGHT(i):
    return 2*i + 2

def check_max_heap(heap, i):
    if i >= len(heap):
        return True

    if LEFT(i) < len(heap) and heap[i] < heap[LEFT(i)]:
        print 'Parent %s has invalid LEFT child %s' % (heap[i], heap[LEFT(i)])
        return False

    if RIGHT(i) < len(heap) and heap[i] < heap[RIGHT(i)]:
        print 'Parent %s has invalid RIGHT child %s' % (heap[i], heap[RIGHT(i)])
        return False

    return check_max_heap(heap, LEFT(i)) and check_max_heap(heap, RIGHT(i))
