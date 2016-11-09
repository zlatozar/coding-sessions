# -*- coding: utf-8 -*-

# Chapter 10 p.233

# ___________________________________________________________
#                                             IMPLEMENTATION

class Stack(object):

    def __init__(self):
        self._items = list()

    def __len__(self):
        return len(self._items)

    def __iter__(self):
        if self._items:
            ptr = len(self._items) - 1
            while ptr >= 0:
                yield self._items[ptr]
                ptr = ptr - 1

    def isEmpty(self):
        return len(self) == 0

    def size(self):
        return len(self._items)

    def peek(self):
        assert not self.isEmpty(), "Cannot peek at an empty stack"
        return self._items[-1]

    def pop(self):
        assert not self.isEmpty(), "Cannot pop from an empty stack"
        return self._items.pop()

    def push(self, item):
        self._items.append(item)
