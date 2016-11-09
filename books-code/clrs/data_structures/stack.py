# -*- coding: utf-8 -*-

# Chapter 10 p.233

# ___________________________________________________________
#                                                      NOTES

# Don't worry about stack overflow (for now)

# ___________________________________________________________
#                                             IMPLEMENTATION

class Stack:

    def STACK_EMPTY(self):
        return len(self._items) == 0

    def POP(self):
        if self.STACK_EMPTY():
            raise IndexError('Cannot POP from an empty stack')
        return self._items.pop()

    def PUSH(self, item):
        self._items.append(item)

# ___________________________________________________________
#                                                    HELPERS

    def peek(self):
        if self.STACK_EMPTY():
            raise IndexError('Cannot peek from an empty stack')
        return self._items[-1]

    def __init__(self):
        self._items = list()

    def __len__(self):
        return len(self._items)

    def __repr__(self):
        return '%s>' % self._items
