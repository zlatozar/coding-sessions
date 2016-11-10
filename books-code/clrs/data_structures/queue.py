# -*- coding: utf-8 -*-

# Chapter 10 p.233

# ___________________________________________________________
#                                                      NOTES

# There is no guarantee that 'head' is ahead because of "wrap around".

# Code highlights:

# Fist of all it is very important 'head'=None at the beginning. Obviously that is the
# only way to say that there is no elements.

# Second very important thing is that we have to manage the size of the queue using the
# range('head', 'tail'). When 'head' and 'tail' are next to each other it means that the
# queue is full.  We control when using self.tail == self.head.

# If this equality happens in the context of DEQUEUE this means that we can't do further
# get so make 'head'=None.

# In the context of ENQUEUE it means that 'tail' reached size of the queue and
# overlapped. You can use this check or use self.tail == 0 and self.head == 0.

# ___________________________________________________________
#                                             IMPLEMENTATION

# Exercise 10.1-4
class Queue:

    def __init__(self, n):
        self._size = n
        self._store = [None] * n

        self.head = None
        self.tail = 0

    def DEQUEUE(self):

        if self.head == None:
            raise IndexError('Queue underflow')

        x = self._store[self.head]

        if self.head == self._size - 1:
			self.head = 0
        else:
			self.head = self.head + 1

        if self.head == self.tail:
            self.head = None

        return x

    def ENQUEUE(self, elm):

        if self.tail == self.head:
            raise IndexError('Queue overflow')

        self._store[self.tail] = elm

        if self.head == None:
            self.head = self.tail

        if self.tail == self._size - 1:
            self.tail = 0
        else:
			self.tail = self.tail + 1

# ___________________________________________________________
#                                                    HELPERS

    def __len__(self):

        if self.head == None:
            return 0

        if self.head == self.tail:
            return self._size

        if self.tail > self.head:
            lengh = self.tail - self.head
        else:
            lengh = self.head - self.tail

        return lengh

    def __str__(self):

        items = []

        if self.head == None:
            return '<%s<' % items

        if self.head < self.tail:
            for i in range(self.head, self.tail):
                items.append(self._store[i])
            return '<%s<' % items

        # in this case there is overlap
        elif self.head > self.tail:
            for i in range(self.head, self._size):
                items.append(self._store[i])
            for j in range(self._size, self.tail):
                items.append(self._store[j])

            return '<%s<' % items

        else:
            return '<%s<' % self._store
