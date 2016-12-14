#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Exercise 10.2-3 p.240

# ___________________________________________________________
#                                             IMPLEMENTATION

class Node:
    def __init__(self, key):
        self.key = key
        self.next = None

    def __str__(self):
        return "Node(%s)" % self.key

class Queue:

    def __init__(self):
        self.head = None
        self.tail = Node(None)

    def ENQUEUE(self, x):
        if self.tail.next == None:
            self.head = x
            self.tail.next = self.head
        else:
            self.head.next = x
            self.head = x

    def DEQUEUE(self):
        if self.tail.next == None:
            return None

        x = self.tail.next.key
        self.tail = self.tail.next

        return x

    # ___________________________________________________________
    #                             HELPERS(just for illustration)

    def __str__(self) :
        s = "[None<-"

        x = self.tail.next
        while x != None:
            s += str(x.key)
            if x.next != None: # if not the last one
                s += ", "
            x = x.next
        return s + "]"

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':

    l = Queue()

    l.ENQUEUE(Node(1))
    l.ENQUEUE(Node(2))
    l.ENQUEUE(Node(3))

    print l

    # delete by node reference
    l.DEQUEUE()
    l.DEQUEUE()

    print l
