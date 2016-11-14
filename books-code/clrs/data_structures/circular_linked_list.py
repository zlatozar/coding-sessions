# -*- coding: utf-8 -*-

# Chapter 10 p.238

# ___________________________________________________________
#                                                      NOTES

# A circular, doubly linked list with a sentinel has the property that every node references
# a next node and a previous node. Always. This uniform way of treating nodes turns out to
# be quite convenient, because as we write methods to insert or delete nodes into the list,
# we don’t have to worry about special cases that would arise if the last node didn’t have a
# next node, and the first node didn’t have a previous node, but all interior nodes had
# both.

# ___________________________________________________________
#                                             IMPLEMENTATION

class Node:

    def __init__(self, data):
        self.prev = None
        self.data = data
        self.next = None

    def __str__(self):
        return "Node(%s)" % self.data

class CircularLinkedList:

    def __init__(self):
        # sentinel node
        self.nil = Node(None)

        # references to itself
        self.nil.prev = self.nil
        self.nil.next = self.nil

    def INSERT(self, data):
        x = Node(data)

        x.next = self.nil.next
        self.nil.next.prev = x
        self.nil.next = x
        x.prev = self.nil

    def SEARCH(self, k):
        x = self.nil.next
        while x != self.nil and x.data != k:
            x = x.next

        return x

    def DELETE(self, x):
        x.prev.next = x.next
        x.next.prev = x.prev

# ___________________________________________________________
#                             HELPERS(just for illustration)

    def __str__(self):
        s = "["

        x = self.nil.next
        while x != self.nil:
            s += str(x.data)
            if x.next != self.nil:
                s += ", "
            x = x.next

        return s + "]"
