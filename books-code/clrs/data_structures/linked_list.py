# -*- coding: utf-8 -*-

# Chapter 10 p.236

# ___________________________________________________________
#                                                      NOTES

# The clue is that data should be wrapped with meta-information about connection to the
# next element. In our implementation is the Node object. Deletion in double linked list
# is faster than in single linked. With a doubly linked list you have ready access to both
# elements because you have links to both of them (see DELETE). This assumes that you
# already have a pointer to the element you need to delete and there is no searching involved.

# Code highlights:

# L.next and L.prev points to the object Node not to its particular field. In this way
# we can use 'head' as a temp variable in INSERT. Treat 'head' as the last element.

# ___________________________________________________________
#                                             IMPLEMENTATION

class Node:

    def __init__(self, data):
        self.prev = None
        self.data = data
        self.next = None

    def __str__(self):
        return "Node(%s)" % self.data

class LinkedList:

    def __init__(self):
        self.head = None

    def INSERT(self, data):
        x = Node(data)

        x.next = self.head
        if self.head != None:
            self.head.prev = x

        # now head is the last element
        self.head = x
        x.prev = None

    def SEARCH(self, k):
        x = self.head

        while x != None and x.data != k:
            x = x.next

        # None if nothing found
        return x

    # bypass x.next and x.prev connections
    def DELETE(self, x):
        if x.prev != None:
            x.prev.next = x.next
        else:
            self.head = x.next

        if x.next != None:
            x.next.prev = x.prev

# ___________________________________________________________
#                             HELPERS(just for illustration)

    def __len__(self):
        count = 0

        x = self.head
        while x != None:
            count = count + 1
            x = x.next
        return count

    def __str__(self) :
        s = "[None<-"

        x = self.head
        while x != None:
            s += str(x.data)
            if x.next != None: # if not the last one
                s += ", "
            x = x.next
        return s + "]"
