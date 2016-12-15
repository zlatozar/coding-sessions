# -*- coding: utf-8 -*-

# Chapter 10

# ___________________________________________________________
#                                             IMPLEMENTATION

class Node:

    def __init__(self, key):
        self.key = key
        self.next = None

    def __str__(self):
        return "Node(%s)" % self.key


class SingleLinkedList:

    def __init__(self):
        self.head = None

    def SEARCH(self, k):
        x = self.head
        while x != None and x.key != k:
            x = x.next
        return x

    def INSERT(self, x):
        x.next = self.head
        self.head = x

    def DELETE(self, x):
        if x == self.head:
            self.head = self.head.next

        else:
            y = self.head
            while y != None and y.next != x:
                y = y.next

            if y is None:
                return None

            y.next = x.next

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
            s += str(x.key)
            if x.next != None: # if not the last one
                s += ", "
            x = x.next
        return s + "]"

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':
    l = SingleLinkedList()

    l.INSERT(Node(1))
    l.INSERT(Node(2))

    node3 = Node(3)
    l.INSERT(node3)

    l.INSERT(Node(4))

    print l

    print "Search %s, found: %s" % (3, l.SEARCH(3))

    print "\nWill delete node3"
    l.DELETE(node3)

    print l
