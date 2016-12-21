#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Exercise 12.3-3 p.299

# ___________________________________________________________
#                                                    HELPERS

tree = None

class Node:

    def __init__(self, k):
        self.key = k
        self.parent = None
        self.left = None
        self.right = None


# Exercise 12.3-1
def INSERT(z):

    global tree

    def insert(new, x):
        if x is None:
            return Node(new)

        if x.key > z:
            x.left = insert(new, x.left)
            x.left.parent = x

        else:
            x.right = insert(new, x.right)
            x.right.parent = x

        return x

    if tree == None:
        tree = Node(z)

        return tree

    else:
        return insert(z, tree)

def INORDER_WALK(x):

    L = []
    def inorder_walk(x):
        if x != None:
            inorder_walk(x.left)
            L.append(x.key)
            inorder_walk(x.right)

    inorder_walk(x)

    return L
# ___________________________________________________________
#                                             IMPLEMENTATION

def TREE_SORT(L):

    for i in L:
        INSERT(i)

    return INORDER_WALK(tree)

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':
    import random

    L = [random.randint(1, 50) for _ in range(10)]

    print L
    print TREE_SORT(L)
