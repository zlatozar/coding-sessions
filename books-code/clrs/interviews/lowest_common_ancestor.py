#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# ___________________________________________________________
#                                                      NOTES

# Given a binary tree, find the lowest common ancestor of two given nodes in the tree.

# Given a binary tree, find the lowest common ancestor (LCA)
# of two given nodes in the tree.
#
# According to the definition of LCA on Wikipedia: "The lowest
# common ancestor is defined between two nodes v and w as the
# lowest node in T that has both v and w as descendants (where we
# allow a node to be a descendant of itself)."

# Time:  O(n)
# Space: O(h)

# Note that this is not the optimal solution

# ___________________________________________________________
#                                             IMPLEMENTATION

from random import random

class Node:

    def __init__(self, k):
        self.key = k
        self.parent = None
        self.left = None
        self.right = None

    def __str__(self):
        return "Node(%s)" % self.key

class BinaryTree(object):

    def __init__(self):
        self.root = None


    def INSERT(self, t):
        new = Node(t)

        if self.root == None:
            self.root = new
        else:
            node = self.root

            while True:
                # Order doesn't matter - choose randomly left or right
                if random() < 0.5:
                    if node.left == None:
                        node.left = new
                        new.parent = node
                        break

                    node = node.left

                else:
                    if node.right == None:
                        node.right = new
                        new.parent = node
                        break

                    node = node.right
        return new

    # We have to traverse the tree(visit all the notes with DFS) and check if the note is
    # the root of the p and q. If not then return the e.g p (or q) and hope that feature
    # iterations(recursion) will find q (or p)
    def LCA(self, l, r):

        def lca(x, p, q):

            if x == None:
                return None

            if p == x or q == x:
                return x

            left = lca(x.left, p, q)
            right = lca(x.right, p, q)

            if left and right:
                return x

            return lca(x.left, p, q) or lca(x.right, p, q)

        return lca(self.root, l, r)

# ___________________________________________________________
#                                                       TEST

from common import pprint_tree

def build_test_tree():
    import random

    items = (random.randrange(100) for i in xrange(20))

    tree = BinaryTree()

    p = None
    q = None

    for item in items:
        if item % 2 == 0:
            p = tree.INSERT(item)
            continue
        elif item % 3 == 0:
            q = tree.INSERT(item)
            continue
        else:
            tree.INSERT(item)

    return (p, q, tree)

if __name__ == '__main__':

    (p, q, tree) = build_test_tree()

    if p and q:
        print
        print pprint_tree(tree)
        print
        print 'The LCA for %s and %s is %s' % (p, q, tree.LCA(p, q))
    else:
        print 'Try again, please'
