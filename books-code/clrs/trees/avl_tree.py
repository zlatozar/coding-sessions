#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Chapter 12

# ___________________________________________________________
#                                                      NOTES

# Supports INSERT, FIND, and DELETE_MIN operations in O(lg n) time.

# ___________________________________________________________
#                                             IMPLEMENTATION

def height(node):
    if node is None:
        return -1

    else:
        return node.height

def update_height(node):
    node.height = max(height(node.left), height(node.right)) + 1

# ___________________________________________________________
#                                             IMPLEMENTATION

import binary_search_tree as bst

class AVL(bst.BST):

    def left_rotate(self, x):
        y = x.right
        y.parent = x.parent

        if y.parent is None:
            self.root = y

        else:
            if y.parent.left is x:
                y.parent.left = y

            elif y.parent.right is x:
                y.parent.right = y

        x.right = y.left

        if x.right is not None:
            x.right.parent = x

        y.left = x
        x.parent = y

        update_height(x)
        update_height(y)

    def right_rotate(self, x):
        y = x.left
        y.parent = x.parent

        if y.parent is None:
            self.root = y

        else:

            if y.parent.left is x:
                y.parent.left = y

            elif y.parent.right is x:
                y.parent.right = y

        x.left = y.right

        if x.left is not None:
            x.left.parent = x

        y.right = x
        x.parent = y

        update_height(x)
        update_height(y)

    def insert(self, t):
        node = bst.BST.INSERT(self, t)
        self.rebalance(node)

    def rebalance(self, node):

        while node is not None:

            update_height(node)
            if height(node.left) >= 2 + height(node.right):

                if height(node.left.left) >= height(node.left.right):
                    self.right_rotate(node)

                else:
                    self.left_rotate(node.left)
                    self.right_rotate(node)

            elif height(node.right) >= 2 + height(node.left):

                if height(node.right.right) >= height(node.right.left):
                    self.left_rotate(node)

                else:
                    self.right_rotate(node.right)
                    self.left_rotate(node)

            node = node.parent

    def delete_min(self):
        node, parent = bst.BST.DELETE_MIN(self)
        self.rebalance(parent)

# ___________________________________________________________
#                                                       TEST

def build_test_tree():
    import random

    items = (random.randrange(100) for i in xrange(20))

    tree = AVL()
    node = None

    found = False
    for item in items:
        if not found and item % 2 == 0:
            node = tree.INSERT(item)

            found = True
            continue

        tree.INSERT(item)

    return (node, tree)

if __name__ == '__main__':

    (node, tree) = build_test_tree()

    print
    print tree
    print
    print "Search for: %s, found: %s" % (node.key, tree.SEARCH(node.key).key)
    print
    print "Delete node: %s" % node.key
    tree.DELETE(node)
    print
    print tree
