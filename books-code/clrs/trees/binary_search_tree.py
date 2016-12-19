#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Chapter 12

# ___________________________________________________________
#                                                      NOTES

# SEARCH, MINIMUM, MAXIMUM, SUCCESSOR, PREDECESSOR, DELETE and INSERT each one in time O(h)
# for search tree of height h.

# Code highlights:

# Implementation is not very Pythonic but very close to the book's code.

# ___________________________________________________________
#                                             IMPLEMENTATION

class Node:

    def __init__(self, k):
        self.key = k
        self.parent = None
        self.left = None
        self.right = None

class BST(object):

    def __init__(self):
        self.root = None

    # p. 288
    def INORDER_WALK(self):

        def inner(x):
            if x != None:
                inner(x.left)
                print x.key
                inner(x.right)

        inner(self.root)

    # p. 290
    def SEARCH(self, k, x=Node(None)):

        if x != None and x.key == None:
            x = self.root

        if x == None or k == x.key:
            return x

        if k < x.key:
            return self.SEARCH(k, x.left)

        else:
            return self.SEARCH(k, x.right)

    # p. 291
    def ITERATIVE_SEARCH(self, k):
        x = self.root

        while x != None and k != x.key:
            if k < x.key:
                x = x.left

            else:
                x = x.right

        return x

    def MINIMUM(self, x=None):
        if not x:
            x = self.root

        while x.left:
            x = x.left
        return x

    def MAXIMUM(self, x=None):
        if not x:
            x = self.root

        while x.right:
            x = x.right

        return x

    # p. 292
    def SUCCESSOR(self, x):
        if x.right:
            return self.MINIMUM(x.right)

        else:
            # search backward trough the rights
            y = x.parent
            while y != None and y.right == x:
                x = y
                y = x.parent

            return y

    # p. 294
    def INSERT(self, z):
        new = Node(z)

        y = None       # trailing pointer
        x = self.root

        while x != None:
            y = x
            if new.key <= x.key:
                x = x.left
            else:
                x = x.right

        new.parent = y

        if y == None:
            self.root = new

        elif new.key < y.key:
            y.left = new

        else:
            y.right = new

        return new

    # Replace one subtree as a child of its parent with another subtree p. 296
    def __TRANSPLANT(self, u, v):

        if u.parent == None:        # u is the root
            self.root = v

        elif u == u.parent.left:    # u is the left child of its parent
            u.parent.left = v

        else:                       # u is the right child of its parent
            u.parent.right = v

        if v != None:
            v.parent = u.parent

    # p. 298
    def DELETE(self, z):

        if z.left == None:
            self.__TRANSPLANT(z, z.right)

        elif z.right == None:
            self.__TRANSPLANT(z, z.left)

        else:
            y = self.MINIMUM(z.right)

            if y.parent != z:
                self.__TRANSPLANT(y, y.right)
                y.right = z.right
                y.right.parent = y

            self.__TRANSPLANT(z, y)
            y.left = z.left
            y.left.parent = y

# ___________________________________________________________
#                                                    HELPERS

    def __str__(self):
        if self.root is None:
            return '<empty tree>'

        def recurse(node):
            if node is None:
                return [], 0, 0

            label = str(node.key)
            left_lines, left_pos, left_width = recurse(node.left)
            right_lines, right_pos, right_width = recurse(node.right)
            middle = max(right_pos + left_width - left_pos + 1, len(label), 2)
            pos = left_pos + middle // 2
            width = left_pos + middle + right_width - right_pos

            while len(left_lines) < len(right_lines):
                left_lines.append(' ' * left_width)

            while len(right_lines) < len(left_lines):
                right_lines.append(' ' * right_width)

            if (middle - len(label)) % 2 == 1 and node.parent is not None and \
               node is node.parent.left and len(label) < middle:
                label += '.'

            label = label.center(middle, '.')
            if label[0] == '.':
                label = ' ' + label[1:]

            if label[-1] == '.':
                label = label[:-1] + ' '

            lines = [' ' * left_pos + label + ' ' * (right_width - right_pos),
                     ' ' * left_pos + '/' + ' ' * (middle-2) +
                     '\\' + ' ' * (right_width - right_pos)] + \
              [left_line + ' ' * (width - left_width - right_width) + right_line
               for left_line, right_line in zip(left_lines, right_lines)]

            return lines, pos, width

        return '\n'.join(recurse(self.root) [0])

# ___________________________________________________________
#                                                       TEST

def build_test_tree():
    import random

    items = (random.randrange(100) for i in xrange(20))

    tree = BST()
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
    print "Min: %s" % tree.MINIMUM().key
    print "Max: %s" % tree.MAXIMUM().key
    print
    print "Search for: %s, found: %s" % (node.key, tree.SEARCH(node.key).key)
    print "Iterative search for: %s, found: %s" % (node.key, tree.ITERATIVE_SEARCH(node.key).key)
    print
    print "Inorder walk (in sorted order):"
    tree.INORDER_WALK()
    print
    print "The successor of %s is %s" % (node.key, tree.SUCCESSOR(node).key if tree.SUCCESSOR(node) else None)
    print
    print "Delete node: %s" % node.key
    tree.DELETE(node)
    print
    print tree
