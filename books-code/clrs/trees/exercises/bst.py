#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Chapter 12

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

    # Exercise 12.2-2
    def MAXIMUM(self, x=None):
        def maximum(x):
            if x.right == None:
                return x
            return maximum(x.right)

        if x != None:
            return maximum(x)
        else:
            return maximum(self.root)

    # Exercise 12.2-2
    def MINIMUM(self, x=None):
        def minimum(x):
            if x.left == None:
                return x
            return minimum(x.left)

        if x != None:
            return minimum(x)
        else:
            return minimum(self.root)

    # Exercise 12.2-3
    def PREDECESSOR(self, x):
        if x.left:
            return self.MAXIMUM(x.left)

        else:
            y = x.parent
            while y != None and y.left == x:
                x = y
                y = x.parent

            return y

    # alternative which is more intuitive
    def INSERT(self, t):
        new = Node(t)

        if self.root == None:
            self.root = new
        else:
            node = self.root

            while True:
                if new.key < node.key:
                    # Go left
                    if node.left == None:
                        node.left = new
                        new.parent = node
                        break

                    node = node.left

                else:
                    # Go right
                    if node.right == None:
                        node.right = new
                        new.parent = node
                        break

                    node = node.right
        return new

    # alternative
    def DELETE(self, z):
        pass

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

    not_existing = 1000

    print
    print tree
    print
    print "Min: %s" % tree.MINIMUM().key
    print "Max: %s" % tree.MAXIMUM().key
    print
    print "The predecessor of %s is %s" % (node.key, tree.PREDECESSOR(node).key if tree.PREDECESSOR(node) else None)
