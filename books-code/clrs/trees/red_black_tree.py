#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Chapter 12

# ___________________________________________________________
#                                                      NOTES


# ___________________________________________________________
#                                             IMPLEMENTATION

BLACK = 'Black'
RED = 'Red'

class Node():

    def __init__(self, x):
        self.key = x

        self.parent = None
        self.left = None
        self.right = None

        self.color = None

class Red_Black_Tree():

    def __init__(self):

        # sentinel
        self.nil = Node(None)
        self.nil.color = BLACK

        self.root = self.nil

    def INSERT(self, z):

        y = self.nil
        x = self.root

        while x != self.nil:
            y = x
            if x.key < z.key:
                x = x.right
            else:
                x = x.left

        z.parent = y

        if y == self.nil:
            self.root = z

        elif y.key < z.key:
            y.right = z

        else:
            y.left = z

        z.left = self.nil
        z.right = self.nil
        z.color = RED

        self.INSERT_FIX(z)

    def INSERT_FIX(self, z):

        while z.parent.color == RED:

            if z.parent.parent.left == z.parent:
                y = z.parent.parent.right

                if y.color == RED:
                    y.color = BLACK
                    z.parent.color = BLACK
                    z.parent.parent.color = RED
                    z = z.parent.parent

                else:
                    if z == z.parent.right:
                        z = z.parent
                        self.LEFT_ROTATE(z)

                    z.parent.color = BLACK
                    z.parent.parent.color = RED
                    self.RIGHT_ROTATE(z.parent.parent)

            else:
                y = z.parent.parent.left

                if y.color == RED:
                    y.color = BLACK
                    z.parent.color = BLACK
                    z.parent.parent.color = RED
                    z = z.parent.parent

                else:
                    if z == z.parent.left:
                        z = z.parent
                        self.RIGHT_ROTATE(z)

                    z.parent.color = BLACK
                    z.parent.parent.color = RED
                    self.LEFT_ROTATE(z.parent.parent)

        self.root.color = BLACK

    def DELETE(self, z):

        y = z
        y_original_color = y.color

        if z.left == self.nil:
            x = z.right
            self.TRANSPLANT(z, z.right)

        elif z.right == self.nil:
            x = z.left
            self.TRANSPLANT(z, z.left)

        else:
            y = self.MINIMUM(z.right)
            y_original_color = y.color
            x = y.right

            if y.parent == z:
                x.parent = y

            else:
                self.TRANSPLANT(y, y.right)
                y.right = z.right
                y.right.parent = y

            self.TRANSPLANT(z, y)
            y.left = z.left
            y.left.parent = y
            y.color = z.color

        if y_original_color == BLACK:
            self.DELETE_FIX(x)

    def DELETE_FIX(self, x):

        while self.nil != x and x.color == BLACK:

            if x.parent.left == x:
                w = x.parent.right

                if w.color == RED:
                    w.color = BLACK
                    x.parent.color = RED
                    self.LEFT_ROTATE(x.parent)
                    w = x.parent.right

                if w.left.color == BLACK and w.right.color == BLACK:
                    w.color = RED
                    x = x.parent

                else:
                    if w.right.color == BLACK:
                        w.left.color = BLACK
                        w.color = RED
                        self.RIGHT_ROTATE(w)
                        w = x.parent.right

                    w.color = x.parent.color
                    w.right.color = BLACK
                    x.parent.color = BLACK
                    self.LEFT_ROTATE(x.parent)
                    self.root = x

            else:
                w = x.parent.left

                if w.color == RED:
                    w.color = BLACK
                    x.parent.color = RED
                    self.RIGHT_ROTATE(x.parent)
                    w = x.parent.left

                if w.left.color == BLACK and w.right.color == BLACK:
                    w.color = RED
                    x = x.parent

                else:
                    if w.left.color == BLACK:
                        w.right.color = BLACK
                        w.color = RED
                        self.LEFT_ROTATE(w)
                        w = x.parent.left

                    w.color = x.parent.color

                    w.left.color = BLACK
                    x.parent.color = BLACK
                    self.RIGHT_ROTATE(x.parent)
                    self.root = x

        x.color = BLACK

    def TRANSPLANT(self, u, v):
        if u.parent == self.nil:
            self.root = v

        elif u.parent.left == u:
            u.parent.left = v

        else:
            u.parent.right = v

        v.parent = u.parent

    def LEFT_ROTATE(self, z):

        y = z.right
        z.right = y.left

        if y.left != self.nil:
            y.left.parent = z

        y.parent = z.parent

        if z.parent == self.nil:
            self.root = y

        elif z.parent.left == z:
            z.parent.left = y

        else:
            z.parent.right = y

        y.left = z
        z.parent = y


    def RIGHT_ROTATE(self, z):

        y = z.left
        z.right = y.right

        if y.right != self.nil:
            y.right.parent = z

        y.parent = z.parent

        if z.parent == self.nil:
            self.root = y

        elif z.parent.left == z:
            z.parent.left = y

        else:
            z.parent.right = y

        y.right = z
        z.parent = y

    def SEARCH(self, k, x=Node(None)):

        if x != None and x.key == None:
            x = self.root

        if x == None or k == x.key:
            return x

        if k < x.key:
            return self.SEARCH(k, x.left)

        else:
            return self.SEARCH(k, x.right)

    def MINIMUM(self, x=None):
        if not x:
            x = self.root

        while x.left:
            x = x.left
        return x

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

    tree = Red_Black_Tree()
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
