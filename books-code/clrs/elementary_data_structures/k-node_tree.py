# -*- coding: utf-8 -*-

# Chapter 10 p.246

# ___________________________________________________________
#                                                      NOTES

# Instead of having a pointer to each of its children, each node x has only
# two pointers:

# 1. x.left_child points to the leftmost child of node x
# 2. x.right_sibling points to the sibling of x immediately to its right

# If node x has no children, then x.left_child = None , and if node x is the rightmost
# child of its parent, then x.right_sibling = None

# ___________________________________________________________
#                                             IMPLEMENTATION

class kTreeNode:

    def __init__(self, data=None, parent=None, left_child=None, right_sibling=None):
        self.data = data

        self.parent = parent
        self.left_child = left_child
        self.right_sibling = right_sibling

    def __str__(self):
        # 'parent' display is skip to avoid endless recursion
        return "%s(l:%s, sib:%s)" % (self.data, self.left_child, self.right_sibling)


def BUILD_TREE(elm, local_parent=None):

    if len(elm) == 0:
        return

    for i in elm:

        if local_parent:
            previous_node = kTreeNode(local_parent)

            if not isinstance(local_parent, list):
                left_child = kTreeNode(i[0])
                previous_node.left_child = left_child
                left_child.parent = previous_node
                print "parent: %s, left_child: %s" % (local_parent, i[0])

                local_parent = i[0]
                BUILD_TREE(i[1:], local_parent)

            else:
                right_sibling = kTreeNode(i[0])
                previous_node.right_sibling = right_sibling
                print "sibling: %s, right_sibling: %s" % (local_parent[0], i[0])

                local_parent = i[0]
                BUILD_TREE(i[1:], local_parent)

        else:
            # set root
            print "root: %s" % i

        local_parent = i

# Exercise 10.4-4 p. 248
def PRINT_TREE(node):

    # go to the root
    while node.parent != None:
        node = node.parent

    while node != None:
        print node.data
        sibling = node.right_sibling

        while sibling != None:
            print sibling.data
            sibling = sibling.right_sibling

        node = node.left_child

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':

    #          1
    #     /  |  |  \
    #    2   3  4   5
    #  / | \    |  / \
    # 6  7  8   9 10 11
    #           |
    #          12
    #
    #

    t = [1, [2, [6], [7], [8]],    [3],     [4, [9, [12]]],   [5, [10], [11]]]

    print t

    BUILD_TREE(t)
