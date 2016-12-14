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
        # 'parent' display is skipped to avoid endless recursion
        return "%s(l:%s, sib:%s)" % (self.data, self.left_child, self.right_sibling)

tree = None
head = None
l_head = None

def BUILD_TREE(elm, previous=None):

    global tree, head, l_head

    before_node = kTreeNode(previous)

    if len(elm) == 0:
        return

    if previous:

        current = elm[0][0]
        rest    = elm[0][1:]

        if not isinstance(previous, list):
            left_child = kTreeNode(current)
            before_node.left_child = left_child
            left_child.parent = before_node

            l_head.left_child = left_child
            l_head = l_head.left_child

            BUILD_TREE(rest, current)

        else:
            right_sibling = kTreeNode(current)
            before_node.right_sibling = right_sibling

            head.right_sibling = right_sibling
            head = head.right_sibling

            l_head = head
            BUILD_TREE(rest, current)

    else:
        tree = kTreeNode(elm[0])
        head = tree

    l_head = head
    BUILD_TREE(elm[1:], elm[0])

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
    t = [1, [2, [6], [7], [8]], [3], [4, [9, [12]]], [5, [10], [11]]]

    BUILD_TREE(t)
    print tree
