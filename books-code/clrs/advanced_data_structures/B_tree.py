# -*- coding: utf-8 -*-

# Chapter 18 p.491

# ___________________________________________________________
#                                                      NOTES


# ___________________________________________________________
#                                             IMPLEMENTATION

class BTreeNode:

    def __init__(self):
        self.keys = []
        self.c = []     # childern
        self.n = 0      # elements count

        self.leaf = True

class BTree:

    def __init__(self):
        self.root = None
        self.t = 2      # minimum degree

    def B_TREE_CREATE(self, data):
        x = BTreeNode()

        x.leaf = True
        x.keys.insert(0, data)
        x.n = 1

        # DISK_WRITE(x)
        self.root = x

    def B_TREE_SPLIT_CHILD(self, x, i):
        z = BTreeNode()

        y = x.c[i]

        z.leaf = y.leaf
        z.n = self.t - 1

        for j in range(0, self.t - 1):
            self.__upsert(z.keys, j, y.keys[j + self.t])

        if not y.leaf:
            for j in range(0, self.t):
                self.__upsert(z.c, j, y.c[j + self.t])

        y.n = self.t - 1

        for j in reversed(range(i, x.n + 1)):
            self.__upsert(x.c, j + 1, x.c[j])

        x.c[i + 1] = z

        for j in reversed(range(i, x.n)):
            self.__upsert(x.keys, j + 1, x.keys[j])

        self.__upsert(x.keys, i, y.keys[self.t - 1])

        x.n = x.n + 1

        # DISK_WRITE(y)
        # DISK_WRITE(z)
        # DISK_WRITE(x)

    def B_TREE_INSERT(self, value):

        if self.root == None:
            self.B_TREE_CREATE(value)
            return

        r = self.root
        if r.n == 2 * self.t - 1:
            s = BTreeNode()

            self.root = s
            s.leaf = False
            s.n = 0
            s.c.insert(0, r)

            self.B_TREE_SPLIT_CHILD(s, 0)
            self.B_TREE_INSERT_NON_FULL(s, value)

        else:
            self.B_TREE_INSERT_NON_FULL(r, value)

    def B_TREE_INSERT_NON_FULL(self, x, value):
        i = x.n

        if x.leaf:
            while i > 0 and value < x.keys[i - 1]:
                self.__upsert(x.keys, i, x.keys[i - 1])
                i = i - 1

            self.__upsert(x.keys, i, value)
            x.n = x.n + 1
            # DISK_WRITE(x)

        else:
            while i > 0 and value < x.keys[i - 1]:
                i = i - 1

            # DISK_READ(x.c[i])
            if x.c[i].n == 2 * self.t - 1:
                self.B_TREE_SPLIT_CHILD(x, i)

                if value > x.keys[i]:
                    i = i + 1

            self.B_TREE_INSERT_NON_FULL(x.c[i], value)

    def B_TREE_SEARCH(self, element):

        def search_element(x, element):
            i = 0
            while i < x.n and element > x.keys[i]:
                i = i + 1

            if i < x.n and  element == x.keys[i]:
                return x, i

            elif x.leaf:
                return None

            else:
                # DISK_READ(x.c[i])
                return search_element(x.c[i], element)

        return search_element(self.root, element)

# ___________________________________________________________
#                                                    HELPERS

    def __upsert(self, arr, index, value):
        if len(arr) <= index:
            arr.insert(index, value)

        else:
            arr[index] = value

    def b_tree_traverse(self):
        self.result = ""

        # with side effect - change 'self.result'
        def tree_traversel(x):
            i = 0
            if not x.leaf:
                is_read = [False] * len(x.c)

                while i <= x.n - 1:
                    if not is_read[i]:
                        tree_traversel(x.c[i])
                        is_read[i] = True

                    self.result += str(x.keys[i]) + ", \n"
                    if not is_read[i + 1]:
                        tree_traversel(x.c[i + 1])
                        is_read[i + 1] = True

                    i = i + 1
            else:
                while i <= x.n - 1:
                    self.result += str(x.keys[i]) + ":"
                    i = i + 1

            return self.result

        if self.root != None:
            tree_traversel(self.root)

        return self.result

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':

    L = [25, 11, 13, 12, 51, 61, 81, 72, 99, 54, 1, 7, 14, 21, 27, 92, 9, 29, 71, 91 ,94]
    tree =  BTree()

    print "\nInserting: %s" %L
    for i in L:
        tree.B_TREE_INSERT(i)

    print
    print "Searching 51 ... found %s" % tree.B_TREE_SEARCH(51)[0].keys
    print "Searching 11 ... found %s" % tree.B_TREE_SEARCH(11)[0].keys
    print "Searching 99 ... found %s" % tree.B_TREE_SEARCH(99)[0].keys
    print "Searching missing 98 ... found %s" % tree.B_TREE_SEARCH(98)
    print "Searching missing 32 ... found %s" % tree.B_TREE_SEARCH(32)
    print "Searching 7 ... found %s" % tree.B_TREE_SEARCH(7)[0].keys
    print "Searching missing 28 ... found %s" % tree.B_TREE_SEARCH(28)
    print
    print "Here is the tree:\n\n%s\n" % tree.b_tree_traverse()
