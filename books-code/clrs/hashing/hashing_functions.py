# -*- coding: utf-8 -*-

# Chapter 11 p.262

# ___________________________________________________________
#                                                      NOTES


# ___________________________________________________________
#                                             IMPLEMENTATION

m = 11

class LinearProbing:
    def __init__(self):
        global m
        self.slots = [None] * m

    def insert(self, key):
        global m
        i = 0
        while True:
            pos = (key + i) % m
            if self.slots[pos] == None:
                break
            i += 1

        self.slots[pos] = key

    def show(self):
        print self.slots


class QuadraticProbing:
    def __init__(self):
        global m
        self.slots = [None] * m

    def insert(self, key):
        global m
        i = 0
        while True:
            pos = (key + i + 3 * i * i) % m
            if self.slots[pos] == None:
                break
            i += 1

        self.slots[pos] = key

    def show(self):
        print self.slots


class DoubleHashing:
    def __init__(self):
        global m
        self.slots = [None] * m

    def insert(self, key):
        global m
        i = 0
        h2 = 1 + (key % (m - 1))
        while True:
            pos = (key + i * h2) % m
            if self.slots[pos] == None:
                break
            i += 1

        self.slots[pos] = key

    def show(self):
        print self.slots

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':

    L = LinearProbing()
    Q = QuadraticProbing()
    D = DoubleHashing()

    for i in [0, 22, 31, 4, 15, 28, 17, 88, 59]:
        L.insert(i)
        Q.insert(i)
        D.insert(i)

    L.show()
    Q.show()
    D.show()
