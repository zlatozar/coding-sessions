# -*- coding: utf-8 -*-

# Chapter 10 p.236

# ___________________________________________________________
#                                                      NOTES

# The clue is that data should be wrapped with meta-information about connection to the
# next element. In our implementation is the Node object.

# ___________________________________________________________
#                                             IMPLEMENTATION

class Node:

    def __init__(self, data=None, next_node=None):
        self.data = data
        self.next_node = next_node

    def get_data(self):
        return self.data

    def get_next(self):
        return self.next_node

    def set_next(self, new_next):
        self.next_node = new_next


class LinkedList:

    def SEARCH(self, data):
        current = self.head
        found = False

        while current and found is False:
            if current.get_data() == data:
                found = True
            else:
                current = current.get_next()

        if current is None:
            raise ValueError('Data not in list')

        return current

    def INSERT(self, data):
        new_node = Node(data)
        new_node.set_next(self.head)
        self.head = new_node

    # Fixme
    def INSERT_TAIL(self, data):
        current = self.head
        while current:
            previous = current
            current = current.get_next()
            previous.set_next(Node(data))

    def DELETE(self, data):
        current = self.head
        previous = None
        found = False

        while current and found is False:
            if current.get_data() == data:
                found = True
            else:
                previous = current
                current = current.get_next()

        if current is None:
            raise ValueError('Data not in list')

        if previous is None:
            self.head = current.get_next()
        else:
            previous.set_next(current.get_next())

# ___________________________________________________________
#                                                    HELPERS

    def __init__(self, head=None):
        self.head = head

    def __len__(self):
        current = self.head
        count = 0
        while current:
            count += 1
            current = current.get_next()

        return count

    def __repr__(self):
        current = self.head
        items = '['
        while current:
            items += str(current.data) + '->'
            current = current.get_next()

        return items + ']'
