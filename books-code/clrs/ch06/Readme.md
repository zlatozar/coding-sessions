## Chapter 6

### Heapsort

- _Insertion sort, merge sort, heapsort,_ and _quicksort_ are all **comparison sorts**: they
determine the sorted order of an input array by comparing elements.

- Like merge sort, but unlike insertion sort, heapsort's running time is O(n.lg n). Like
insertion sort, but unlike merge sort, **heapsort sorts in place**: only a constant number of
array elements are stored outside the input array at any time.

- Heapsort also introduces another algorithm design technique: **using a data structure**, in
this case one we call a "heap," to manage information. Not only is the heap data structure
useful for heapsort, but it also makes **an efficient priority queue.**

![Sorting](images/sorting_complexity.png "Sorting time complexity")

The **(binary) heap data structure** is _an array object_ that we can view as a nearly
complete **binary tree**. An array ```A``` that represents a heap is an object with two
attributes: ```A.length```, which (as usual) gives the number of elements in the array, and
```A.heap-size```, which represents how many elements in the heap are stored within array ```A```.

If the heap begins at ```L[1]``` then the left child of the node stored in ```L[i]``` is in ```L[2i]``` and
the right child is in ```L[2i + 1]```. Therefore, the parent of the node stored in ```L[j]``` is in
```L[lower_bound(j/2)]```. More generally, we can put the heap anywhere in the array; it does not have to
begin at location one. If it begins at ```L[lower]``` then the appropriate access functions are:

```
PARENT(i) = lower_bound((i + lower - 1) / 2)
LEFT(i)   = 2i - lower + 1
RIGHT(i)  = 2i - lower + 2

```
Note there is no waste of space.

- There are two kinds of binary heaps: **max-heaps** and **min-heaps**(used in priority queues).
In both kinds, the values in the nodes satisfy a **heap property**, the specifics of which
depend on the kind of heap. In a max-heap, the max-heap property is that for every node i
other than the root,

```
A[PARENT(i) >= A[i]
```

that is, the value of a node is at most the value of its parent.

### Disadvantage

Not a stable sort, that is, the order of equal value elements is not preserved.

### Priority queues

A **priority queue** is a data structure for maintaining a set ```S``` of elements, each
with an associated value called a **key**. There are two kind: **max-priority** and
**min-priority** queues.
