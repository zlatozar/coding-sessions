## Chapter 2

### Insertion sort

- It is efficient algorithm for sorting a small number of elements.

- The algorithm sorts the input numbers **in place**: it rearranges the numbers within the
array ```A```, with at most a constant number of them stored outside the array at any
time. The input array A contains the sorted output sequence when the INSERTION SORT
procedure is finished.

- It is **stable**

**Stable** sorts maintain the order of items that are deemed equal, whereas **unstable** sorts
make no such guarantees. In other words will not exchange equal values. An unstable sort
(of which the most famous sort, quicksort, is an example) wouldn't guarantee anything
about the order of the equals, just that the smaller appear before.

**Normally, the property of stability is important only when satellite data are carried
around with the element being sorted.**

- The best case occurs if the array is already sorted.

- Running time is ```theta of n-squared```

### Loop invariants

**Initialization:** It is true prior to the first iteration of the loop.

**Maintenance:** If it is true before an iteration of the loop, it remains true before the
                 next iteration.

**Termination:** When the loop terminates, the invariant gives us a useful property
                 that helps show that the algorithm is correct.

### Merge sort

- Uses divide and conquer to recursively divide and sort the list

![Merge sort](images/merge_sort_recursion.png "Illustrate merge sort recursion")

- Time Complexity: O(n log n)
- Space Complexity: O(n)
- Stable: Yes
