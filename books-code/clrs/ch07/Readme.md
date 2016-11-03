## Chapter 7

### Quicksort

Heapsort is an excellent algorithm, but a **good** implementation of quicksort
usually beats it in practice.

**Divide:** Partition (rearrange) the array ```A[p..r]``` into two (possibly empty) subarrays
```A[p..q-1]``` and ```A[q+1..r]``` such that each element of ```A[p..q-1]``` is less than or equal to A[q],
which is, in turn, less than or equal to each element of ```A[q+1..r]```. Compute the index ```q``` as
part of this partitioning procedure.

**Conquer:** Sort the two subarrays ```A[p..q-1]``` and ```A[q+1..r]``` by recursive calls to quicksort

**Combine:** Because the subarrays are already sorted, no work is needed to combine them:
the entire array ```A[p..r]``` is now sorted.

- The running time of quicksort depends on whether the partitioning is **balanced** or
**unbalanced**, which in turn depends on which elements are used for partitioning.

### Partitioning

- Worst-case partitioning

The worst-case behavior for quicksort occurs when the partitioning routine produces one
subproblem with ```n-1``` elements and one with ```0``` elements.

- Best-case partitioning

If PARTITION produces two subproblems, each of size no more than ```n/2```, since one is of size
```n/2``` and one of size ```n/2 - 1```. In this case, quicksort runs much faster.
