### Chapter 16

#### Greedy algorithms strategy

A greedy algorithm obtains an optimal solution to a problem by making a sequence
of choices. At each decision point, the algorithm makes choice that seems best at
the moment. This heuristic strategy does not always produce an optimal solution,
but as we saw in the activity-selection problem, sometimes it does.

1. Determine the optimal substructure of the problem.
2. Show that if we make the greedy choice, then only **one** subproblem remains.
3. Greed choice property

#### Design greedy algorithm

1. Cast the optimization problem as one in which we make a choice and are left
with one subproblem to solve.
2. Prove that there is always an optimal solution to the original problem that makes
the greedy choice, so that the greedy choice is always safe.
3. Demonstrate optimal substructure by showing that, having made the greedy
choice, what remains is a subproblem with the property that if we combine an
optimal solution to the subproblem with the greedy choice we have made, we
arrive at an optimal solution to the original problem.

#### Huffman code

```python
class HuffmanNode:
    def __init__(self, char, freq, left, right):
        self.char = char
        self.freq = freq
        self.left = left
        self.right = right

    def __gt__(self, othernode):
        return self.freq > othernode.freq
```
