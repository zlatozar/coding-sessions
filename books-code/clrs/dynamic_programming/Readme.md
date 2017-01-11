### Chapter 15

**Dynamic programming**, like the _divide-and-conquer_ method, solves problems by
combining the solutions to subproblems. It applies when the subproblems **overlap** - that
is, _when subproblems share subsubproblems_.  A dynamic-programming algorithm solves each
subsubproblem just once and then saves its answer in a table, thereby avoiding the work of
recomputing the answer every time it solves each subsubproblem.

**Optimization problems** are solved using divide-and-conquer. Such problems can have many
possible solutions. Each solution has a value, and we wish to find a solution with the
optimal (minimum or maximum) value. We call such a solution an optimal solution to the
problem, as opposed to the optimal solution, since there may be several solutions that
achieve the optimal value.

When developing a dynamic-programming algorithm, we follow a sequence of four steps:

```
1. Characterize the structure of an optimal solution.
2. Recursively define the value of an optimal solution.
3. Compute the value of an optimal solution, typically in a bottom-up fashion.
4. (optional) Construct an optimal solution from computed information.
```

Dynamic Programming is a powerful technique that can be used to solve many problems in
time O(n2) or O(n3) for which a naive approach would take exponential time. Remember that
there should be only a polynomial number of different subproblem.
