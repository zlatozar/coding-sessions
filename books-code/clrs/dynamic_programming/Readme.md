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

### When to apply?

- Optimal substructure

1. You show that a solution to the problem consists of making a choice.
Making this choice leaves one or more subproblems to be solved.

2. You suppose that for a given problem, you are given the choice that leads to an
optimal solution. You do not concern yourself yet with how to determine this
choice. You just assume that it has been given to you.

3. Given this choice, you determine which subproblems ensue and how to best
characterize the resulting space of subproblems.

4. You show that the solutions to the subproblems used within an optimal solution
to the problem must themselves be optimal by using a "cut-and-paste" tech-
nique. You do so by supposing that each of the subproblem solutions is not
optimal and then deriving a contradiction. In particular, by "cutting out" the
nonoptimal solution to each subproblem and "pasting in" the optimal one, you
show that you can get a better solution to the original problem, thus contradict-
ing your supposition that you already had an optimal solution. If an optimal
solution gives rise to more than one subproblem, they are typically so similar
that you can modify the cut-and-paste argument for one to apply to the others
with little effort.

Optimal substructure varies across problem domains in two ways:

1. how many subproblems an optimal solution to the original problem uses, and
2. how many choices we have in determining which subproblem(s) to use in an
optimal solution.

Informally, the running time of a dynamic-programming algorithm depends on
the product of two factors: _the number of subproblems overall and how many
choices we look at for each subproblem._

- Overlapping subproblems

Space of subproblems must be "small" in the sense that a recursive algorithm for the
problem solves the same subproblems over and over, rather than always generating new
subproblems. In contrast, a problem for which a divide-and- conquer approach is suitable
usually generates brand-new problems at each step of the recursion. Dynamic-programming
algorithms typically take advantage of overlapping subproblems by solving each subproblem
once and then storing the solution in a table where it can be looked up when needed, using
constant time per lookup.
