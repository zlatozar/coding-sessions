## Chapter 4:

### Divide and conquer technique

**Divide** the problem into a number of subproblems that are smaller instances of the
           same problem.

**Conquer** the subproblems by solving them recursively. If the subproblem sizes are
           small enough, however, just solve the subproblems in a straightforward manner.

**Combine** the solutions to the subproblems into the solution for the original problem.

When the subproblems are large enough to solve recursively, we call that the **recursive**
**case**. Once the subproblems become small enough that we no longer recurse, we say that the
recursion "bottoms out" and that we have gotten down to the **base case**. Sometimes, in
addition to subproblems that are smaller instances of the same problem, we have to solve
subproblems that are _not quite the same as the original problem_. We consider solving such
subproblems as part of the combine step.

### Master theorem

The master method provides a "cookbook" method for solving recurrences of the form:

```
T(n) = a.T(n/b) + f(n)
```

The recurrence describes the running time of an algorithm that divides a problem of size
```n``` into a subproblems, each of size ```n/b```, where ```a``` and ```b``` are positive
constants. The ```a``` subproblems are solved recursively, each in time ```T(n/b)```.  The
function ```f(n)``` encompasses the cost of dividing the problem and combining the results
of the subproblems.
