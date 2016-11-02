# General notes

### Requirements

Requires Python **2.5.x**

### Python code notes

- In book array index starts from ```1```. That is very convenient because algorithm
  listings where last array element should be used is just ```A.length```. In Python code
  should be ```A.length - 1```

- Array length and last element index

```
# in book examples
A = [1, 2, 3, 4, 5] => array length is 5 is equal to the last element index 5

# in Python code
A = [0, 1, 2, 3, 4] => array length is 5 but last element index is len(A) - 1
```

- Remember that Python ```range``` function do not include upper bound and array index
  starts from ```0```. For example:

```python
for i in range(1, len(A)):
   ...
```
  it means: all elements except the first one. No need to say ```len(A) - 1``` because last
  element is not addressed.

- Like in ```range``` slicing do not include last e.g ```A[start : end]``` - items from ```start```
  through ```end - 1```

- Algorithms intensively works with indices, use to use them

- ```//``` floor divide is a convenient way to take lower bound

- If you can't calculate it in the procedure - pass it as parameter
