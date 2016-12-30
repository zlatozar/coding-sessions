## Chapter 12

### BST property

Let **X** be a node in a binary search tree. If **Y** is a node in the _left subtree_
of **X**, then ```y.key <= x.key```. If **Y** is a node in the _right subtree_ of **X**, then
```y.key >= x.key```.

The worst-case running time for most search-tree operations is proportional
to the height of the tree.

### Delete node N with value X from a BST

To begin, we may locate the node containing **X**; if there is no such node, we are
done. If **X** is at a leaf, we can simply delete the leaf. If **X** is at an interior
node **N**, however, we cannot delete that node, because to do so would disconnect the
tree.  We must rearrange the tree in some way so that the BST property is maintained and
yet **X** is no longer present. There are two cases.

_First_, if **N** has only one child, we can replace **N** by that child, and the BST
property will be maintained.

_Second_, suppose **N** has both children present. One strategy is to find the node **M**
with value **Y**, the _smallest element_ in the _right subtree_ of **N**, and _replace_ **X** by
**Y** in node **N**. We can then remove node **M** from the right subtree.
  The BST property continues to hold. The reason is that **X** is greater than everything
in the left subtree of **N**, and so **Y**, being greater than **X** (because **Y** is in
the right subtree of **N**), is also greater than everything in the left subtree of **N**.
Thus, as far as the left subtree of **N** is concerned, **Y** is a suitable element at
**N**. As far as the right subtree of **N** is concerned, **Y** is also suitable as the
root, because **Y** was chosen to be the smallest element in the right subtree.

## Chapter 13

### Red-Black properties

```
1. Every node is either red or black.
2. The root is black.
3. Every leaf (NIL) is black.
4. If a node is red, then both its children are black.
5. For each node, all simple paths from the node to descendant leaves contain the
   same number of black nodes.
```

A red-black tree with ```N``` internal nodes has height at most ```2 * lg(n + 1)```
