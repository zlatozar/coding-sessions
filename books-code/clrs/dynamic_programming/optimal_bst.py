# -*- coding: utf-8 -*-

# Chapter 15 p. 394

# ___________________________________________________________
#                                                      NOTES

# Having PRINT_OPTIMAL_BST it is possible to build the tree

# ___________________________________________________________
#                                             IMPLEMENTATION

import sys
import pprint
pp = pprint.PrettyPrinter(indent=10)

def OPTIMAL_BST(p, q):
    n = len(p)

    e = []
    w = []
    for i in range(0, n + 1):
        e.append([0] * n)
        w.append([0] * n)

    root = []
    for i in range(0, n):
        root.append([0] * n)

    for i in range(1, n + 1):
	e[i][i - 1] = q[i - 1]
	w[i][i - 1] = q[i - 1]

    for length in range(1, n):
	for i in range(1, n - length + 1):

	    j = i + length - 1
	    e[i][j] = sys.maxint
	    w[i][j] = w[i][j - 1] + p[j] + q[j]
            
            for r in range(i, j + 1):
		val = e[i][r - 1] + e[r + 1][j] + w[i][j]
                
                if val < e[i][j]:
		    e[i][j] = val
		    root[i][j] = r
				
    return e, root

# Exercise 15.5-1 p. 403
def PRINT_OPTIMAL_BST(root):
    
    stack = []
    stack.append(((1, len(root) - 1), ' is root'))

    while stack:
	value = stack.pop()
            
	if type(value[0]) == str:
	    print ''.join(value)
                        
	else:
	    left = value[0][0]
	    right = value[0][1]
	    root_val = root[left][right]
            
            print 'p%d%s' % (root_val , value[1])
            if root_val == left:
		left_val = 'q%d' % (root_val - 1)

            else:
		left_val = (left, root_val - 1)
                        
                                
	    if root_val == right:
		right_val = 'q%d' % root_val

            else:
                right_val = (root_val + 1, right)
				
            stack.append((right_val, ' is right of p%d' % root_val))
	    stack.append((left_val, ' is left of p%d' % root_val))

# ___________________________________________________________
#                                                       TEST


if __name__ == '__main__':
    p = [0,    0.04, 0.06, 0.08, 0.02, 0.1, 0.12, 0.14]
    q = [0.06, 0.06, 0.06, 0.06, 0.05, 0.05, 0.05, 0.05]
    
    e, root = OPTIMAL_BST(p, q)

    print e[1][len(p) - 1]
    print '\ne = '
    pp.pprint(e)
    print
    PRINT_OPTIMAL_BST(root)
