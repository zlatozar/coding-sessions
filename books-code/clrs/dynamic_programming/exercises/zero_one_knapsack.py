#!/usr/bin/env python
#
# -*- coding: utf-8 -*-

# Exercise 16.2-2 p.427

# ___________________________________________________________
#                                                      NOTES

# ___________________________________________________________
#                                             IMPLEMENTATION

def RECURCIVE_KNAPSACK(K, wt, val, n):
    '''
    - K   knapsack maximum weight
    - wt  the object weights
    - val the object values
    - n   number of objects
    '''
 
    if n == 0 or K == 0 :
        return 0
 
    if (wt[n - 1] > K):
        return RECURCIVE_KNAPSACK(K, wt, val, n - 1)
 
    else:
        return max( val[n - 1] + RECURCIVE_KNAPSACK(K - wt[n - 1], wt, val, n - 1),
                                 RECURCIVE_KNAPSACK(K            , wt, val, n - 1) )

    
def ZERO_ONE_KNAPSACK(items, k):

    # Number of items
    n = len(items)
    
    # create empty table with len(items) + 1 columns and k + 1 rows
    table = []
    for i in range(0, k + 1):
        table.append([0] * (len(items) + 1))
    
    for i in range(1, k + 1):
        for j in range(1, len(items) + 1):

            val, wt = items[j - 1]
            if wt > i:
                table[i][j] = table[i][j - 1]

            else:
                table[i][j] = max(table[i][j - 1], val + table[i - wt][j - 1])

    return table[k][len(items)]

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':
    
    knapsack = 50
    val = [60, 20, 120]
    wt =  [10,  100,  30]
    
    print '\nKnapsack capacity: %s' % knapsack
    print 'Values: %s' % val
    print 'Weights: %s' % wt
    print '(recursively) Could take: %s' % RECURCIVE_KNAPSACK(knapsack, wt, val, len(wt))
    print
    val_wt = [(60, 10), (20, 100), (120, 30)]
    print '\nKnapsack capacity: %s' % knapsack
    print 'Value and weight tuples: %s' % val_wt
    print '(dynamic programing) Could take: %s' % ZERO_ONE_KNAPSACK(val_wt, knapsack)
    print
