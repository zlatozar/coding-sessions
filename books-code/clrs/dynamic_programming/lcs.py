# -*- coding: utf-8 -*-

# Chapter 15 p.394

# ___________________________________________________________
#                                                      NOTES

# Longest common subsequence could be used in 'diff' operations

# ___________________________________________________________
#                                             IMPLEMENTATION

import pprint
pp = pprint.PrettyPrinter(indent=4)

def LCS_LENGTH(X, Y):

    m = len(X)
    n = len(Y)

    c = []
    b = []
    for i in range((m + 1)):
        c.append([0] * (n + 1))
        b.append(['00'] * (n + 1))

    for i in range(1, m + 1):

        for j in range(1, n + 1):

            if X[i - 1] == Y[j - 1]:
                c[i][j] = c[i - 1][j - 1] + 1
                b[i][j] = '\\'

            elif c[i - 1][j] >= c[i][j - 1]:
                c[i][j] = c[i - 1][j]
                b[i][j] = '^^'

            else:
                c[i][j] = c[i][j - 1]
                b[i][j] = '<<'

    return b, c

def PRINT_LCS(b, X, i, j):

    if i == 0 or j == 0:
        return

    if b[i][j] == '\\':
        PRINT_LCS(b, X, i - 1, j - 1)
        print X[i - 1]

    elif b[i][j] == '^^':
        PRINT_LCS(b, X, i - 1, j)

    else:
        PRINT_LCS(b, X, i, j - 1)

# ___________________________________________________________
#                                                       TEST

if __name__ == '__main__':
    X = 'ABCBDAB'
    Y = 'BDCABA'

    print 'X = %s' % X
    print 'Y = %s' % Y
    b, c = LCS_LENGTH(X, Y)

    print '\nc ='
    pp.pprint(c)
    print '\nb ='
    pp.pprint(b)

    print '\nLCS ='
    PRINT_LCS(b, X, len(X), len(Y))
