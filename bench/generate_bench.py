#!/usr/bin/python
# -*- coding: iso-8859-1 -*-
import sys
import os

def gen(n):
    header = ("#############  Benchmarks for POR ############\n"
              "# %d IN.OUT in parallel" % n)
    res = header + "\n"
    res += ("\n# Channels\n")
    res += "\n".join([("free c%d." % i) for i in range(n)])
    res += ("\n\n# Public constant\nfree ok.\n")

    res += ("let P = \n")
    res += "\n".join([("new n%d;" % i) for i in range(n)])
    res += "\n"
    res += "\n".join([
            ("( in(c%d, x); if x = ok then out(c%d, o%d) ) |" % (i,i,i))
            for i in range(n-1)])
    res += ("\n( in(c%d, x); if x = ok then out(c%d, o%d) ).\n\n" % (n-1,n-1,n-1))

    res += "equivalence P and P."
    return(res)

n = int(sys.argv[1])
path = sys.argv[2]
file = open(path + ("Simple_Benchmarks_Graph_%d_par.txt" % n), 'w')
file.write(gen(n))
file.close()
