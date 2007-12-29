
#----------------------------------------------------------
# flaw #3:
#  lambda is neutered
#  It allows just one expression, and no statements
#

# This should work:

import os

#os.path.walk(
#    "..",
#    lambda(a, b, c):
#        print(b)
#        print(b)
#    None)

# Instead I must say:

def Callback(a, b, c):
    print(b)
    print(b)
    
os.path.walk(
    "..",
    Callback,
    None)

#
# Moving callbacks away from their point of use hurts readability.
# This is probably mitigated by lexical scoping of functions, but
# notice that other blocks are not lexically scoped.
#
