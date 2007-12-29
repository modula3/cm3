
#----------------------------------------------------------
# flaw #1
# not lexically scopied
# Functions have their own locals, but other blocks do not.
# This is true both for "normal" variables and lexically nested functions.
#

#
# This is probably largely an outcome of not declaring variables?
#

A = "A1:global"

def F1():
    A = "A1:in first F1"
    print "F1:global"

if True:
    A = "A1:in if"
    def F1():
        A = "A1:in if.F1" # This does the right thing.
        print "F1:in if"
    F1() # This does the right thing.

# This should go to "global" but it goes to "in if"
F1()

def F2():
    A = "A1:in F2"
    def F1():
        A = "A1:in F2.F1"
        print "F1:in F2"

# Given how if behaved, you'd expect this to go to F2.F1 but it does not.
F1()

# This should be "global" but is "in if".
print("A is " + A)
