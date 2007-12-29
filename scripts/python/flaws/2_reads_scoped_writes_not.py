
#----------------------------------------------------------
# flaw #2
#
# In functions, reads are scoped. Writes are not.
#

A = "1"

def F1():
    A = "2" # not an error

def F2():
    #B = A # error
    A = "3"
    
F1()
F2()
print(A)
