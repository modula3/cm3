
#----------------------------------------------------------
# flaw #5
#
# for loops suck
#
# It should be easy to iterate from 0 to n, not 0 to n - 1,
# thereby knowing why the loop terminated
#

#This should work:

# print the first even number, if there are any

A = [1, 3]
for i in range(0, len(A)):
    if ((A[i] % 2) == 0):
        print("even number found")
        break;
if (i == len(A)):
    print("no even numbers found")
 
# instead I must write:

i = 0
while (i != len(A)):
    if ((A[i] % 2) == 0):
        print("even number found")
        break;
    i += 1
if (i == len(A)):
    print("no even numbers found")

# with the attendent problem of not being able to "continue" ever, since
# the end of the loop body must be run in order to proceed
