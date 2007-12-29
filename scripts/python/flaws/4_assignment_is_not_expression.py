
#----------------------------------------------------------
# flaw #4:
#   assignment is not an expression
#

# This should work (seen in Perl code, nice idiom):

#A = [1,2]
#while (B = A.pop()):
#    print(B)

# instead I must say:

A = [1,2]
while (A):
    B = A.pop()
    print(B)

# Even if you reject popping an empty list, ok
# there are PLENTY of applications of this.
