
#----------------------------------------------------------
# Flaw #7
#
# This is a compatibility issue.
# print is both a statement and a function, or something.
#

print() # should print just a newline, but prints two parens

# workaround:

print("")
