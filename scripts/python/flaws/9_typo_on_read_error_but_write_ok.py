
#----------------------------------------------------------
# flaw #9
#
# Python protects me, via early checking, from typos
# when I read, but not when I write. It should do both.
# That is, I should declare variables.
#

Correct = 1
# proposed typo is Corect

#A = Corect # error, good
Corect = 1 # no error, bad

# For classes, by default, same thing:

class Foo(object):
    def __init__(self):
        self.Correct =1

F = Foo()
# print(F.Corect) # error, good
F.Corect = 1 # no error, bad

# __slots__ fixes this, but is not the default

class Bar(object):
    __slots__ = ["Correct"]
    def __init__(self):
        self.Correct =1

B = Bar()
# print(B.Corect) # error, good
#B.Corect = 1 # error, good

# __slots__ should be the default and then some other syntax
# for "expandable" types, like __slots__ = [ "*" ]
