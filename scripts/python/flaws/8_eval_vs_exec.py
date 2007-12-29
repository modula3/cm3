
#----------------------------------------------------------
# flaw #8
#
# Having to eval expressions but exec statements feels wrong.
# There should just be eval.
#

# eval("print(1)") # error
exec("print(1)") # not an error

exec("1 + 2") # not an error?
eval("1 + 2") # not an error
