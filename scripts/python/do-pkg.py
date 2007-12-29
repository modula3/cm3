import sys
from  sysinfo import *
from  pkgcmds import *
from  pkgmap import *
import os

map = {"basename" : os.path.basename(__file__)}
map.update(vars())
sysinfo.USAGE = """
  %(basename)s [ generic_options ] [ generic_cmd ] pkg+

  will apply the given symbolic command to one or more CM3 packages.

  generic_options:
%(GEN_OPTS)s
  
  generic_cmd:
%(GEN_CMDS)s""" % map

show_usage(sys.argv)

OPTIONS = extract_options(sys.argv[1:])
sysinfo.IGNORE_MISS = True
ACTION = map_action(sys.argv[1:])
ADDARGS = add_action_opts(sys.argv[1:])
P = get_args(sys.argv[1:]) # This should be changed to a list.

a = ("%(ROOT)s/scripts/python/pkgmap.py %(OPTIONS)s %(ADDARGS)s -c \"%(ACTION)s\" %(P)s" % vars())
a = a.replace("  ", " ")
a = a.replace("  ", " ")
print(a)

pkgmap([OPTIONS, ADDARGS, "-c", ACTION] + P.split(" "))
