#
# Look for all files src/m3makefile in the CM3 source
# and write their relative paths from ROOT to PKGSDB.
#

import os
import os.path
from sysinfo import PKGSDB, ROOT

def find_packages(argv):
def Callback(Result, Directory, Names):
    if (os.path.split(Directory)[1] != "src"):
        return
    if 	(Directory.find("_darcs") != -1):
        return
    if not "m3makefile" in Names:
        return
    if (not os.path.isfile(os.path.join(Directory, "m3makefile"))):
        return
    Result.append(Directory[len(ROOT) + 1:-4] + "\n")

Result = [ ]

os.path.walk(
    ROOT,
    Callback,
    Result
    )

Result.sort()
open(PKGSDB, "w").writelines(Result)
