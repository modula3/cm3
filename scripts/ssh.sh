#!/bin/sh

set -e
set -x

#
# There seems no hope of getting this command line into an environment
# variable, so use a wrapper .sh.
#

exec "/cygdrive/C/Program Files/PuTTY/pscp.exe" -i "`/bin/cygpath.exe -w "$HOME/.ssh/id_rsa.ppk"`" $@
