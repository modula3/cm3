#!/bin/sh

set -e
set -x
exec "/cygdrive/C/Program Files/PuTTY/pscp.exe" -i "`/bin/cygpath.exe -w "$HOME/.ssh/id_rsa.ppk"`" $@
