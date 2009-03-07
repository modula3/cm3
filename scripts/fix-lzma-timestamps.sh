#!/bin/sh

#
# Generally the .tar.gz timestamps are correct and should
# be applied to the .tar.lzma files.
#

set -e
set -x
for a in *.tar.lzma; do
    touch -r `echo $a | sed s/lzma$/gz/` $a
done

