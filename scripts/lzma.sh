#
# cd to a directory that has some .tar.gz files, this will give
# you equivalent .tar.lzma files.
#
# This is not done as a matter of course in make-dist because
# it is a bit slow and you might want to test the faster-to-produce
# .tar.gz files first.
#

set -e
set -x
for a in *.tar.gz; do gzip -cd $a > `echo $a | sed s/\.gz$//`; done
for a in *.tar; do lzma $a; done
