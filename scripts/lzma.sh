#
# cd to a directory that has some .tar.gz files, this will give
# you equivalent .tar.lzma files.
#
# This is not done as a matter of course in make-dist because
# it is a bit slow and you might want to test the faster-to-produce
# .tar.gz files first.
#

sh -exc 'for a in *.tar.gz ; do zcat $a | lzma > `echo $a | sed s/\.gz$/\.lzma/` ; done'
