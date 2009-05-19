. ./0env.sh

set -e
set -x

rm -rf $cctools
svn co http://iphone-dev.googlecode.com/svn/branches/odcctools-9.2-ld $cctools
mkdir -p $build/cctools
cd $build/cctools

# repeat this presumably for every directory in include, at least if not on Darwin
# sed -i -e s/mbstate_t/remove_mbstate_t/g $cctools/include/i386/_types.h

$cctools/configure --target=$target --prefix=$prefix --disable-ld64
$GMAKE
$GMAKE install
