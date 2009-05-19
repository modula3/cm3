. ./0env.sh

set -e
set -x

mkdir -p $build
cd $build
svn co http://iphone-dev.googlecode.com/svn/branches/include-1.2-sdk include
cd include
./configure --prefix=$sysroot/usr
bash install-headers.sh
