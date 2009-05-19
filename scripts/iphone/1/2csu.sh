. ./0env.sh

set -e
set -x

mkdir -p $csu
cd $csu
svn co http://iphone-dev.googlecode.com/svn/trunk/csu .
mkdir -p $sysroot/usr/lib
cp -R *.o $sysroot/usr/lib
cd $sysroot/usr/lib
chmod 644 *.o
cp -Rf crt1.o crt1.10.5.o
cp -Rf dylib1.o dylib1.10.5.o
