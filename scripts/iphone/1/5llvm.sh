. ./0env.sh

set -e
set -x

# If there are errors and you want to edit and resume,
# comment out the rm -rf, the git clone, usually
# the configure -- just cd and make

rm -rf $gcc
git clone git://git.saurik.com/llvm-gcc-4.2 $gcc
rm -rf $gcc/intl
rm -rf $build/gcc
mkdir -p $build/gcc
cd $build/gcc
$gcc/configure -target=$target -prefix=$prefix \
    -with-sysroot -enable-wchar_t=no -disable-nls \
    -enable-languages=c,objc,c++,obj-c++ \
    --with-as="${prefix}"/bin/"${target}"-as \
    --with-ld="${prefix}"/bin/"${target}"-ld \

# remove gmp/mpfr/iconv dependencies

sed -ie "s/HOST_GMPLIBS = -lmpfr -lgmp/HOST_GMPLIBS = /" Makefile
$GMAKE configure-host
for a in libcpp/config.h gcc/auto-host.h; do
    sed -i "" -e "s/^#define HAVE_ICONV.*$//" $a
done
for a in gcc/Makefile libcpp/Makefile; do
    sed -i "" -e "s/^LIB_ICONV.*$//" $a
done

$GMAKE
$GMAKE install
