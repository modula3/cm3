. ./0env.sh

set -e
set -x

$sudo -s
mkdir -p $sysroot
cd $sysroot

find . | grep \\.lproj$ | xargs rm -rf
find . | grep \\.png$ | xargs rm -rf
find . | grep \\.plist$ | xargs rm -rf
find . | grep \\.artwork$ | xargs rm -rf
find . | grep \\.aiff$ | xargs rm -rf
find . | grep \\.aif$ | xargs rm -rf
find . | grep \\.strings$ | xargs rm -rf
find . | grep \\.wav$ | xargs rm -rf
find . | grep \\.pdf$ | xargs rm -rf
find . | grep Resources$ | xargs rm -rf
find . | grep /calldate.db$ | xargs rm -rf
rm -rf usr/lib/terminfo
