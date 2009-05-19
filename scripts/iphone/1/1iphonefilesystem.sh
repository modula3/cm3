. ./0env.sh

set -e
set -x

$sudo -s
mkdir -p $sysroot
cd $sysroot

# if iphone goes into power save mode during this, it will
# stall; wake up iphone
# this copies more than necessary but not by a lot
# afterward you can do stuff like
# find ${sysroot} | grep \\.lproj$ | xargs rm -rf
# find ${sysroot} | grep \\.png$ | xargs rm -rf
# find ${sysroot} | grep \\.plist$ | xargs rm -rf
# etc

mkdir -p ./System/Library ./usr
scp -r $iphonessh:/System/Library/Frameworks/ ./System/Library || true
scp -r $iphonessh:/System/Library/PrivateFrameworks/ ./System/Library || true
scp -r $iphonessh:/usr/lib ./usr
