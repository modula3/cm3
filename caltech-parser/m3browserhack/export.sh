#!/bin/sh
RESTORE=`pwd`
PWDNAMEUTIL=m3browserhack/pwdname.sh
for NEXT in $*
do

# make subdirectory visible in m3browser
cd $RESTORE
cd $NEXT
#if [ `pwdname` = "src" ]
#then
#	cd ..
#fi
echo "examining directory"
LIB=`../../$PWDNAMEUTIL`
cd ..
DIR=`../$PWDNAMEUTIL`
cd ..
FULL=$DIR/$LIB
echo "adding $FULL"

#currently m3browser only looks at these two files:
BINDIR=`cat .bindir`
TARGET=$BINDIR/.M3EXPORTS
WEB=$BINDIR/.M3WEB


DEST=".browserhack_"$DIR"_"$LIB
UTIL=m3browserhack/$BINDIR/m3browserhack
mkdir $DEST 2> /dev/null
mkdir $DEST/$BINDIR 2> /dev/null
echo "creating exports"
$UTIL $FULL/$TARGET $DEST/$TARGET $FULL
cp $FULL/$WEB $DEST/$WEB
echo "done."

done
