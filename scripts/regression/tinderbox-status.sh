#!/bin/bash

# This script generates a tinderbox header suitable for use as 
# header for build logs sent to a tinderbox-server.
#
# Start it without parameters in order to get help.

TREE_NAME=$1
BUILD_NAME=$2
STATUS=$3
STARTTIME=$4

if [ -z "${TREE_NAME}" -o -z "${BUILD_NAME}" -o -z "${STATUS}" -o -z "${STARTTIME}" ]
then
     echo ""
     echo "example:"
     echo "   $0 firefox debian-sarge-i386-gcc-4.1 building 1199624301"
     echo ""
     echo "usage:"
     echo "   $0 <Tinderbox-tree-name> <build-name> <status> <starttime>"
     echo "where"
     echo "   <Tinderbox-tree-name>   a tree-name as defined in the"
     echo "                           tinderbox-configuration"
     echo "   <build-name>            will be the row-title on the"
     echo "                           tinderbox build status page"
     echo "   <status>                one of "building", "build_failed","
     echo "                           "test_failed" or "success""
     echo "   <starttime>             tinderbox-build-number in unix time format,"
     echo "                           get it with:"
     echo "                           date 'date +%s'"
     echo "                           once for each build"
     echo ""
     echo "This script outputs the mail header that can be prepended"
     echo "to the build log when sending the status mail."
	 echo ""
	 echo "Tinderbox identifies builds by starttime, so if you want to send"
	 echo "multiple status messages for the same build, be sure to always"
	 echo "use the same starttime"
     echo ""
     echo "ATTENTION: This script can not check validity of parameters."
     echo "           Wrong tree-name or status will make the tinderbox"
     echo "           ignore the status-mail."
     exit 1
fi

echo ""
echo tinderbox: tree: $TREE_NAME
echo tinderbox: starttime: $STARTTIME
echo tinderbox: timenow: `date`
echo tinderbox: status: $STATUS
echo tinderbox: buildname: $BUILD_NAME
echo tinderbox: errorparser: unix
echo tinderbox: END
echo ""
