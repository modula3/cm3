#!/bin/sh -x
#
# $Id$
#

# ok this is broken
CMD="time ../FreeBSD4/x -respectbbox"

${CMD} test1
${CMD} test2
${CMD} test3
#${CMD} test4
${CMD} test5
${CMD} test6
${CMD} test7
${CMD} test8
${CMD} test9
${CMD} test12
${CMD} test13
${CMD} test14
#${CMD} test15

${CMD} test10
${CMD} test11
${CMD} -slashesaredots test16
