#!/bin/sh -x
#
# $Id$
#

# ok this is broken
TESTS="test1 test2 test3 test5 test6 test7 test8 test9 test12 test13 test14 test10 test11 test21"
#TESTS="test24"

ARCH=`../../../../m3arch.sh`

PRECMD="time ../../pauls2/${ARCH}/y"
CMD="time ../../pauls3/${ARCH}/z -respectbbox -cheapMetal5"
AFTERCMD="time ../${ARCH}/t"

export NOROUTERASSERTIONS
export DEBUGRIPUPS

for t in ${TESTS}; do
	${PRECMD} $t
done

for t in ${TESTS}; do
	${CMD} $t &
done

wait

for t in ${TESTS}; do
	${AFTERCMD} $t
done
