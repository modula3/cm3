#!/bin/sh -x

rm -f *.o
ifort -g -c -recursive -reentrancy threaded *.f


