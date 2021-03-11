#!/bin/sh -x

rm -rf a.out *.s *.o

GFLAGS=-g

ifort ${GFLAGS} -S -W1  -recursive -reentrancy threaded  *.f
