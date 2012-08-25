#!/bin/sh

set -e
set -x

rm -f x86.txt x86-ms.txt amd64.txt amd64-ms.txt ppc32.txt ppc32-ms.txt

gcc experiment-bitfield.c
./a.out > x86.txt

gcc -m64 experiment-bitfield.c
./a.out > amd64.txt

gcc -arch ppc experiment-bitfield.c
./a.out > ppc32.txt
