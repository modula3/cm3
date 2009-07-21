#!/bin/sh -x
cp ../src/boot/*3 .
sed s/SeekRd\.DiscardPrevious\(self\.rd\)\;//g ../src/boot/RegExpLex.m3 >itemp
sed s/SeekRd/FileRdErr/g itemp >RegExpLex.m3
rm itemp