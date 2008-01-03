#! /bin/csh -f
# Last modified on Thu Apr 22 15:05:50 PDT 1993 by mcjones

# Usage: gen.sh
# Creates <i1>ArraySort.{i3,m3} for <i1> in {Int,Text}
# and appends corresponding lines to m3makefile

echo "" >> m3makefile
foreach i1 (Int Text)
    set e1=$i1
    if("$e1" == "Int") set e1=Integer
    if("$e1" == "Ref") set e1=Refany
    set b=${i1}ArraySort
    echo "(* Copyright 1993 Digital Equipment Corporation. *)" > $b.i3
    echo "(* Distributed only by permission. *)"               >>$b.i3
    echo "(* Last modified on `date` by $USER *)"              >>$b.i3
    echo " "                                                   >>$b.i3
    echo "INTERFACE ${i1}ArraySort = ArraySort($e1) END ${i1}ArraySort." >>$b.i3
    echo "(* Copyright 1993 Digital Equipment Corporation. *)" > $b.m3
    echo "(* Distributed only by permission. *)"               >>$b.m3
    echo "(* Last modified on `date` by $USER *)"              >>$b.m3
    echo " "                                                   >>$b.m3
    echo "MODULE ${i1}ArraySort = ArraySort($e1) END ${i1}ArraySort." >>$b.m3
    echo "Module(${i1}ArraySort)" >> m3makefile
end
