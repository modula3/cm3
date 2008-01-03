#! /bin/csh -f
# Last modified on Tue Apr 13 14:01:00 PDT 1993 by mcjones

# Usage: gen.sh
# Creates <i1><i2>Tbl.{i3,m3} for <i1>,<i2> in {Atom,Int,Ref,Text}
# and appends corresponding lines to m3makefile

echo "" >> m3makefile
foreach i1 (Atom Int Ref Text)
  foreach i2 (Atom Int Ref Text)
    set e1=$i1
    set e2=$i2
    if("$e1" == "Int") set e1=Integer
    if("$e1" == "Ref") set e1=Refany
    if("$e2" == "Int") set e2=Integer
    if("$e2" == "Ref") set e2=Refany
    set b=$i1${i2}Tbl
    echo "(* Copyright 1993 Digital Equipment Corporation. *)"      > $b.i3
    echo "(* Distributed only by permission. *)"                    >>$b.i3
    echo "(* Last modified on `date` by $USER *)"                   >>$b.i3
    echo " "                                                        >>$b.i3
    echo "INTERFACE $i1${i2}Tbl = Table($e1, $e2) END $i1${i2}Tbl." >>$b.i3
    echo "(* Copyright 1993 Digital Equipment Corporation. *)"      > $b.m3
    echo "(* Distributed only by permission. *)"                    >>$b.m3
    echo "(* Last modified on `date` by $USER *)"                   >>$b.m3
    echo " "                                                        >>$b.m3
    echo "MODULE $i1${i2}Tbl = Table($e1, $e2) END $i1${i2}Tbl."    >>$b.m3
    echo "Module($i1${i2}Tbl)" >> m3makefile
  end
end
