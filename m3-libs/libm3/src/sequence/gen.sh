#! /bin/csh -f
# Last modified on Thu Apr 29 16:40:48 PDT 1993 by mcjones

# Usage: gen.sh
# Creates <i1>{Seq,Rep}.{i3,m3} for <i1> in {Atom,Int,Ref,Text}
# and appends corresponding lines to m3makefile

echo "" >> m3makefile
foreach i1 (Atom Int Ref Text)
  set e1=$i1
  if("$e1" == "Int") set e1=Integer
  if("$e1" == "Ref") set e1=Refany
  set b=${i1}Seq
  echo "(* Copyright 1993 Digital Equipment Corporation. *)" > $b.i3
  echo "(* Distributed only by permission. *)"               >>$b.i3
  echo "(* Last modified on `date` by $USER *)"              >>$b.i3
  echo " "                                                   >>$b.i3
  echo "INTERFACE ${i1}Seq = Sequence($e1) END ${i1}Seq."    >>$b.i3

  echo "(* Copyright 1993 Digital Equipment Corporation. *)" > ${b}Rep.i3
  echo "(* Distributed only by permission. *)"               >>${b}Rep.i3
  echo "(* Last modified on `date` by $USER *)"              >>${b}Rep.i3
  echo " "                                                   >>${b}Rep.i3
  echo "INTERFACE ${i1}SeqRep = SequenceRep($e1, ${i1}Seq) END ${i1}SeqRep." >>${b}Rep.i3

  echo "(* Copyright 1993 Digital Equipment Corporation. *)" > $b.m3
  echo "(* Distributed only by permission. *)"               >>$b.m3
  echo "(* Last modified on `date` by $USER *)"              >>$b.m3
  echo " "                                                   >>$b.m3
  echo "MODULE ${i1}Seq = Sequence($e1, ${i1}Seq, ${i1}SeqRep) END ${i1}Seq." >>$b.m3

  echo "Interface(${i1}SeqRep)" >> m3makefile
  echo "Module(${i1}Seq)" >> m3makefile
end
