(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: PaneMan.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE PaneMan;
IMPORT Starter;
IMPORT StarterList;
IMPORT StartingStarters;
IMPORT PaneManVBT;
IMPORT Debug;
IMPORT Trestle, TrestleComm;
CONST
  DebugLevel = 90;
<* FATAL TrestleComm.Failure *>
PROCEDURE Run(s1,s2,s3,s4,s5,s6,s7: Starter.T := NIL) =
  VAR
    a := ARRAY [1..7] OF Starter.T {s1,s2,s3,s4,s5,s6,s7};
    s: StarterList.T := NIL;
    v: PaneManVBT.T;
  BEGIN
    Debug.S("PaneMan.Run", DebugLevel);
    FOR i := 1 TO LAST(a) DO
      IF a[i] # NIL THEN
        s := StarterList.Cons(a[i],s);
      END;
    END;
    v := NEW(PaneManVBT.T).init(s, StartingStarters.Get());
    Trestle.Install(v);
    Trestle.AwaitDelete(v);
  END Run; 

BEGIN
END PaneMan.
