(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

MODULE Server EXPORTS Main;

IMPORT NetObj, Thread, Err, Atom,
       BoardServerX;

<*FATAL Thread.Alerted*>

VAR bs := NEW (BoardServerX.T).init ();
BEGIN
  TRY
    NetObj.Export ("BoardServer", bs);
  EXCEPT 
    NetObj.Error (atom) => Err.Print (Atom.ToText (atom.head)); 
  END;
  LOOP Thread.Pause (10.0d0); END;
END Server. 
