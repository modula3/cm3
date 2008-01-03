(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT checkI, done;

TYPE
    T = REF RECORD
        a: INTEGER := 5;
        b: INTEGER := 15;
    END;

VAR
    a : T;

BEGIN
    (* Does not intialize a.a, and a.b to 5, and 15 as specified by the NEW 
       function description in the report *)
    a := NEW (T);

    checkI (a.a, 5);
    checkI (a.b, 15);   

    done ();
END Main.

