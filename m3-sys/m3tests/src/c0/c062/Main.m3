(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
UNSAFE MODULE Main;

TYPE
  t1 = RECORD
         c: [12..134];
         d: RECORD 
           z : REF INTEGER;
         END;
         y: REF t1;
         f: UNTRACED REF INTEGER;
         g: UNTRACED REF t2;
         h: PROCEDURE (i: INTEGER);
       END;
  t2 = RECORD
         r: REF t1;
         s: t4;
         u: ARRAY [1..10] OF INTEGER;
         v: ARRAY [10..100] OF REF REAL;
       END;
  t3 = REF t2;
  t4 = REF t1;

  t10 = OBJECT 
         a: INTEGER;
         i: REF INTEGER;
        END;
  
  t11 = t10 OBJECT 
            c: REF INTEGER;
        END;

VAR
  v1: t1;
  v2: t2;
  v3: t3;
  v4: t4;

  v10: t10;
  v11: t11;

BEGIN
  EVAL v1;
  EVAL v2;

  v3 := NEW (t3);
  v3.r := NEW (REF t1);
  v3.v[23] := NEW (REF REAL);

  v4 := NEW (t4);
  v4.d.z := NEW (REF INTEGER);
  v4.y := NEW (REF t1);

  v10 := NEW (t10);
  v11 := NEW (t11);
END Main.
