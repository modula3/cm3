(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT checkI, done;

TYPE
  X1   = BITS 2 FOR [-1 .. 1];
  Bug1 = RECORD x1: X1 END;

TYPE
  X2   = BITS 3 FOR [0 .. 7];
  Bug2 = RECORD x1: X1;  x2: X2 END;

TYPE
  X3   = BITS 5 FOR [-16 .. 15];
  Bug3 = RECORD x1: X1;  x2: X2;  x3: X3 END;

VAR
  B1: Bug1;
  B2: Bug2;
  B3: Bug3;

BEGIN
  FOR i := FIRST(X1) TO LAST(X1) DO
    B1.x1 := i;
    checkI (B1.x1, i);
  END;

  FOR i := FIRST(X2) TO LAST(X2) DO
    B2.x2 := i;
    checkI (B2.x2, i);
  END;

  FOR i := FIRST(X3) TO LAST(X3) DO
    B3.x3 := i;
    checkI (B3.x3, i);
  END;

  done ();
END Main.
