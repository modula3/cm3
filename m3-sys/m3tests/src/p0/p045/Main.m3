(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT checkI, done;

PROCEDURE app (h: PROCEDURE (i: INTEGER): INTEGER; i: INTEGER): INTEGER =
  BEGIN
    RETURN h (i);
  END app;

PROCEDURE f (x: INTEGER): INTEGER = 

  PROCEDURE g (y: INTEGER): INTEGER =
    BEGIN
      RETURN x + y;
    END g;

  BEGIN
    RETURN app (g, 1);
  END f;

VAR
  i: INTEGER;

BEGIN
  i := f (7);

  checkI (i, 8);

  done ();
END Main.

    
