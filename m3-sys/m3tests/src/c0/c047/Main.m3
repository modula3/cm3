(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: RAISE statements *)

MODULE Main;

TYPE Rec = RECORD a, b: INTEGER END;

VAR i : INTEGER;

<*FATAL Error, Failure, BigArg *>
EXCEPTION Error;
EXCEPTION Failure (INTEGER);
EXCEPTION BigArg (Rec);

VAR r: Rec;
BEGIN

  IF (i < 0) THEN RAISE Error; END;

  IF (i > 0) THEN
    RAISE Failure (i);
  ELSIF (i > 3) THEN
    RAISE BigArg (r);
  ELSE
    RAISE Error;
  END;

END Main.
