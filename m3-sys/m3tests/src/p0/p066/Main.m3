(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;
<*FATAL ANY*>

EXCEPTION Number1;
EXCEPTION Number2;

CONST T = TRUE;

PROCEDURE ReturnFromException() RAISES ANY =
BEGIN
  TRY
    IF T THEN RETURN; END;
  EXCEPT
    Number1 => RETURN;
  END;
END ReturnFromException;

PROCEDURE RaiseException2() RAISES ANY =
BEGIN
  RAISE Number2;
END RaiseException2;

BEGIN
  TRY
    ReturnFromException();
    RaiseException2();
  EXCEPT 
    Number2 => (* DO Nothing *)
  END;

END Main.
