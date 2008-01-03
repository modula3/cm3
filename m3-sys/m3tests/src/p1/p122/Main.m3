(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

IMPORT Test;

PROCEDURE P (): INTEGER =
  BEGIN
    TRY
      RETURN 1;
    FINALLY
      TRY
        RETURN 2;
      EXCEPT ELSE
        (* fall through *)
      END;
    END;
  END P;

PROCEDURE Q (): INTEGER =
  BEGIN
    TRY
      RETURN 1;
    FINALLY
      RETURN 2;
    END;
  END Q;

BEGIN
  Test.checkI (P(), 1);
  Test.checkI (Q(), 2);
  Test.done ();
END Main.
