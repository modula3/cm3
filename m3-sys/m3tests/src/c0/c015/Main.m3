(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: Imbricated TRY FINALLY statements *)

MODULE Main;

PROCEDURE P () =
  VAR j: INTEGER;
  BEGIN

    TRY
      j := 1;
    FINALLY
      j := 2;
    END;

    TRY
      j := 3;
      TRY
        j := 4;
      FINALLY
        j := 5;
      END;
      j := 6;
    FINALLY
      j := 7;
      TRY
        j := 8;
      FINALLY
        j := 9;
      END;
      j := 10;
    END;
  END P;

VAR
  i: INTEGER;

BEGIN
  P();

  TRY
    i := 1;
  FINALLY
    i := 2;
  END;

  TRY

    i := 3;
    TRY
      i := 4;
    FINALLY
      i := 5;
    END;
    i := 6;

  FINALLY

    i := 7;
    TRY
      i := 8;
    FINALLY
      i := 9;
    END;
    i := 10;

  END;

END Main.
