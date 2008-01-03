(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 

(* The manual isn't clear on whether a[i,j] should be desugared to
    a^[i]^[j] OR a^[i][j]. *)

MODULE Main;

IMPORT Test;

TYPE Row = REF ARRAY OF INTEGER;
TYPE Mat = REF ARRAY OF Row;

VAR a := NEW (Mat, 4);
BEGIN
  FOR i := 0 TO LAST (a^) DO a^[i] := NEW (Row, 4) END;
  FOR i := 0 TO LAST (a^) DO
    a^[i] := NEW (Row, 4);
    FOR j := 0 TO LAST (a^[i]^) DO
      a^[i]^[j] := 10 * i + j;
    END;
  END;
  FOR i := 0 TO LAST (a^) DO
    FOR j := 0 TO LAST (a[i]^) DO
      Test.checkI (a[i,j], 10 * i + j);
    END;
  END;
  Test.done ();
END Main.
