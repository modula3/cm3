(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: Imbricated TRY FINALLY statements *)

MODULE Main;

FROM Test IMPORT checkI, done;

VAR
  i: INTEGER;

BEGIN

  i := 0;

  TRY
    INC (i, 1);
  FINALLY
    INC (i, 2); END;

  TRY
    INC (i, 4);
    TRY
      INC (i, 8);
    FINALLY
      INC (i, 16); END;
    INC (i, 32);
  FINALLY
    INC (i, 64);
    TRY
      INC (i, 128);
    FINALLY
      INC (i, 256); END;
    INC (i, 512); END;

  checkI (i, 1023);

  done ();
END Main.
