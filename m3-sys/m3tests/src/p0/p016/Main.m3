(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: CASE statements *)

MODULE Main;

FROM Test IMPORT checkI, done;

VAR
  i, j: INTEGER;

BEGIN

  i := 1;
  CASE i OF
  | 0, 1, 2   => j := 1;
  | 3         => j := 2;
  | 7         => j := 3;
  | 4, 8..12  => j := 4;
  ELSE           j := 5;
  END;
  checkI (j, 1);

  i := 3;
  CASE i OF
  | 0, 1, 2   => j := 1;
  | 3         => j := 2;
  | 7         => j := 3;
  | 4, 8..12  => j := 4;
  ELSE           j := 5;
  END;
  checkI (j, 2);

  i := 4;
  CASE i OF
  | 0, 1, 2   => j := 1;
  | 3         => j := 2;
  | 7         => j := 3;
  | 4, 8..12  => j := 4;
  ELSE           j := 5;
  END;
  checkI (j, 4);

  i := 9;
  CASE i OF
  | 0, 1, 2   => j := 1;
  | 3         => j := 2;
  | 7         => j := 3;
  | 4, 8..12  => j := 4;
  ELSE           j := 5;
  END;
  checkI (j, 4);

  i := 12;
  CASE i OF
  | 0, 1, 2   => j := 1;
  | 3         => j := 2;
  | 7         => j := 3;
  | 4, 8..12  => j := 4;
  ELSE           j := 5;
  END;
  checkI (j, 4);

  i := 50;
  CASE i OF
  | 0, 1, 2   => j := 1;
  | 3         => j := 2;
  | 7         => j := 3;
  | 4, 8..12  => j := 4;
  ELSE           j := 5;
  END;
  checkI (j, 5);

  i := 50;
  CASE i OF
  | 0, 1, 2   => j := 1;
  | 3         => j := 2;
  | 7         => j := 3;
  | 4, 8..12  => j := 4;
  | 50        => j := 5;
  ELSE           j := 6;
  END;
  checkI (j, 5);

  i := 49;
  CASE i OF
  | 0, 1, 2   => j := 1;
  | 3         => j := 2;
  | 7         => j := 3;
  | 4, 8..12  => j := 4;
  | 50        => j := 5;
  ELSE           j := 6;
  END;
  checkI (j, 6);

  done ();
END Main.
