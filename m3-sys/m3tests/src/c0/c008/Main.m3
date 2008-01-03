(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: CASE statements *)

MODULE Main;

VAR
  i: INTEGER;
  r: [0..4];
  c: CHAR;

BEGIN

  CASE i OF
  | 0, 1, 2   => c := '1';
  | 3         => c := '2';
  | 7         => c := '3';
  | 4, 8..12  => c := '4';
  END;

  CASE i OF
  | 0, 1, 2   => c := '1';
  | 3         => c := '2';
  | 7         => c := '3';
  | 4, 8..12  => c := '4';
  ELSE           c := '5';
  END;

  CASE i OF
  | 0, 1, 2   => c := '1';
  | 3         => c := '2';
  | 7         => c := '3';
  | 4, 8..12  => c := '4';
  | 50        => c := '5';
  END;

  CASE i OF
  | 0, 1, 2   => c := '1';
  | 3         => c := '2';
  | 7         => c := '3';
  | 4, 8..12  => c := '4';
  | 50        => c := '5';
  ELSE           c := '6';
  END;

  CASE r OF
  | 0, 2..3   => c := '1';
  | 4         => c := '2';
  | 1         => c := '3';
  END;

END Main.
