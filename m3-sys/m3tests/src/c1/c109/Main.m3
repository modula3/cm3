(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Thanks to Regis Crelier for this one *)

UNSAFE MODULE test EXPORTS Main;

IMPORT Text;

TYPE A = REF ARRAY OF CHAR;

VAR
  foo := NEW (A, 10);
  bar := Text.FromChars (foo^);
  fc  := Text.FromChars (LOOPHOLE (foo, A)^);
  fd  := Text.FromChars (SUBARRAY (foo^, 1, 3));
  fb  := Text.FromChars (SUBARRAY (LOOPHOLE (foo, REF ARRAY OF CHAR)^, 1, 3));

BEGIN
  EVAL bar;
  EVAL fc;
  EVAL fd;
  EVAL fb;
END test.
