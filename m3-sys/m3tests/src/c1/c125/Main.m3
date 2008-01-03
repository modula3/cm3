(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: LAST(Subrange) *)

MODULE Main;

TYPE
  Enum = { A, B, C, D };
  Subrange = [Enum.B .. Enum.C];
  Record = RECORD first, last: Subrange;  number: INTEGER END;

VAR r: Record;

BEGIN
  r := Record { first  := FIRST  (Subrange),
                last   := LAST   (Subrange),
                number := NUMBER (Subrange) };
END Main.
