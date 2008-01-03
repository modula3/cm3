(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE 
  Bit = BITS 1 FOR BOOLEAN;
  Aconit = BITS 32 FOR RECORD gres, granit: Bit END;

CONST Twang = Aconit{gres := FALSE, granit := TRUE};

BEGIN
  EVAL Twang;
END Main.
