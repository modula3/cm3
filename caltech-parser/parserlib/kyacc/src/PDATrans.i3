(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE PDATrans;
CONST
  Brand = "PDATrans";
TYPE
  ActKind = {Shift, Reduce, Error, Accept,
             ShiftReduce, ShiftAccept, Jump};

(* Jump is introduced in the flatten step.
   jumps are unconditional and have code = -2 *)

  T = RECORD
    code: INTEGER;    (* -1 means "set default", else a symbol code *)
    kind: ActKind;
    target: INTEGER; (* reduce rule number, or shift state number, or 0 *)
  END;
PROCEDURE Compare(a, b: T): [-1..1];
PROCEDURE Equal(a, b: T): BOOLEAN;
PROCEDURE Hash(a: T): INTEGER;
PROCEDURE Format(a: T): TEXT;

PROCEDURE PreShift(a: T; code: INTEGER): T;

END PDATrans.
