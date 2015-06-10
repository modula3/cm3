(* Test shift_right with a signed integer in the IR.
   Specifically m3front/src/misc/cg/Setrange.
*)
UNSAFE MODULE Main;
IMPORT Dump;

PROCEDURE F31(start, count: INTEGER) =
(* Fit in word on all machines. *)
TYPE T = SET OF[0..31];
VAR a := T{start..count - 1};
BEGIN  
   Dump.Dump(31, 0, 1, BITSIZE(T), BYTESIZE(T), ADR(a));
END F31;

PROCEDURE F32(start, count: INTEGER) =
(* Fit in word on 64bit machines. *)
TYPE T = SET OF[0..32];
VAR a := T{start..count - 1};
BEGIN  
   Dump.Dump(32, 0, 1, BITSIZE(T), BYTESIZE(T), ADR(a));
END F32;

PROCEDURE F64(start, count: INTEGER) =
(* Fit in word on no machines. *)
TYPE T = SET OF[0..64];
VAR a := T{start..count - 1};
BEGIN
   Dump.Dump(64, 0, 1, BITSIZE(T), BYTESIZE(T), ADR(a));
END F64;

BEGIN
F31(0, 1);
F32(0, 1);
F64(0, 1);
END Main.
