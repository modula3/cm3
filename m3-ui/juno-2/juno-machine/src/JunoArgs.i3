(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jun 30 20:37:17 PDT 1995 by heydon                   *)
(*      modified on Fri Jul 17 17:20:47 PDT 1992 by myers                    *)

INTERFACE JunoArgs;

IMPORT RTVal, JunoValue;

(* "JunoArgs" provides an interface to the Juno runtime stack that is
   useful for implementing external Modula-3 procedures. These procedures
   allow clients to read and write a procedure's actual arguments.

   Where possible, the procedures in this interface convert between the
   "RTVal.T"'s on the machine's run-time stack and Modula-3 types like
   INTEGER, REAL, and TEXT. *)

PROCEDURE ReadValue(i: CARDINAL): RTVal.T;
(* Return the parameter with index "-i". Usually, the parameter is expected to
   have a particular type, in which case one of the "ReadX" procedures below
   should be used. *)

PROCEDURE ReadInt(i: CARDINAL; VAR err: BOOLEAN): INTEGER;
PROCEDURE ReadReal(i: CARDINAL; VAR err: BOOLEAN): JunoValue.Real;
PROCEDURE ReadText(i: CARDINAL; VAR err: BOOLEAN): TEXT;
PROCEDURE ReadPair(i: CARDINAL; VAR err: BOOLEAN): RTVal.Pair;

(* Read the actual parameter of the specified type with index "-i". If the
   value at position "-i" in the stack frame is not the correct type, set
   "err" to "TRUE" and return an undefined value. Otherwise, leave "err"
   unchanged. The "ReadInt" procedure expects to find a real number, and
   returns the result of rounding that real value to the nearest integer. *)

PROCEDURE WriteValue(i: CARDINAL; val: RTVal.T);
(* Write the value "val" as the result parameter with index "-i". *)

PROCEDURE WriteInt(i: CARDINAL; int: INTEGER);
PROCEDURE WriteReal(i: CARDINAL; r: JunoValue.Real);
PROCEDURE WriteText(i: CARDINAL; t: TEXT);
(* Equivalent to: "WriteValue(i, RTVal.FromInt(int))", "WriteValue(i,
   RTVal.FromReal(r))", and "WriteValue(i, RTVal.FromText(t))". *)

PROCEDURE PushValue(v: RTVal.T);
(* Push "v" onto the runtime stack. Requires "v # NIL". *)

END JunoArgs.
