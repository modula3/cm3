(* Copyright (C) 2026, the CM3 contributors.                   *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Capture analysis for nested-procedure lambda-lifting.
   Walks a nested proc's AST and classifies each up-level variable
   reference as read-only or read-write, so the MSIR lowering can
   pass read-only captures by value and read-write captures by pointer. *)

INTERFACE CaptureAnalysis;

IMPORT Variable;

TYPE
  Capture = RECORD
    var     : Variable.T;
    written : BOOLEAN;  (* TRUE => nested proc assigns through this var *)
  END;

  T <: REFANY;

PROCEDURE New (): T;

PROCEDURE Note (ca: T;  v: Variable.T;  written: BOOLEAN);
(* Record that the nested proc captures v.
   If written=TRUE the variable must be a by-pointer param in the lambda-
   lifted signature.  Multiple calls for the same v are OR-ed: once marked
   written it stays written. *)

PROCEDURE GetCaptures (ca: T): REF ARRAY OF Capture;
(* Return a fresh array containing all captures recorded so far,
   in the order first noted. *)

PROCEDURE Remove (ca: T;  v: Variable.T);
(* Remove v from the capture set (no-op if v is not present). *)

END CaptureAnalysis.
