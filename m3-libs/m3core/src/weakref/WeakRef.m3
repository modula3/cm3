(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Wed May  5 15:29:31 PDT 1993 by mjordan        *)
(*      modified on Thu Apr 29 17:11:03 1993 by gnelson        *)
(*      modified on Wed Sep 23 12:54:38 PDT 1992 by jdd        *)

MODULE WeakRef;

IMPORT RTWeakRef;

PROCEDURE FromRef (r: REFANY; p: CleanUpProc := NIL): T =
  BEGIN
    RETURN RTWeakRef.WeakRefFromRef(r, p);
  END FromRef;

PROCEDURE ToRef (w: T): REFANY =
  BEGIN
    RETURN RTWeakRef.WeakRefToRef(w);
  END ToRef;

BEGIN
END WeakRef.
