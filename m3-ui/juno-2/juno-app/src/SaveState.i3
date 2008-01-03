(* Copyright 1995 by Digital Equipment Corp. *)
(* Last  modified on Aug 2 15:31:28 PDT 1995 by gnelson                  *)

INTERFACE SaveState;

(* Routines and types for saving the state of Juno so that it can be recovered
   after a crash. *)

IMPORT Wr, Rd;

TYPE T =
  RECORD
    file, editor, source: TEXT
  END;

(* The name of the current file (possibly "Untitled.juno") and the contents of the
   current editor and source window. *)

PROCEDURE Save(READONLY st: T; wr: Wr.T);
(* Write "st" to "wr". *)

PROCEDURE Restore(VAR st: T; rd: Rd.T): BOOLEAN;
(* Read a saved state from "rd", store the result in "st", and return "TRUE".
   Return "FALSE" if "rd" does not contain a properly saved state. *)

END SaveState.
  