(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Aug 12 11:50:40 PDT 1994 by heydon                   *)
(*      modified on Mon Jun 13 15:02:09 PDT 1994 by shillner                 *)

INTERFACE UFInput;

IMPORT FormsVBT;

EXCEPTION Error(TEXT);

TYPE
  T <: TPublic;
  TPublic = OBJECT METHODS
    next(VAR (*OUT*) cmd: Cmd): BOOLEAN RAISES {Error};
  END;

  Cmd = ROOT OBJECT END;
  NewSetCmd       = Cmd OBJECT arg1: TEXT END;
  FinishedSetsCmd = Cmd OBJECT END;
  FindCmd         = Cmd OBJECT arg1: CARDINAL END;
  UnionCmd        = Cmd OBJECT arg1, arg2: CARDINAL; bothRoots: BOOLEAN END;

(* New "T" objects should only be created by calls to "New" below. Subsequent
   "t.next" operations set "cmd" to the next "NewSetCmd", "FinishSetsCmd",
   "FindCmd", or "UnionCmd" in the input and return TRUE, or return FALSE if
   there are no more commands. *)

PROCEDURE New(fv: FormsVBT.T): T RAISES {Error};
(* Return a new initialized "T" from the form "fv". Raises "Error" in the
   event of some input error. *)

END UFInput.
  
