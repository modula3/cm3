(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 16 13:16:27 PDT 1992 by muller     *)
(*      modified on Wed Jul 31 20:34:55 PDT 1991 by meehan     *)
(*      modified on Tue Jul 14 18:44:38 1987 by chan           *)

INTERFACE MTextDebug;

(* These procedures are used only for debugging. *)

IMPORT MText, MTextPrivate, Wr;

PROCEDURE Dump (m: MText.T);
(* Gives a structural printout of the state of an mtext. *)

PROCEDURE Verify (wr: Wr.T; node: MTextPrivate.Node; msg: TEXT);
(* Verifies that the internal consistency conditions expected of an mtext
   hold for this particular mtext. In case of failure, prints an error
   message, along with the identifying message msg, and aborts the
   program. *)
   
END MTextDebug.

