(* Copyright (C) 1994, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Tue May  2 11:42:57 PDT 1995 by kalsow  *)

(* This interface defines platform (machine + OS) dependent
   types and constants. *)

INTERFACE RTMachine;

(*----------------------------------------------- exception stack walking ---*)
(* The "FrameInfo" type must minimally include fields named "pc" and "sp". *)

  Has_stack_walker = FALSE;
  (* Indicates whether this platform supports the stack walking functions
     defined in the "RTStack" interface. *)

TYPE FrameInfo = RECORD pc, sp: ADDRESS END;

END RTMachine.
