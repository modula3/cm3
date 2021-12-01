(* Copyright (C) 1994, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Sat Nov  9 19:20:08 PST 1996 by heydon  *)
(*      modified on Tue May  2 11:40:32 PDT 1995 by kalsow  *)

(* This interface defines platform (machine + OS) dependent
   types and constants. *)

INTERFACE RTMachine;

(*----------------------------------------------- exception stack walking ---*)
(* The "FrameInfo" type must minimally include fields named "pc" and "sp". *)

CONST
  Has_stack_walker = TRUE;
  (* Indicates whether this platform supports the stack walking functions
     defined in the "RTStack" interface. *)

TYPE
  FrameInfo = RECORD
    pc  : ADDRESS;
    sp  : ADDRESS;
    cursor : ADDRESS; (* libunwind cursor to cur frame *)
    lock: INTEGER;  (* to ensure that cxt isn't overrun!! *)
  END;

END RTMachine.
