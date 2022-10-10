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
    bp  : ADDRESS;        (* base pointer *)
    lock: INTEGER;        (* to ensure that cxt isn't overrun!! *)
    excRef  : ADDRESS;    (* ref to the exception activation *)
    tTypeIndex : INTEGER; (* tTypeIndex from the unwinder *)
    cursor : ADDRESS;     (* libunwind cursor to cur frame *)
    startIP : ADDRESS;    (* libunwind start ip of current proc *)
    endIP : ADDRESS;      (* libunwind end ip of current proc *)
    lsda : ADDRESS;       (* libunwind lsda *)
    persFn : ADDRESS;     (* libunwind handler pers fn *)
    landingPad : ADDRESS; (* libunwind landing pad - the IP of the handler *)
  END;

END RTMachine.
