(* Copyright (C) 1994, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Sat Nov 19 09:23:17 PST 1994 by kalsow  *)
(*      modified on Thu Dec 24 15:36:20 PST 1992 by jdd     *)

INTERFACE RTHeapDep;

IMPORT Word, RTMachine;

(* This is the interface to the machine-dependent portion
   of the traced heap allocator and collector. *)

(* The heap page size is machine-dependent, since it might depend on the
   architecture's VM page size (if VM is TRUE).  Otherwise, 8192 bytes is a
   reasonable page size.  The page size must be a power of two. *)

CONST
  BytesPerPage    = RTMachine.BytesPerHeapPage;    (* bytes per page *)
  LogBytesPerPage = RTMachine.LogBytesPerHeapPage;
  AdrPerPage      = RTMachine.AdrPerHeapPage;      (* addresses per page *)
  LogAdrPerPage   = RTMachine.LogAdrPerHeapPage;

TYPE Page = [0 .. Word.Divide(-1, AdrPerPage)];

END RTHeapDep.
