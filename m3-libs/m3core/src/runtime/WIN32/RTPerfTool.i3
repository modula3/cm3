(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(* Last Modified On Fri Jun 11 14:47:11 PDT 1993 by kalsow                   *)
(*      Modified On Wed Feb  3 22:48:42 PST 1993 by mjordan                  *)
(*      Modified On Fri May 29 16:46:51 PDT 1992 by muller                   *)

INTERFACE RTPerfTool;

(* This interface provides procedures to start performance tools with
   a very low-level communication channel. *)

TYPE Handle = ADDRESS;

PROCEDURE Start (param: TEXT;  VAR w: Handle): BOOLEAN;
(* If the Modula runtime parameter "param" is not set, return "FALSE".
   Otherwise, use the value of that parameter to determine the name
   of the program to start.  If the value of the parameters is "",
   the "param" is used as its value.  Start the program and wait until
   it writes a byte on its stdout file.  If everything succeeds, return
   "TRUE" and set "w" to a handle connected to the tool's stdin file.
   Otherwise, return "FALSE". *)

PROCEDURE Close (w: Handle);
(* Makes a good effort attempt to shutdown the connection to the
   tool attached to "w".  The result of reusing a closed handle is
   undefined. *)

PROCEDURE Send (w: Handle;  at: ADDRESS;  len: CARDINAL): BOOLEAN;
(* Sends "len" bytes of data beginning at address "at" to the
   monitoring tool connected to handle "w".  Returns "TRUE" iff
   the operation was successful.  This routine must be called
   from within a runtime critical section. *)

END RTPerfTool.
