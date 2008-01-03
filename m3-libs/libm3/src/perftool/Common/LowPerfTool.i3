(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last Modified On Wed Mar 23 08:50:50 PST 1994 by kalsow                   *)
(*      Modified On Wed Feb  3 22:48:42 PST 1993 by mjordan                  *)
(*      Modified On Fri May 29 16:46:51 PDT 1992 by muller                   *)


INTERFACE LowPerfTool;

IMPORT File;

(* This interface provides procedures to start performance tools AND
   get a low level communication channel to them. *)

PROCEDURE Start (name: TEXT; VAR r,w: File.T): BOOLEAN;
  (* start the tool name.
     If the operation is successful, return true;
       r is a file handle connected to stdout of the tool;
       w is a file handle connected to stdin of the tool.
     If the operation fails, return false. *)

PROCEDURE StartAndWait (name: TEXT; VAR r,w: File.T): BOOLEAN;
  (* start the tool name.
     If the operation is successul,
       wait until a byte is written by the tool on its stdout and return TRUE; 
       r is a file handle connected to stdout of the tool;
       w is a file handle connected to stdin of the  tool.
     If the operation fails, return FALSE. *)
      
PROCEDURE ParamStart (param: TEXT; VAR r,w: File.T): BOOLEAN;
  (* If the Modula runtime parameter "param" is not set, just
       return FALSE.
     Otherwise, use the value of that parameter to determine the name
       of the tool: it is the name of the parameter if its value is "",
       it is the value otherwise.  Return Start (name, r, w). *)

PROCEDURE ParamStartAndWait (param: TEXT; VAR r,w: File.T): BOOLEAN;
  (* If the Modula runtime parameter "param" is not set, just
       return FALSE.
     Otherwise, use the value of that parameter to determine the name
       of the tool: it is the name of the parameter if its value is "",
       it is the value otherwise.  Return StartAndWait (name, r,w). *)

END LowPerfTool.
