(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last Modified On Fri May 29 16:53:29 PDT 1992 by muller                   *)


INTERFACE PerfTool;

IMPORT Rd, Wr, Text;

(* This interface provides procedures to start performance tools. *)

PROCEDURE Start (name: Text.T; VAR rd: Rd.T; VAR wr: Wr.T): BOOLEAN;
  (* start the tool name.
     If the operation is successful, return true;
       rd is a reader connected to stdout of the tool;
       wr is a writer connected to stdin of the tool.
     If the operation fails, return false. *)

PROCEDURE StartAndWait (name: Text.T; VAR rd: Rd.T; VAR wr: Wr.T): BOOLEAN;
  (* start the tool name.
     If the operation is successul,
       wait until the tool writes a character to its stdout and return TRUE; 
       rd is a reader connected to stdout of the tool;
       wr is a writer connected to stdin of the  tool.
     If the operation fails, return FALSE. *)
      
PROCEDURE ParamStart (param: Text.T; VAR rd: Rd.T; wr: Wr.T): BOOLEAN;
  (* If the Modula runtime parameter "param" is not set, just
       return FALSE.
     Otherwise, use the value of that parameter to determine the name
       of the tool: it is the name of the parameter if its value is "",
       it is the value otherwise.  Return Start (name, rd, wr). *)

PROCEDURE ParamStartAndWait (param: Text.T;
      	      	      	     VAR rd: Rd.T; VAR wr: Wr.T): BOOLEAN;
  (* If the Modula runtime parameter "param" is not set, just
       return FALSE.
     Otherwise, use the value of that parameter to determine the name
       of the tool: it is the name of the parameter if its value is "",
       it is the value otherwise.  Return StartAndWait (name, rd, wr). *)


END PerfTool.
