INTERFACE M3CWarnTool;

(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)

IMPORT M3Args;

PROCEDURE Get(): M3Args.T RAISES {};
(* return tool handle *)

PROCEDURE Init() RAISES {};
(* Initialise the 'warning' tool, which registers extra passes
   with the compiler front-end.  *)

CONST (* M3Args *)
  WE_Arg = "WarnExceptions"; (* /f *)
  WU_Arg = "WarnUses";       (* /f *)
  WR_Arg = "WarnReturns";    (* /f *)
  WN_Arg = "WarnNarrows";    (* /f *)
  WA_Arg = "WarnAll";        (* /f *)
  WIF_Arg = "WarnIgnoreFor"; (* /f *)
  WO_Arg = "WarnObsolete";   (* /f *)

END M3CWarnTool.

