INTERFACE M3CBE_C_Tool;

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

(* This tool provides the user control over which target machine (C compiler)
is used to parameterise M3CBackEnd_C. *)

PROCEDURE ToolInit() RAISES {};
(* Register all the targets as options to the tool.  This must be 
   explicitly initialised because the argumeent registration must occur
   after the targets have registered themselves. This procedure must
   be called before handling help information. *)

PROCEDURE Init(): INTEGER RAISES {};
(*  Initialise the C back-end to the one selected by the user,
    or the default if unspecified. Returns < 0  if error in target choice.
*)

CONST  (* M3Args *)
  CCTarget_Arg = "CCTarget";  (* /l *)

END M3CBE_C_Tool.
