INTERFACE M3LTool;

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

IMPORT M3Context, M3Args;

PROCEDURE Initialise(): M3Args.T RAISES {};
(* Requires explicit initialisation because it calls 
   M3LBackEnd.RegisterArgs, which is a procedure variable
   initialised by a module unknown to this one.
   Returns the tool handle.
*)

PROCEDURE Run(c: M3Context.T; do_depends := TRUE): INTEGER RAISES {};
(* Run the pre-linker tool on 'c'. Any error returns < 0.
   If "do_depends = FALSE", the "pl_depends_on" data is assumed
   to have been computred already. *)

END M3LTool.
