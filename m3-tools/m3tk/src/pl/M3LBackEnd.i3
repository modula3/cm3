INTERFACE M3LBackEnd;

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

IMPORT M3Context, M3Args, M3LInitOrder, M3LTypeCodes;

(* This module defines the interface between the pre-linker front-end 
and a back-end. In order to support multiple back-ends simultaneously, 
the interface is in terms of procedure variables, which must be assigned 
by an appropriate implementation. *)

TYPE
  RunProc = PROCEDURE(
    c: M3Context.T; 
    h: M3LInitOrder.T;
    tcl: M3LTypeCodes.T
    ): INTEGER RAISES {};
(* Activate the pre-linker back end on context 'c, h'.  Result < 0 if error *)

  RegisterArgsProc = PROCEDURE(t: M3Args.T) RAISES {};
(* To register any back-end specific arguments *)

  HardWiredProc = PROCEDURE(interface, proc: TEXT): BOOLEAN RAISES {};
(* Is 'interface.proc' hard-wired in the implementation, and therefore no 
   message about it being unimplemented need be generated. *)
   

VAR
  Run: RunProc;
  RegisterArgs: RegisterArgsProc;
  HardWired: HardWiredProc;

END M3LBackEnd.
