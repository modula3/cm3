INTERFACE M3CConsActualS;

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

IMPORT M3AST_AS;

PROCEDURE Set(constructor: M3AST_AS.Constructor) RAISES {};
(* set the sm_actual_s field for a constructor. Computation depends on the
following attributes being set:
sm_def                  for used identifiers
sm_init_exp             for RECORD field ids
sm_exp_type_spec        for constructors
*)

PROCEDURE TypeCheck(
    constructor: M3AST_AS.Constructor;
    safe: BOOLEAN)
    RAISES {};
(* type check the elements of the given constructor. Assumes that Set has
already been called to set up the 'sm_actual_s' field. 'safe' indicates whether
or not the constructor is in a safe module/interface *)

END M3CConsActualS.
