INTERFACE M3CStdActualS;

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

IMPORT M3CStdProcs;


(* Neither of the procedures below deal with NEW; NEW is handled separately
in M3CNEWActualS *)

PROCEDURE Set(call: M3AST_AS.Call; pf: M3CStdProcs.T) RAISES {};
(* set the sm_actual_s field for a call of a standard procedure or function.
Computation depends on the following attributes being set:
sm_def                  for used identifiers
sm_concrete_type_spec   for opaque types
sm_type_spec            for named types
sm_exp_type_spec        for the 'as_callexp' field of procedure calls and on
                        expressions which are, in fact, named types (e.g. in
                        FIRST(Enum), 'Enum' looks like an expression but is
                        really a named type).
*)

PROCEDURE TypeCheck(
    call: M3AST_AS.Call;
    pf: M3CStdProcs.T;
    safe: BOOLEAN)
    RAISES {};
(* type check the arguments of the given standard procedure or function call.
Assumes that Set has already been called to set up the sm_actual_s field *)

END M3CStdActualS.
