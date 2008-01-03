INTERFACE M3CActualS;

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

IMPORT AST, ASTWalk;


PROCEDURE Set(
    cl: ASTWalk.Closure;
    an: AST.NODE;
    vm: ASTWalk.VisitMode)
    RAISES {};
(* checks if 'an' is a constructor or a call of a procedure other than the
standard procedure NEW. If so it sets up the 'sm_actual_s' field. Computation
depends on the following attributes being set:
sm_def                  for used identifiers
sm_concrete_type_spec   for opaque types
sm_init_exp             for RECORD field ids
sm_type_spec            for named types
sm_exp_type_spec        for constructors, the 'as_callexp' field of procedure
                        calls and on expressions which are, in fact, named
                        types (e.g. in FIRST(Enum), 'Enum' looks like an
                        expression but is really a named type).
  Calls of NEW do not get their 'sm_actual_s' field set up until the type
checking phase. The distinction is made because it is more efficient to set up
'sm_actual_s' and type check it all in one fell swoop, hence we delay most of
the work until type checking time.
  Note that constructors, most of the standard procedures and 'Word' calls can
be used in constant expressions and hence in types. The 'sm_actual_s' field
must be set up before such constant calls can be evaluated which is why we set
it up before the type checking phase *)

END M3CActualS.
