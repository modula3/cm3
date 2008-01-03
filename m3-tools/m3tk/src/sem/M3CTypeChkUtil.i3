INTERFACE M3CTypeChkUtil;

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

IMPORT M3AST_AS, M3AST_SM;
IMPORT M3CExpsMisc;


PROCEDURE IsBoolean(type: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {};
PROCEDURE IsSubTypeOfInteger(
    type: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {};
PROCEDURE IsSubTypeOfBoolean(
    type: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {};
PROCEDURE IsSubTypeOfCardinal(
    type: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {};
PROCEDURE IsSubTypeOfText(type: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {};
PROCEDURE IsSubTypeOfRefany(
    type: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {};
PROCEDURE IsSubTypeOfAddress(
    type: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {};
(* check if the given type is or is a subtype of the standard type given by
the name of the procedure. Optimistic - if 'type' is unset they return TRUE *)

PROCEDURE IsTopLevel(id: M3AST_AS.Proc_id): BOOLEAN RAISES {};
(* checks if the given procedure is top level *)

TYPE
  Proc = {TopLevel, Nested, Method, Standard, Variable};
  ProcSet = SET OF Proc;

PROCEDURE ClassifyProc(exp: M3AST_AS.EXP): Proc RAISES {};
(* classifies an expression which evaluates to a procedure:
  'TopLevel'     expression is a top level procedure constant
  'Nested'       expression is a nested procedure constant
  'Method'       expression is an object method being used as a procedure
                 i.e. ObjectType.method
  'Standard'     expression is one of the predefined "standard" procedures
  'Variable'     not a constant procedure - result of expression not known
                 at compile time.
The default is 'VariableProc' i.e. if 'exp' is not one of the other
alternatives it will be assumed to be 'VariableProc' even if it is, for
example, an integer expression *)

PROCEDURE EXPAssignable(
    type: M3AST_SM.TYPE_SPEC_UNSET;
    exp: M3AST_AS.EXP;
    safe: BOOLEAN)
    : BOOLEAN
    RAISES {};
(* is 'exp' assignable to 'type'? Uses the 'Assignable' relation but also does
two additional checks:
1) if 'type' is a subrange type and 'exp' is a constant, checks that 'exp' is
in bounds.
2) if 'type' is a procedure type and 'exp' is procedure constant, checks that
'exp' is top level.
These two checks cover 'expression assignability' (as the report calls it)
insofar as it can be checked at compile time. 'safe' indicates whether or not
the check is being done in a safe module/interface. *)

PROCEDURE IsExpectedClass(
    exp: M3AST_AS.EXP;
    classes: M3CExpsMisc.ClassSet)
    : BOOLEAN
    RAISES {};
(* checks if 'exp' is of one of the given classes. Fails and gives an
appropriate message if it is not *)

PROCEDURE IsNormalEXP(exp: M3AST_AS.EXP): BOOLEAN RAISES {};
(* Just 'IsExpectedClass' called with a set only containing 'Normal' *)

END M3CTypeChkUtil.
