INTERFACE M3CBackEnd_C;

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
IMPORT M3CBackEnd_Float_Real, M3CBackEnd_Float_LongReal,
    M3CBackEnd_Float_Extended;

(* This interface provides a 'friends' interface to the implementation
of M3CBackEnd for the C-code generator. *)

PROCEDURE RegisterProcs() RAISES {};
(* Register implementations of the M3CBackEnd.XXX procedures for
   the C back-end.
*)

CONST  (* values possibly of interest to others *) 
  Separator = "__";            (* Replaces '.' in C version of M3 QualId *)

(* We reveal here the representation used for Exp_value's *)

REVEAL M3AST_SM.Exp_value = BRANDED OBJECT END;

TYPE
  Set_constructor_value = M3AST_SM.Exp_value BRANDED OBJECT
    sm_low: INTEGER;
    sm_value: REF ARRAY OF INTEGER;
  END;
  Array_or_record_constructor_value = M3AST_SM.Exp_value BRANDED OBJECT
    sm_constructor: M3AST_AS.Constructor;
  END;

  (* Note - Integer_value is used for all ordinals, including enumerations 
  and the CHAR enumeration *)

  Integer_value = M3AST_SM.Exp_value BRANDED OBJECT
    sm_value: INTEGER;
  END;

  Proc_value = M3AST_SM.Exp_value BRANDED OBJECT
    sm_value: TEXT;  (* "I.P" *)
  END;
  Text_value = M3AST_SM.Exp_value BRANDED OBJECT
    sm_value: TEXT;
  END;

  (* Here we use Modula-3 REAL/LONGREAL/EXTENDED values, which are computed
  on the host machine.  This allows all standard operations to be
  computed, but possibly incorrectly if we are cross-compiling to
  another target. *)

  Real_value = M3CBackEnd_Float_Real.T;
  LongReal_value = M3CBackEnd_Float_LongReal.T;
  Extended_value = M3CBackEnd_Float_Extended.T;

PROCEDURE NewInteger_value(i: INTEGER): Integer_value RAISES {};
(* use this to create 'Integer_value' types *)

END M3CBackEnd_C.
