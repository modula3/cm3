MODULE M3CBaseTypeSpec;

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

IMPORT M3AST, M3AST_AS, M3AST_SM;

IMPORT M3AST_AS_F, M3AST_SM_F;


IMPORT M3CTypesMisc;


PROCEDURE Set(an: M3AST.NODE) RAISES {}=
  VAR 
    ts: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    TYPECASE an OF
    | M3AST_AS.Subrange_type(subrangeType1) =>
        ts := M3CTypesMisc.CheckedUnpack(
            subrangeType1.as_range.as_exp1.sm_exp_type_spec);
        TYPECASE ts OF
        | M3AST_AS.Integer_type, M3AST_AS.Enumeration_type =>
            (* trivial (includes NIL case) *)
        | M3AST_AS.Subrange_type(subrangeType2) =>
            IF subrangeType2.sm_base_type_spec = NIL THEN
              Set(ts);
            END; (* if *)
            ts := subrangeType2.sm_base_type_spec;
        ELSE
          ts := NIL;
        END;
        subrangeType1.sm_base_type_spec := ts;
    ELSE
    END; (* typecase *)
  END Set;


BEGIN
END M3CBaseTypeSpec.
