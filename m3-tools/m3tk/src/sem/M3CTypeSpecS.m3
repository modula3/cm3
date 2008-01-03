MODULE M3CTypeSpecS;

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

(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

IMPORT M3AST, M3AST_AS, M3Context;

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;

IMPORT SeqM3AST_AS_TYPE_SPEC;

PROCEDURE Set(an: M3AST.NODE; unit: M3AST_AS.UNIT_WITH_BODY) RAISES {}=
  BEGIN
    IF NOT ISTYPE(an, M3AST_AS.TYPE_SPEC) THEN RETURN END;

    TYPECASE an OF
    | M3AST_AS.Ref_type(rt) => (* no untraced ref types *)
        IF rt.as_trace_mode # NIL THEN RETURN END;

    | M3AST_AS.Object_type, M3AST_AS.Opaque_type => 
        (* Ok *)

    ELSE
      RETURN
    END; (* case *)
    SeqM3AST_AS_TYPE_SPEC.AddRear(unit.sm_type_spec_s, an)
  END Set;

PROCEDURE TCTag(ts: M3AST_AS.TYPE_SPEC): INTEGER  RAISES {}=
  BEGIN
    VAR
      unit_id := TCUnit_id(ts);
      iter := SeqM3AST_AS_TYPE_SPEC.NewIter(
        NARROW(unit_id.sm_spec, M3AST_AS.UNIT_WITH_BODY).sm_type_spec_s);
      tag := 1;
      tts: M3AST_AS.TYPE_SPEC;
    BEGIN
      WHILE SeqM3AST_AS_TYPE_SPEC.Next(iter, tts) DO
        TYPECASE ts OF
        | M3AST_AS.RefAny_type, M3AST_AS.Root_type, M3AST_AS.Null_type =>
            IF TYPECODE(tts) = TYPECODE(ts) THEN RETURN tag END;
        ELSE
          IF ts = tts THEN RETURN tag END;
        END;
        INC(tag);
      END; (* while *)
    END;
    RETURN 0; (* bound to cause an error *)
  END TCTag;

PROCEDURE TCUnit_id(ts: M3AST_AS.TYPE_SPEC): M3AST_AS.UNIT_ID RAISES {}=
  BEGIN
    TYPECASE ts OF
    | M3AST_AS.RefAny_type, M3AST_AS.Root_type, M3AST_AS.Null_type =>
        RETURN M3Context.Standard().as_root.as_id;
    ELSE RETURN ts.tmp_unit_id.sm_spec.as_id;
    END;    
  END TCUnit_id;


BEGIN
END M3CTypeSpecS.
