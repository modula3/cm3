MODULE M3CNEWNorm;

(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

IMPORT M3AST, M3AST_LX, M3AST_AS;

IMPORT M3ASTNext, M3CTypesMisc, M3CTypeSpecS;
IMPORT M3CTmpAtt, M3CEncTypeSpec, M3CSpec, M3CInitExp, M3CDef;

IMPORT SeqM3AST_AS_Actual, SeqM3AST_AS_Override; 

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TL_F;

PROCEDURE Set(n: M3AST.NODE; unit_id: M3AST_AS.UNIT_ID) RAISES {}=
  VAR actual: M3AST_AS.Actual;
      ts: M3AST_AS.TYPE_SPEC := NIL;
      ots: M3AST_AS.Object_type := NIL;
  BEGIN
    TYPECASE n OF
    | M3AST_AS.NEWCall(call) =>
        actual := SeqM3AST_AS_Actual.First(call.as_param_s);
        TYPECASE actual.as_exp_type OF
        | M3AST_AS.Object_type(ot) =>
            ts := ot;
        | M3AST_AS.Exp_used_id(exp) =>
            CheckNamedObjectType(exp, ots, ts);
        | M3AST_AS.Select(s) =>
              (* For this to be an object type name, then the
                 LHS must be a reference to an Interface_id.
                 However, at this stage the sm_def attribute
                 of the RHS has not been set up and we need this
                 to decide if it is an object type. So we verify
                 the LHS is an Interface_id and explicitly call
                 M3CDef.ResolveInterfaceId to set sm_def of the RHS. 
              *)
              TYPECASE s.as_exp OF
              | M3AST_AS.Exp_used_id(e1) =>
                  TYPECASE e1.vUSED_ID.sm_def OF
                  | NULL => (* some other error *)
                  | M3AST_AS.Interface_id(intf_id) =>
                      M3CDef.ResolveInterfaceId(intf_id, s.as_id.vUSED_ID);
                          CheckNamedObjectType(s.as_id, ots, ts);
                  ELSE
                  END;
              ELSE
              END;
        ELSE
        END; (* typecase *)
        IF ots # NIL THEN
          CreateOverride(call, ts, ots, unit_id)
        END; (* if *)
    ELSE
    END; (* typecase *)
  END Set;

PROCEDURE CheckNamedObjectType(e: M3AST_AS.Exp_used_id;
    VAR (*out*) ots: M3AST_AS.Object_type;
    VAR (*out*) ts: M3AST_AS.TYPE_SPEC) RAISES {}=
  VAR rts: M3AST_AS.TYPE_SPEC;
  BEGIN
    TYPECASE e.vUSED_ID.sm_def OF
    | M3AST_AS.Type_id(id) =>
        IF id # NIL AND id.sm_type_spec # NIL THEN
           rts := M3CTypesMisc.Reveal(id.sm_type_spec);
           IF ISTYPE(rts, M3AST_AS.Object_type) THEN
             ots := rts;
             ts := id.sm_type_spec;
           END;
        END; (* if *)
     ELSE
     END; (* typecase *)
  END CheckNamedObjectType;


PROCEDURE CreateOverride(
    call: M3AST_AS.NEWCall;
    ts: M3AST_AS.TYPE_SPEC;
    ots: M3AST_AS.Object_type;
    unit_id: M3AST_AS.UNIT_ID)
    RAISES {}=
  VAR
    iter := SeqM3AST_AS_Actual.NewIter(call.as_param_s);
    type, actual, new_actual: M3AST_AS.Actual;
    started := FALSE;
    ov_ot: M3AST_AS.Object_type := NIL;
    ancestor: M3AST_AS.M3TYPE;
    new_param_s := SeqM3AST_AS_Actual.Null;
  BEGIN
    (* 'ts' may equal 'ots', but not if 'ts' was opaque, in which case
       'ots' is the current revelation. It is very important that we create
       a Named_Type with 'ts' rather than 'ots' as the sm_type_spec attribute,
       else type-checking will fail later. 
    *)
    EVAL SeqM3AST_AS_Actual.Next(iter, type);
    WHILE SeqM3AST_AS_Actual.Next(iter, actual) DO
      IF actual.as_id # NIL AND ISTYPE(actual.as_id, M3AST_AS.Exp_used_id) AND
           IsMethod(actual.as_id, ots) AND
           ISTYPE(actual.as_exp_type, M3AST_AS.EXP) THEN
        IF NOT started THEN
          (* create the object type *)
          started := TRUE;
          ov_ot := NEW(M3AST_AS.Object_type).init();
          ov_ot.lx_srcpos := type.lx_srcpos;
          TYPECASE type.as_exp_type OF <*NOWARN*>
          | M3AST_AS.TYPE_SPEC(tts) =>
              ancestor := tts;
          | M3AST_AS.EXP(e) =>
              VAR aa: M3AST_AS.Named_type := NEW(M3AST_AS.Named_type).init();
                q: M3AST_AS.Qual_used_id := NEW(M3AST_AS.Qual_used_id).init();
              BEGIN
                aa.lx_srcpos := ov_ot.lx_srcpos;
                aa.sm_type_spec := ts;
                ancestor := aa;
                aa.as_qual_id := q;
                q.as_id := NEW(M3AST_AS.Used_def_id).init();
                TYPECASE e OF <*NOWARN*>
                | M3AST_AS.Exp_used_id(u) =>
                    q.as_id.lx_srcpos := u.vUSED_ID.lx_srcpos;
                    q.as_id.lx_symrep := u.vUSED_ID.lx_symrep;
                    q.as_id.sm_def := u.vUSED_ID.sm_def;
                | M3AST_AS.Select(b) =>
                    q.as_intf_id := NEW(M3AST_AS.Used_interface_id).init();
                    WITH e1 = NARROW(b.as_exp, M3AST_AS.Exp_used_id).vUSED_ID DO
                      q.as_intf_id.lx_srcpos := e1.lx_srcpos;
                      q.as_intf_id.lx_symrep := e1.lx_symrep;
                      q.as_intf_id.sm_def := e1.sm_def;
                    END;
                    WITH e2 = b.as_id.vUSED_ID DO
                      q.as_id.lx_srcpos := e2.lx_srcpos;
                      q.as_id.lx_symrep := e2.lx_symrep;
                      q.as_id.sm_def := e2.sm_def;
                    END;
                END; (* typecase *)
              END; (* begin *)
          END; (* typecase *)
          ov_ot.as_ancestor := ancestor;
          new_actual := NEW(M3AST_AS.Actual).init();
          SeqM3AST_AS_Actual.AddFront(new_param_s, new_actual);
          new_actual.as_exp_type := ov_ot;
        END; (* if *)
        VAR
          override: M3AST_AS.Override := NEW(M3AST_AS.Override).init();
          override_id: M3AST_AS.Override_id :=
              NEW(M3AST_AS.Override_id).init();
        BEGIN
          override_id.lx_srcpos := actual.as_id.lx_srcpos;
          override_id.lx_symrep := NARROW(actual.as_id,
              M3AST_AS.Exp_used_id).vUSED_ID.lx_symrep;
          override.as_id := override_id;
          override.as_default := actual.as_exp_type;
          SeqM3AST_AS_Override.AddRear(ov_ot.as_override_s, override);
        END;
      ELSE
        new_actual := actual;
        SeqM3AST_AS_Actual.AddRear(new_param_s, new_actual);
      END; (* if *)
    END; (* while *)

    IF started THEN
      (* set temporary/semantic attributes on the new object type *)
      M3CTmpAtt.Set(ov_ot, unit_id);
      M3CEncTypeSpec.Set(ov_ot);
      M3CTypeSpecS.Set(ov_ot, unit_id.sm_spec);
      (* set the semantic attributes on the object overrides *)
      VAR iter := SeqM3AST_AS_Override.NewIter(ov_ot.as_override_s);
        override: M3AST_AS.Override;
      BEGIN
        WHILE SeqM3AST_AS_Override.Next(iter, override) DO
          M3CSpec.Set(override);
          M3CInitExp.Set(override);
          M3CTmpAtt.Set(override.as_id, unit_id);
        END; (* while *)
      END;
      call.sm_norm_actual_s := new_param_s;
    ELSE
      call.sm_norm_actual_s := call.as_param_s;
    END;
  END CreateOverride;

PROCEDURE IsMethod(
    e: M3AST_AS.Exp_used_id;
    ts: M3AST_AS.Object_type)
    : BOOLEAN
    RAISES {}=
  VAR
    iter := M3ASTNext.NewIterFieldOrMethod(ts);
    f: M3AST_AS.Field_id;
    m: M3AST_AS.Method;
    symrep: M3AST_LX.Symbol_rep;
  BEGIN
    (* search back through the type hierarchy, until we
       find a matching symbol. Return TRUE iff it is a method. *)
    WHILE M3ASTNext.FieldOrMethod(iter, f, m, symrep) DO
      IF symrep = e.vUSED_ID.lx_symrep THEN
        RETURN m # NIL;
      END; (* if *)
    END; (* while *)
    RETURN FALSE; (* mismatch; error elsewhere *)
  END IsMethod;


BEGIN
END M3CNEWNorm.
