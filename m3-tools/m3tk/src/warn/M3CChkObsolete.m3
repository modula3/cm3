(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3CChkObsolete;

IMPORT PropertyV;

IMPORT ASTWalk;
IMPORT AST, M3AST_AS;
IMPORT M3Error, M3CPragma;
IMPORT SeqM3AST_AS_DEF_ID, SeqM3AST_AS_Used_interface_id;
IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F, M3AST_TL_F;


REVEAL
  Handle = ASTWalk.Closure BRANDED OBJECT
  OVERRIDES callback := Node;
  END; (* record *)

PROCEDURE Node(
    <*UNUSED*> h: Handle;
    n: AST.NODE;
    vm: ASTWalk.VisitMode)
    RAISES {}=
  VAR used_id: M3AST_AS.USED_ID;
  BEGIN
    IF vm = ASTWalk.VisitMode.Entry THEN
      IF n.IsA_USED_ID(used_id) THEN
        TYPECASE used_id.sm_def OF
        | NULL => (* not bound *)
        | M3AST_AS.DEF_ID(d) =>
            (* Now we can look at the pre-computed set of
               OBSOLETE DEF_IDs in "d.tmp_unit_id" *)
            IF IsObsolete(d) THEN
              M3Error.WarnWithId(used_id, "%s is OBSOLETE", used_id.lx_symrep);
            END;
        END;
      END;
    END;
  END Node;


PROCEDURE NewHandle(cu: M3AST_AS.Compilation_Unit): Handle RAISES {}=
  BEGIN
    (* Set up information on OBSOLETE declarations for this unit,
       and all imported ones. *)
    TYPECASE cu.as_root OF
    | NULL =>
    | M3AST_AS.UNIT_NORMAL(unit) =>
        SetupDefIds(cu);
        VAR iter := SeqM3AST_AS_Used_interface_id.NewIter(unit.sm_import_s);
            used_id: M3AST_AS.Used_interface_id;
        BEGIN
          WHILE SeqM3AST_AS_Used_interface_id.Next(iter, used_id) DO
            TYPECASE used_id.sm_def OF
            | NULL =>
            | M3AST_AS.UNIT_ID(unit_id)=>
                SetupDefIds(unit_id.sm_spec.sm_comp_unit);
            ELSE
            END;
          END;
        END
    ELSE
    END;
    RETURN NEW(Handle);
  END NewHandle;

PROCEDURE SetupDefIds(cu: M3AST_AS.Compilation_Unit)=
  VAR iter := M3CPragma.NewIter(cu.lx_pragmas);
      pragma: M3CPragma.T;
      args: TEXT;
      follower: M3AST_AS.SRC_NODE;
      bad: BOOLEAN;
  BEGIN
    IF GetSeq(cu) # NIL THEN RETURN END;

    WITH usd = NEW(UniqSeqM3AST_AS_DEF_ID) DO
      PropertyV.Put(cu.tl_pset, usd);
      WHILE M3CPragma.Next(iter, pragma) DO
        IF M3CPragma.Match(pragma, "OBSOLETE", args) THEN
          follower := M3CPragma.FollowingNode(pragma);
          bad := FALSE;
          TYPECASE follower OF
          | NULL => bad := TRUE;
          | M3AST_AS.DECL_REVL(d) =>
              <*FATAL ANY*> BEGIN
                ASTWalk.VisitNodes(d, NEW(SetupClosure, rs := usd));
              END;
          ELSE bad := TRUE
          END;
          IF bad THEN M3Error.ReportAtPos(M3CPragma.Position(pragma),
              "OBSOLETE must precede a declaration or revelation");
          END;
        END;
      END;
    END;
  END SetupDefIds;

TYPE SetupClosure = ASTWalk.Closure OBJECT
    rs: UniqSeqM3AST_AS_DEF_ID;
  OVERRIDES callback := Setup 
  END;

  UniqSeqM3AST_AS_DEF_ID = BRANDED REF SeqM3AST_AS_DEF_ID.T;

PROCEDURE Setup(cl: SetupClosure; n: AST.NODE; <*UNUSED*> vm: ASTWalk.VisitMode)=
  BEGIN
    TYPECASE n OF
    | M3AST_AS.DEF_ID(d) =>
        SeqM3AST_AS_DEF_ID.AddFront(cl.rs^, d);
    ELSE
    END;
  END Setup;

PROCEDURE IsObsolete(d: M3AST_AS.DEF_ID): BOOLEAN=
  BEGIN
    WITH cu = d.tmp_unit_id.sm_spec.sm_comp_unit DO
      VAR iter := SeqM3AST_AS_DEF_ID.NewIter(GetSeq(cu)^);
        od: M3AST_AS.DEF_ID;
      BEGIN
        WHILE SeqM3AST_AS_DEF_ID.Next(iter, od) DO
          IF d = od THEN RETURN TRUE END;
        END;
      END;
    END;
    RETURN FALSE;
  END IsObsolete;

PROCEDURE GetSeq(cu: M3AST_AS.Compilation_Unit): UniqSeqM3AST_AS_DEF_ID=
  BEGIN
    RETURN PropertyV.Get(cu.tl_pset, TYPECODE(UniqSeqM3AST_AS_DEF_ID));
  END GetSeq;

BEGIN

END M3CChkObsolete.
