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
(**)
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3LReveals;

IMPORT TextRefTbl;
IMPORT M3Context, M3CId, M3Error, M3CUnit, ASTWalk;
IMPORT AST, M3AST_AS, M3AST_SM; 

IMPORT M3AST_AS_F, M3AST_SM_F;

IMPORT SeqM3AST_AS_TYPE_SPEC;
IMPORT SeqM3AST_SM_Opaque_type_Revln;
IMPORT M3CTypeRelation, M3CTypesMisc;
IMPORT M3CBackEnd_C; (* for representation of brands *)

TYPE
  CClosure = M3Context.Closure OBJECT
    result := 0;
    table: TextRefTbl.T;
  OVERRIDES
    callback := CheckRevealsInUnit;
  END;

  WClosure = ASTWalk.Closure OBJECT
    unit: M3AST_AS.UNIT;
    ccl: CClosure;
  OVERRIDES
    callback := CheckBrands;
  END;

PROCEDURE Check(c: M3Context.T): INTEGER RAISES {}=
  VAR cl := NEW(CClosure, table := NEW(TextRefTbl.Default).init(50));
  BEGIN
    <*FATAL ANY*> BEGIN
      M3Context.Apply(c, cl);
    END;
    RETURN cl.result;
  END Check;

PROCEDURE CheckRevealsInUnit(cl: CClosure; <*UNUSED*> ut: M3CUnit.Type; 
    <*UNUSED*> name: TEXT; cu: M3AST_AS.Compilation_Unit) RAISES {}=
  VAR
    unit: M3AST_AS.UNIT_WITH_BODY := cu.as_root;
    iter := SeqM3AST_SM_Opaque_type_Revln.NewIter(unit.sm_reveal_s);
    otr: M3AST_SM.Opaque_type_Revln;
    ot: M3AST_AS.Opaque_type;
  BEGIN
    WHILE SeqM3AST_SM_Opaque_type_Revln.Next(iter, otr) DO
      ot := NARROW(otr.sm_type_id, M3AST_AS.TYPED_ID).sm_type_spec; 
      (* we dont care about partial revelations for types not given
         a concrete revelation. *)
      IF ot.sm_concrete_type_spec # NIL THEN
        VAR 
          iter2 := SeqM3AST_AS_TYPE_SPEC.NewIter(otr.sm_opaque_rev_s);
          ts, rts: M3AST_SM.TYPE_SPEC_UNSET;
        BEGIN
          (* the concrete revelation has to be a subtype of all the
             partial ones. *)
          WHILE SeqM3AST_AS_TYPE_SPEC.Next(iter2, ts) DO
            (* 'ts' may be opaque, and for M3CTypeRelation.SubType
                to work, we must use the rhs of the opaque definition. *)
            rts := M3CTypesMisc.Reveal(ts);
            IF NOT M3CTypeRelation.SubType(ot.sm_concrete_type_spec, rts) THEN
              M3Error.SetCu(cu);
              M3Error.ReportWithId(FindRevelationInUnit(cu, ts), 
                "incompatible partial \'REVEAL\' for opaque type \'%s\'",
                   otr.sm_type_id.lx_symrep);
              cl.result := -1;
            END; (* if *)
          END; (* while *)
        END; (* begin *)
      END; (* if *)
    END; (* while *)

    <*FATAL ANY*> BEGIN
      ASTWalk.VisitNodes(cu, NEW(WClosure, ccl := cl, unit := cu.as_root));
    END;
  END CheckRevealsInUnit;

TYPE FClosure = ASTWalk.Closure OBJECT 
    ts: M3AST_AS.TYPE_SPEC;
    n: M3AST_AS.Subtype_reveal := NIL;
  OVERRIDES
    callback := FindRevelation;
  END;

PROCEDURE FindRevelationInUnit(cu: M3AST_AS.Compilation_Unit;
    ts: M3AST_AS.TYPE_SPEC
    ): M3AST_AS.Subtype_reveal RAISES {}=
  VAR fcl := NEW(FClosure, ts := ts);
  BEGIN
    <*FATAL ANY*> BEGIN
      ASTWalk.VisitNodes(cu, fcl);
    END;
    RETURN fcl.n;
  END FindRevelationInUnit;

PROCEDURE FindRevelation(fcl: FClosure; n: AST.NODE; 
    <*UNUSED*> vm: ASTWalk.VisitMode) RAISES {ASTWalk.Aborted}=
  VAR
    reveal_ts: M3AST_AS.TYPE_SPEC;
  BEGIN
    TYPECASE n OF
    | M3AST_AS.Subtype_reveal(s) =>
        M3CTypesMisc.GetTYPE_SPECFromM3TYPE(
                        s.as_type, reveal_ts);
        IF fcl.ts = reveal_ts THEN
	  fcl.n := s;
	  ASTWalk.Abort();
        END;
    ELSE (* ignore *)
    END;
  END FindRevelation;

PROCEDURE CheckBrands(wcl: WClosure; n: AST.NODE; 
    <*UNUSED*> vm: ASTWalk.VisitMode) RAISES {}=
  VAR id: REFANY;
  BEGIN
    TYPECASE n OF
    | NULL =>
    | M3AST_AS.Brand(b) =>
        IF b.as_exp # NIL THEN
         VAR brand := NARROW(b.sm_brand, M3CBackEnd_C.Text_value).sm_value;
         BEGIN
          IF wcl.ccl.table.put(brand, wcl.unit) THEN
            VAR unit: M3AST_AS.UNIT;
            BEGIN
              EVAL wcl.ccl.table.get(brand, id);
              unit := NARROW(id, M3AST_AS.UNIT);
              wcl.ccl.result := -1;
              M3Error.Report(n, 
                "brand " & brand & " already used in " &
                UnitTypeName(unit) & " \'" & 
                M3CId.ToText(unit.as_id.lx_symrep) & "\'");
            END;
           END;
          END; (* if *)
        END; (* if *)
    ELSE
    END;
  END CheckBrands;

PROCEDURE UnitTypeName(unit: M3AST_AS.UNIT): TEXT RAISES {}=
  BEGIN
    IF ISTYPE(unit, M3AST_AS.Interface) THEN RETURN "interface";
    ELSE RETURN "module";
    END;
  END UnitTypeName;

BEGIN

END M3LReveals.
