MODULE M3CSM;

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

IMPORT AST, M3AST_AS;

IMPORT M3AST_AS_F;


IMPORT ASTWalk;

IMPORT M3CImportS;
IMPORT M3CTmpAtt;
IMPORT M3CSpec;
IMPORT M3CExternal;
IMPORT M3CNormType;
IMPORT M3CInitExp;
IMPORT M3CTypeSpecS;
IMPORT M3CEncTypeSpec;
IMPORT M3CDef;
IMPORT M3CIntDef;
IMPORT M3CTypeSpec;
IMPORT M3CConcTypeSpec;
IMPORT M3CBaseTypeSpec;
IMPORT M3CExpValue;
IMPORT M3CBitSize;
IMPORT M3CActualS;
IMPORT M3CTypeCheck;
IMPORT M3CBrand;
IMPORT M3CSundries;
IMPORT M3CNEWNorm;


PROCEDURE ComputeAttributeNoClosure(
    an: AST.NODE;
    p: ASTWalk.NodeCallbackProc)
    RAISES {}=
  <*FATAL ANY*>
  BEGIN
    ASTWalk.VisitNodes(an, ASTWalk.NodeProcClosure(p));
  END ComputeAttributeNoClosure;


TYPE
  InitialPassClosure =
    ASTWalk.Closure OBJECT
      cu: M3AST_AS.Compilation_Unit;
    OVERRIDES
      callback := InitialPass;
    END;


PROCEDURE InitialPass(
    cl: InitialPassClosure;
    n: AST.NODE;
    <*UNUSED*> vm: ASTWalk.VisitMode)
    RAISES {}=
  BEGIN
    (* As all the procedures below are called in parallel i.e. in one walk
     over the tree, none of them can depend on any other semantic attributes
     being set. *)
    M3CImportS.Set(n);
    M3CTmpAtt.Set(n, cl.cu.as_root.as_id);
    M3CSpec.Set(n);
    M3CExternal.Set(n, cl.cu);
    M3CNormType.Set(n);
    M3CInitExp.Set(n);
    M3CTypeSpecS.Set(n, cl.cu.as_root);
    M3CEncTypeSpec.Set(n);
  END InitialPass;


TYPE
  BundledPasses1Closure =
    ASTWalk.Closure OBJECT
      unit: M3AST_AS.UNIT;
    OVERRIDES
      callback := BundledPasses1;
    END;


PROCEDURE BundledPasses1(
    cl: BundledPasses1Closure;
    an: AST.NODE;
    <*UNUSED*> vm: ASTWalk.VisitMode)
    RAISES {}=
  BEGIN
    M3CIntDef.Set(an, cl.unit);
    M3CTypeSpec.SetPass1(an);
  END BundledPasses1;


PROCEDURE BundledPasses2(
    cl: ASTWalk.Closure;
    an: AST.NODE;
    vm: ASTWalk.VisitMode)
    RAISES {}=
  BEGIN
    M3CBaseTypeSpec.Set(an);
    M3CActualS.Set(cl, an, vm);
  END BundledPasses2;


TYPE
  BundledPasses3Closure =
    ASTWalk.Closure OBJECT
      brandHandle: M3CBrand.Handle;
      typeCheckHandle: M3CTypeCheck.Handle;
      sundriesHandle: M3CSundries.Handle;
    OVERRIDES
      callback := BundledPasses3;
    END;


PROCEDURE BundledPasses3(
    cl: BundledPasses3Closure;
    an: AST.NODE;
    mode: ASTWalk.VisitMode)
    RAISES {}=
  BEGIN
    M3CBrand.Set(cl.brandHandle, an, mode);
    M3CTypeCheck.Node(cl.typeCheckHandle, an, mode);
    M3CSundries.Check(cl.sundriesHandle, an, mode);
  END BundledPasses3;

TYPE
  NEWNormPassClosure = ASTWalk.Closure OBJECT
    unit: M3AST_AS.UNIT;
  OVERRIDES
    callback := NEWNormPass
  END;

PROCEDURE NEWNormPass(
    cl: NEWNormPassClosure;
    an: AST.NODE;
    <*UNUSED*> mode: ASTWalk.VisitMode) RAISES {}=
  BEGIN
    M3CNEWNorm.Set(an, cl.unit.as_id);
  END NEWNormPass;

PROCEDURE Check(cu: M3AST_AS.Compilation_Unit) RAISES {}=
  <*FATAL ANY*>
  VAR
    unit: M3AST_AS.UNIT_NORMAL := cu.as_root;
    interface := ISTYPE(unit, M3AST_AS.Interface);
  BEGIN
    (* Initial pass - sets many attributes which do not depend on others being
     set *)
    ASTWalk.VisitNodes(cu, NEW(InitialPassClosure, cu := cu).init());

    (* First bash at resolving names *)
    ASTWalk.ModeVisitNodes(
        cu, NEW(ASTWalk.Closure, callback := M3CDef.SetPass1).init(),
        ASTWalk.OnEntryAndExit);

    (* Set alternative definitions for multiply defined items, defaults and
     start on setting type attributes *)
    ASTWalk.VisitNodes(cu, NEW(BundledPasses1Closure, unit := unit).init());

    (* revelations *)
    M3CConcTypeSpec.Set(cu);
    M3CConcTypeSpec.SetCurrentReveal(cu, ASTWalk.VisitMode.Entry);
    (* desugar NEW(ObjectType, method := E) calls *)
    ASTWalk.VisitNodes(cu, NEW(NEWNormPassClosure, unit := unit).init());
    (* Complete setting of type attributes *)
    ASTWalk.ModeVisitNodes(
        cu, M3CTypeSpec.NewSetPass2Closure(unit), ASTWalk.OnExit);

    (* Set base type for subranges and sm_actuals lists for calls and
     constructors *)
    ASTWalk.VisitNodes(cu,
        NEW(ASTWalk.Closure, callback := BundledPasses2).init());

    (* Evaluate constant expressions, do constant folding and evaluate type
     sizes *) 
    ASTWalk.ModeVisitNodes(
        cu, M3CExpValue.NewClosure(interface), ASTWalk.OnEntryAndExit);
    ComputeAttributeNoClosure(cu, M3CBitSize.Set);

    (* Finally do type checking and sundry other checks *)
    VAR
      bp3c := NEW(BundledPasses3Closure,
          brandHandle := M3CBrand.NewHandle(unit),
          typeCheckHandle := M3CTypeCheck.NewHandle(unit.as_unsafe = NIL, NIL),
          sundriesHandle :=
              M3CSundries.NewHandle(NOT interface, FALSE, FALSE, FALSE));
    BEGIN
      ASTWalk.ModeVisitNodes(cu, bp3c, ASTWalk.OnEntryAndExit);
    END;
  END Check;

PROCEDURE FinishUp(cu: M3AST_AS.Compilation_Unit) RAISES {}=
  BEGIN
    M3CConcTypeSpec.SetCurrentReveal(cu, ASTWalk.VisitMode.Exit);
  END FinishUp;

BEGIN
END M3CSM.
