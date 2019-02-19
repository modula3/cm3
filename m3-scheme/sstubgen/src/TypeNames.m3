(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: TypeNames.m3                                          *)
(* Last Modified On Mon Feb 28 17:00:15 PST 1994 by wobber     *)
(*      Modified On Tue Feb 16 22:26:58 PST 1993 by owicki     *)

MODULE TypeNames;

IMPORT AST, M3AST_AS, M3AST_AS_F, M3AST_TL_F, 
       ASTWalk, M3Context, M3CUnit, PropertyV;

TYPE ContextClosure = M3Context.Closure OBJECT
  OVERRIDES callback := VisitUnit;
  END;

PROCEDURE Preprocess(c: M3Context.T) =
    <* FATAL ANY *>
  BEGIN
    M3Context.Apply(c, NEW(ContextClosure),
                    findStandard := FALSE); (* ignore 'standard' unit *)
  END Preprocess;

PROCEDURE VisitUnit(
    <*UNUSED*> cl: ContextClosure;
    ut: M3CUnit.Type;
    <*UNUSED*> name: TEXT;
    cu: M3AST_AS.Compilation_Unit)
    RAISES {}=
    <* FATAL ANY *>
  BEGIN
    (* if it is a generic instantiation, get to actual instantiated tree *)
    cu := M3CUnit.ToGenIns(cu, ut); 
    ASTWalk.VisitNodes(cu, NEW(ASTWalk.Closure, callback := Node));
  END VisitUnit;

PROCEDURE Node(
    <*UNUSED*> cl: ASTWalk.Closure;
    n: AST.NODE; vm: ASTWalk.VisitMode) =
  BEGIN
    IF vm = ASTWalk.VisitMode.Entry THEN
      TYPECASE n OF
      | M3AST_AS.TYPE_DECL(type_decl) =>
          TYPECASE type_decl.as_type OF
          | M3AST_AS.TYPE_SPEC(ts) =>
              PropertyV.Put(ts.tl_pset, type_decl.as_id.lx_symrep); 
          ELSE
          END;
      ELSE
      END;
    END;
  END Node;

BEGIN

END TypeNames.
