MODULE M3LOpaque;

(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

IMPORT AST, ASTWalk;
IMPORT M3Context, M3CUnit;
IMPORT M3AST_AS;
IMPORT M3AST_SM_F, M3AST_TM_F;

PROCEDURE SetReveal(c: M3Context.T) RAISES {}=
  <*FATAL ANY*>
  BEGIN
    M3Context.Apply(c, NEW(M3Context.Closure, callback := SetRevealUnit));
  END SetReveal;

PROCEDURE SetRevealUnit(<*UNUSED*> cl: M3Context.Closure;
                                   ut: M3CUnit.Type;
                        <*UNUSED*> name: TEXT; 
    cu: M3AST_AS.Compilation_Unit) RAISES {}=
  <*FATAL ANY*>
  BEGIN
    cu := M3CUnit.ToGenIns(cu, ut);
    IF ut = M3CUnit.Type.Interface THEN
      ASTWalk.VisitNodes(cu, NEW(ASTWalk.Closure,
          callback := SetRevealNode));
    END; (* if *)   
  END SetRevealUnit;

PROCEDURE SetRevealNode(<*UNUSED*> cl: ASTWalk.Closure; 
                        n: AST.NODE;
                        <*UNUSED*> vm: ASTWalk.VisitMode) RAISES {}=
  BEGIN
    TYPECASE n OF
    | M3AST_AS.Opaque_type(opaque_type) =>
        opaque_type.tmp_rev_type_spec := opaque_type.sm_concrete_type_spec
    
    ELSE (* ignore *)
    END; (* typecase *)
  END SetRevealNode;

BEGIN

END M3LOpaque.
