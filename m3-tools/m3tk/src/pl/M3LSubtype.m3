(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3LSubtype;

IMPORT AST, M3AST_AS, ASTWalk, M3ASTNext;
IMPORT SeqM3AST_AS_Object_type;
IMPORT M3AST_SM_F, M3AST_PL_F;

PROCEDURE Set(cu: M3AST_AS.Compilation_Unit) RAISES {}=
  <*FATAL ANY*>
  BEGIN
    ASTWalk.VisitNodes(cu,
        NEW(ASTWalk.Closure, callback := SetNode));
  END Set;

PROCEDURE SetNode(
    <*UNUSED*> cl: ASTWalk.Closure;
    n: AST.NODE;
    <*UNUSED*> vm: ASTWalk.VisitMode)
    RAISES {}=
 BEGIN
   TYPECASE n OF
   | M3AST_AS.Object_type(ot) =>
      (* add self to immediate ancestor *)
      VAR me := ot;
        st: M3AST_AS.Object_type;
      BEGIN
        IF SuperType(ot, st) THEN
          SeqM3AST_AS_Object_type.AddFront(st.pl_subtype_s, me);
        END; (* while *)
      END;
   ELSE
   END; (* typecase *)
 END SetNode;

PROCEDURE SuperType(
    object: M3AST_AS.Object_type;
    VAR (* OUT *) superType: M3AST_AS.Object_type)
    : BOOLEAN
    RAISES {}=
  VAR
    ts: M3AST_AS.TYPE_SPEC;
  BEGIN
    IF M3ASTNext.SimpleSuperType(object, ts) AND ts # NIL THEN
      TYPECASE ts OF
      | M3AST_AS.Object_type =>
          superType := ts;
          RETURN TRUE
      | M3AST_AS.Opaque_type(ot) =>
          WITH c = ot.sm_concrete_type_spec DO
            IF c # NIL THEN superType := c; RETURN TRUE; END;
          END;
      ELSE
      END;
    END; (* if *)
    RETURN FALSE;
  END SuperType;
BEGIN

END M3LSubtype.
