(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3LMethodTable;

IMPORT ASTWalk;
IMPORT M3Context, M3CUnit, M3ASTNext;
IMPORT AST, M3AST_AS;

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F, M3AST_PL_F;

TYPE
  WClosure = ASTWalk.Closure OBJECT
  OVERRIDES callback := SetNode
  END;

PROCEDURE Set(c: M3Context.T) RAISES {}=
  <*FATAL ANY*>
  BEGIN
    M3Context.Apply(c, NEW(M3Context.Closure, callback := SetUnit), FALSE);  
  END Set;

PROCEDURE SetUnit(
    <*UNUSED*> cl: M3Context.Closure;
    <*UNUSED*> ut: M3CUnit.Type;
    <*UNUSED*> name: TEXT;
    cu: M3AST_AS.Compilation_Unit) RAISES {}=
  <*FATAL ANY*>
  BEGIN
    ASTWalk.VisitNodes(cu, NEW(WClosure));
  END SetUnit;


PROCEDURE SetNode(<*UNUSED*> cl: WClosure;
                             n: AST.NODE;
                  <*UNUSED*> vm: ASTWalk.VisitMode) RAISES {}=
  BEGIN
    TYPECASE n OF
    | M3AST_AS.Object_type(ot) =>
        GenerateMethodTable(ot);
    ELSE
    END; (* typecase *)
  END SetNode;

PROCEDURE MethodTableSize(ot: M3AST_AS.Object_type): CARDINAL RAISES {}=
  VAR
    iter := M3ASTNext.NewIterObjectMethod(ot);
    method: M3AST_AS.METHOD_OVERRIDE;
    is_override: BOOLEAN;
    count: CARDINAL := 0;
  BEGIN
    WHILE M3ASTNext.ObjectMethod(iter, method, is_override) DO
      IF NOT is_override THEN INC(count) END;
    END; (* while *)
    RETURN count;
  END MethodTableSize;

PROCEDURE GenerateMethodTable(ot: M3AST_AS.Object_type) RAISES {}=
  VAR
    mts := MethodTableSize(ot);
    mt := NEW(M3AST_PL_F.MethodTable, mts);
    iter := M3ASTNext.NewIterObjectMethod(ot);
    method: M3AST_AS.METHOD_OVERRIDE;
    is_override: BOOLEAN;
    count: CARDINAL := 0;
    slot: CARDINAL;
  BEGIN
    ot.pl_method_table := mt;
    WHILE M3ASTNext.ObjectMethod(iter, method, is_override) DO
      IF is_override THEN
        WITH o_method = method.as_id.vREDEF_ID.sm_int_def DO
          FOR i := 0 TO mts-1 DO
            IF mt[i].method_id = o_method THEN slot := i; EXIT END;
          END; (* for *)
        END;
      ELSE
        mt[count].method_id := method.as_id;
        slot := count;
        INC(count);
      END;
      (* fill in mt[slot] with the procedure value *)
      mt[slot].proc_id := GetDefault(method.as_id);
    END; (* while *)
  END GenerateMethodTable;

PROCEDURE GetDefault(m: M3AST_AS.METHOD_OVERRIDE_ID): M3AST_AS.Proc_id =
  VAR def_id: M3AST_AS.DEF_ID;
  BEGIN
    (* Find the procedure that is bound to 'm', and add 'ot'
    to the list of object types for which it is a default *)
    IF IsDEF_ID(m.vINIT_ID.sm_init_exp, def_id) THEN
      TYPECASE def_id OF
      | M3AST_AS.Proc_id(p) =>
           RETURN p
      | M3AST_AS.METHOD_OVERRIDE_ID(method_id) =>
          RETURN GetDefault(method_id);
      ELSE
      END; (* typecase *)
    END; (* if *)
    RETURN NIL;
  END GetDefault;

PROCEDURE IsDEF_ID(exp: M3AST_AS.EXP; 
    VAR (*out*) def_id: M3AST_AS.DEF_ID): BOOLEAN RAISES {}=
  BEGIN
    TYPECASE exp OF
    | NULL => RETURN FALSE
    | M3AST_AS.Exp_used_id(e) =>
        def_id := e.vUSED_ID.sm_def;

    | M3AST_AS.Select(b) =>
        def_id := b.as_id.vUSED_ID.sm_def;

    ELSE RETURN FALSE
    END; (* typecase *)
    RETURN def_id # NIL;
  END IsDEF_ID;

BEGIN

END M3LMethodTable.






