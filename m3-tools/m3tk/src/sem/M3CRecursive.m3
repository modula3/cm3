MODULE M3CRecursive;

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

IMPORT AST, M3AST_AS, ASTWalk;

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;


IMPORT M3CId, M3CScope_priv, M3Error;

(* Recursion checking. This is done when exiting a scope; note that recursion
(except for recursive revelations) can only happen between identifiers at the
same scope level. *)

TYPE
  State = {NotVisited, BeingVisited, Visited};
  Rec = RECORD
    def: M3CScope_priv.Definitions;
    state: State;
  END;

  Array = REF ARRAY OF Rec;

  Closure =
    ASTWalk.Closure OBJECT
      arr: Array;
    OVERRIDES
      callback := CheckNode;
    END;


PROCEDURE CheckNode(
    cl: Closure;
    n: AST.NODE;
    <*UNUSED*> vm: ASTWalk.VisitMode)
    RAISES {}=
  VAR
    id: M3AST_AS.USED_ID;
  BEGIN
    IF n.IsA_USED_ID(id) THEN
      CheckUsedId(cl.arr, id);
      RETURN;
    END;
    TYPECASE n OF
    | M3AST_AS.Ref_type, M3AST_AS.Procedure_type =>
        ASTWalk.IgnoreChildren(cl);
    | M3AST_AS.Object_type(objectType) =>
        ASTWalk.IgnoreChildren(cl);
        TYPECASE objectType.as_ancestor OF
        | NULL =>
        | M3AST_AS.Named_type(namedType) =>
            CheckUsedId(cl.arr, namedType.as_qual_id.as_id);
        ELSE
          <*FATAL ANY*> BEGIN
            ASTWalk.VisitNodes(objectType.as_ancestor, 
                NEW(Closure, arr := cl.arr));
          END;
        END;
    ELSE
    END;
  END CheckNode;


PROCEDURE CheckUsedId(
    arr: Array;
    usedId: M3AST_AS.USED_ID)
    RAISES {}=
  VAR
    defId := usedId.sm_def;
  BEGIN
    IF defId # NIL THEN
      VAR
        defs := defId.lx_symrep.defs;
      BEGIN
        IF defs # NIL AND defs.scope < 0 THEN
          WITH rec = arr[-defs.scope-1] DO
            IF rec.def.defId = defId THEN
              (* Another id in the same scope *)
              CheckDecl(arr, rec);
            END;
          END;
        END;
      END;
    END;
  END CheckUsedId;


PROCEDURE CheckDecl(arr: Array; VAR r: Rec) RAISES {}=
  BEGIN
    IF r.state = State.NotVisited THEN
      VAR
        start: AST.NODE := NIL;
      BEGIN
        r.state := State.BeingVisited;
        TYPECASE r.def.hook OF
        | M3AST_AS.Var_decl(varDecl) =>
            start := varDecl.as_type;
        | M3AST_AS.Const_decl(constDecl) =>
            start := constDecl.as_exp;
        | M3AST_AS.Exc_decl(excDecl) =>
            start := excDecl.as_type;
        | M3AST_AS.TYPE_DECL(typeDecl) =>
            start := typeDecl.as_type;
        ELSE
        END; (* typecase *)
        TYPECASE start OF
        | NULL =>
        | M3AST_AS.Named_type(namedType) =>
            CheckUsedId(arr, namedType.as_qual_id.as_id);
        | M3AST_AS.Exp_used_id(expUsedId) =>
            CheckUsedId(arr, expUsedId.vUSED_ID);
        | M3AST_AS.Bad_M3TYPE, M3AST_AS.Bad_EXP,
          M3AST_AS.Ref_type, M3AST_AS.Procedure_type =>
            (* No chance of recursion *)
        ELSE
          (* We'll have to do a thorough job; tree walk time *)
          <*FATAL ANY*> BEGIN
            ASTWalk.VisitNodes(start, NEW(Closure, arr := arr));
          END;
        END;
      END;
      r.state := State.Visited;
    ELSIF r.state = State.BeingVisited THEN
      (* Recursion! *)
      VAR
        last: M3AST_AS.DEF_ID;
      BEGIN
        FOR i := 0 TO LAST(arr^) DO
          WITH rec = arr[i] DO
            IF rec.state = State.BeingVisited THEN
              last := rec.def.defId;
              last.tmp_recursive := TRUE;
            END;
          END;
        END;
        M3Error.ReportWithId(last,
            "Illegal recursive declaration of \'%s\'", last.lx_symrep);
      END;
    END;
  END CheckDecl;


PROCEDURE CheckDeclarations(defs: M3CScope_priv.Definitions) RAISES {}=
  VAR
    d := defs;
    count := 0;
    arr: Array;
  BEGIN
    WHILE d # NIL DO IF d.hook # NIL THEN INC(count) END; d := d.next END;
    IF count = 0 THEN RETURN END;
    arr := NEW(Array, count);
    d := defs;
    count := 0;
    WHILE d # NIL DO
      IF d.hook # NIL THEN
        WITH rec = arr[count] DO
          rec.def := d;
          rec.state := State.NotVisited;
        END;
        INC(count);
        d.scope := -count;
      END;
      d := d.next;
    END;
    FOR i := 0 TO LAST(arr^) DO
      CheckDecl(arr, arr[i]);
    END;
  END CheckDeclarations;


BEGIN

END M3CRecursive.
