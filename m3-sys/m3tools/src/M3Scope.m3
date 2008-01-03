(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE M3Scope;

IMPORT M3ID, M3AST;

TYPE
  ScopeInfo = BRANDED "M3Scope.ScopeInfo" REF RECORD
    ast  : M3AST.T;
    n_scopes : CARDINAL;
    scopes   : REF ARRAY OF ScopeDesc;
    n_defns  : CARDINAL;
    defns    : REF ARRAY OF SymDef;
  END;

  ScopeDesc = RECORD
    loc    : Range;
    defn   : Range;
    parent : CARDINAL;  (* index into scopes, LAST(CARD) for root scope *)
  END;

  Range = RECORD start, stop: CARDINAL; END;
   
  SymDef = RECORD
    sym   : M3ID.T;
    class : Class;
    loc   : M3AST.NodeIndex;
    info  : REFANY;
  END;

PROCEDURE LookUp (ast: M3AST.T;  loc: M3AST.NodeIndex;  sym: M3ID.T;
                  VAR(*OUT*) defn: Defn): BOOLEAN =
  VAR si: ScopeInfo;  sx: CARDINAL;
  BEGIN
    IF (loc < 0) OR (loc >= NUMBER (ast.nodes^)) THEN
      RETURN FALSE;
    END;

    IF (ast.scope_info = NIL) THEN Bind (ast); END;
    si := ast.scope_info;

    sx := FindScope (si, loc);
    WHILE (sx < si.n_scopes) DO
      WITH scope = si.scopes [sx] DO
        FOR i := scope.defn.start TO scope.defn.stop DO
          WITH z = si.defns[i] DO
            IF (z.sym = sym) THEN
              defn.ast   := ast;
              defn.loc   := z.loc;
              defn.class := z.class;
              defn.info  := z.info;
              defn.uid   := i;
              RETURN TRUE;
            END;
          END;
        END;
        sx := scope.parent;
      END; (* WITH *)
    END;

    RETURN FALSE;
  END LookUp;

PROCEDURE FindScope (si: ScopeInfo;  loc: M3AST.NodeIndex): CARDINAL =
  VAR
    lo  : CARDINAL := 0;
    hi  : CARDINAL := si.n_scopes;
    mid : CARDINAL;
  BEGIN
    IF (loc = 0) THEN
      (* skip the outermost scope that includes the imports *)
      IF    hi > 1 THEN  loc := si.scopes[1].loc.start;
      ELSIF hi > 0 THEN  loc := si.scopes[0].loc.start;
      END;
    END;

    (* find the first scope begining after "loc" *)
    WHILE (lo < hi) DO
      mid := (lo + hi) DIV 2;
      IF loc < si.scopes[mid].loc.start
        THEN hi := mid;
        ELSE lo := mid + 1;
      END;
    END;

    (* search backwards until we find a scope containing "loc" *)
    FOR i := MIN (lo, si.n_scopes-1) TO 0 BY -1 DO
      WITH z = si.scopes [i] DO
        IF (z.loc.start <= loc) AND (loc <= z.loc.stop) THEN RETURN i; END;
      END;
    END;

    (* failed, return an out-of-range index *)
    RETURN si.n_scopes;
  END FindScope;

PROCEDURE Define (READONLY defn: Defn;  info: REFANY) =
  VAR si: ScopeInfo;  ast := defn.ast;
  BEGIN
    IF (ast.scope_info = NIL) THEN Bind (ast); END;
    si := ast.scope_info;
    <*ASSERT (0 <= defn.uid) AND (defn.uid < si.n_defns) *>
    si.defns[defn.uid].info := info;
  END Define;

(*----------------------------------------------------- scope construction ---*)

TYPE
  BindState = RECORD
    ast     : M3AST.T;
    si      : ScopeInfo;
    n_temps : CARDINAL;
    temps   : REF ARRAY OF SymDef;
  END;

PROCEDURE Bind (ast: M3AST.T) =
  VAR s: BindState;
  BEGIN
    s.ast         := ast;
    s.si          := NEW (ScopeInfo);
    s.si.ast      := ast;
    s.si.n_scopes := 0;
    s.si.scopes   := NEW (REF ARRAY OF ScopeDesc, 16);
    s.si.n_defns  := 1;
    s.si.defns    := NEW (REF ARRAY OF SymDef, 32);
    s.n_temps     := 0;
    s.temps       := NEW (REF ARRAY OF SymDef, 32);

    BindScope (s, 0, NUMBER (ast.nodes^), LAST (CARDINAL));
    ast.scope_info := s.si;
  END Bind;

PROCEDURE BindScope (VAR s: BindState;  start, stop: M3AST.NodeIndex;
                     parent: CARDINAL) =
  VAR
    self := s.si.n_scopes;
    si   := s.si;
  BEGIN
    (* push the new scope *)
    IF si.n_scopes >= NUMBER (si.scopes^) THEN ExpandScopes (si); END;
    WITH z = si.scopes [self] DO
      z.loc.start  := start;
      z.loc.stop   := stop-1;
      z.defn.start := s.n_temps;
      z.defn.stop  := s.n_temps;
      z.parent     := parent;
    END;
    INC (si.n_scopes);

    BindNodes (s, start, stop, self);

    (* pop the new scope & all of its definitions *)
    WITH z = si.scopes [self], cnt = s.n_temps - z.defn.start DO
      IF (cnt > 0) THEN
        WHILE (si.n_defns+cnt >= NUMBER (si.defns^)) DO ExpandDefns (si); END;
        SUBARRAY (si.defns^, si.n_defns, cnt)
          := SUBARRAY (s.temps^, z.defn.start, cnt);
      END;
      s.n_temps := z.defn.start;
      z.defn.start := si.n_defns;
      z.defn.stop  := si.n_defns + cnt - 1;
      INC (si.n_defns, cnt);
    END;
  END BindScope;

PROCEDURE BindNodes (VAR s: BindState;  start, stop: M3AST.NodeIndex;
                     self: CARDINAL) =
  VAR x:= start;  op: M3AST.OP;
  BEGIN
    WHILE (x < stop) DO
      WITH z = s.ast.nodes[x] DO
        op := z.op;  (* force a range check here so we know the CASE
                        is handling all the legal values *)
        CASE op OF
        | M3AST.OP_Block       => BindScope (s, x+1, x + z.width, self);
        | M3AST.OP_GenericArg  => PushDefn (s, x, z.info, Class.GenericArg);
        | M3AST.OP_Import      => PushDefn (s, x, z.info, Class.Import);
        | M3AST.OP_ImportAs    => PushDefn (s, x, z.info, Class.Import);
        | M3AST.OP_FromImport  => PushDefn (s, x, z.info, Class.Import);
        | M3AST.OP_ConstDecl   => PushDefn (s, x, z.info, Class.Const);
        | M3AST.OP_TypeDecl    => PushDefn (s, x, z.info, Class.Type);
        | M3AST.OP_OpaqueDecl  => PushDefn (s, x, z.info, Class.Type);
        | M3AST.OP_ExceptDecl  => PushDefn (s, x, z.info, Class.Exception);
        | M3AST.OP_VarDefn     => PushDefn (s, x, z.info, Class.Var);
        | M3AST.OP_FormalDefn  => PushDefn (s, x, z.info, Class.Formal);

        | M3AST.OP_ProcDecl    => PushDefn (s, x, z.info, Class.Procedure);
                                  BindScope (s, x+1, x + z.width, self);

        | M3AST.OP_For1, M3AST.OP_ForN, M3AST.OP_TryHandlerVar,
          M3AST.OP_TypeCaseVar, M3AST.OP_With =>
            IF (start = x)
              THEN PushDefn (s, x, z.info, Class.Var);
              ELSE BindScope (s, x, x + z.width, self);
            END;

        | M3AST.OP_Unit, M3AST.OP_Generic, M3AST.OP_GenInstance, M3AST.OP_VarDecl,
          M3AST.OP_Case, M3AST.OP_CaseBranch, M3AST.OP_CaseElse,
          M3AST.OP_If, M3AST.OP_IfClause, M3AST.OP_IfElse, M3AST.OP_Lock,
          M3AST.OP_Loop, M3AST.OP_Repeat, M3AST.OP_TryFinally,
          M3AST.OP_TryExcept, M3AST.OP_TryHandler, M3AST.OP_TryElse,
          M3AST.OP_TypeCase, M3AST.OP_TypeCaseArm, M3AST.OP_TypeCaseElse,
          M3AST.OP_While, M3AST.OP_ProcType, M3AST.OP_Formal, M3AST.OP_StmtList =>
            (* visit the subtrees *)
            BindNodes (s, x + 1, x + z.width, self);

        | M3AST.OP_Empty, M3AST.OP_Export,
          M3AST.OP_Reveal, M3AST.OP_RevealPartial,
          M3AST.OP_Assign, M3AST.OP_Assert, M3AST.OP_CallStmt,
          M3AST.OP_CaseLabel, M3AST.OP_CaseRange, M3AST.OP_Exit,
          M3AST.OP_Eval, M3AST.OP_Raise, M3AST.OP_RaiseValue,
          M3AST.OP_Return, M3AST.OP_ReturnValue, M3AST.OP_Array,
          M3AST.OP_OpenArray, M3AST.OP_Enum, M3AST.OP_EnumDefn,
          M3AST.OP_NamedType, M3AST.OP_Method, M3AST.OP_Override,
          M3AST.OP_Packed, M3AST.OP_Field, M3AST.OP_FieldDefn,
          M3AST.OP_Raises, M3AST.OP_RaisesAny, M3AST.OP_Object,
          M3AST.OP_NoBrand, M3AST.OP_DefaultBrand, M3AST.OP_Record,
          M3AST.OP_Ref, M3AST.OP_Root, M3AST.OP_Set, M3AST.OP_Subrange,
          M3AST.OP_UntracedRef, M3AST.OP_UntracedRoot,
          M3AST.OP_Or .. M3AST.OP_Qualify,          (* expr operators *)
          M3AST.OP_Id ..M3AST.OP_Text,              (* literals *)
          M3AST.OP_Attributes .. M3AST.OP_FatalAny  (* pragmas *)
          =>
            (* skip this node and any subtrees *)

        END; (* CASE *)
        x := x + z.width;
      END;
    END;
  END BindNodes;

PROCEDURE PushDefn (VAR s: BindState;  loc: M3AST.NodeIndex;
                    sym: M3ID.T;  class: Class) =
  BEGIN
    IF (s.n_temps >= NUMBER (s.temps^)) THEN ExpandTemps (s); END;
    WITH z = s.temps [s.n_temps] DO
      <*ASSERT sym # M3ID.NoID *>
      z.sym   := sym;
      z.class := class;
      z.loc   := loc;
    END;
    INC (s.n_temps);
  END PushDefn;

PROCEDURE ExpandScopes (si: ScopeInfo) =
  VAR n := NUMBER (si.scopes^);  xx := NEW (REF ARRAY OF ScopeDesc, n+n);
  BEGIN
    SUBARRAY (xx^, 0, n) := si.scopes^;
    si.scopes := xx;
  END ExpandScopes;

PROCEDURE ExpandDefns (si: ScopeInfo) =
  VAR n := NUMBER (si.defns^);  xx := NEW (REF ARRAY OF SymDef, n+n);
  BEGIN
    SUBARRAY (xx^, 0, n) := si.defns^;
    si.defns := xx;
  END ExpandDefns;

PROCEDURE ExpandTemps (VAR s: BindState) =
  VAR n := NUMBER (s.temps^);  xx := NEW (REF ARRAY OF SymDef, n+n);
  BEGIN
    SUBARRAY (xx^, 0, n) := s.temps^;
    s.temps := xx;
  END ExpandTemps;

BEGIN
END M3Scope.
