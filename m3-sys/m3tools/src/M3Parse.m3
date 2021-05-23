(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE M3Parse EXPORTS M3AST;

IMPORT Target, TInt, MxConfig;
IMPORT M3ID, Text;
IMPORT M3Lexer;

FROM M3Scanner IMPORT TK_Comment, TK_EOF, TK_Error,
  (* literals *)
  TK_Ident, TK_Card_const, TK_Real_const, TK_Longreal_const,
  TK_Extended_const, TK_Char_const, TK_Text_const,

  (* operators *)
  TK_Plus, TK_Minus, TK_Asterisk, TK_Slash, TK_Assign, TK_Ampersand,
  TK_Dot, TK_Comma, TK_Semi, TK_L_paren, TK_L_bracket, TK_L_brace,
  TK_Arrow, TK_Equal, TK_Sharp, TK_Less, TK_Greater, TK_Ls_equal,
  TK_Gr_equal, TK_Dot_dot, TK_Colon, TK_R_paren, TK_R_bracket,
  TK_R_brace, TK_Bar, TK_Subtype, TK_Implies,
  TK_End_pragma,

  (* reserved words *)
  TK_And, TK_Any, TK_Array, TK_As, TK_Begin, TK_Bits, TK_Branded,
  TK_By, TK_Case, TK_Const, TK_Div, TK_Do, TK_Else, TK_Elsif, TK_End,
  TK_Eval, TK_Except, TK_Exception, TK_Exit, TK_Exports, TK_Finally,
  TK_For, TK_From, TK_Generic, TK_If, TK_Import, TK_In, TK_Interface,
  TK_Lock, TK_Loop, TK_Methods, TK_Mod, TK_Module, TK_Not, TK_Object,
  TK_Of, TK_Or, TK_Overrides, TK_Procedure, TK_Raise, TK_Raises,
  TK_Readonly, TK_Record, TK_Ref, TK_Repeat, TK_Return, TK_Reveal,
  TK_Set, TK_Then, TK_To, TK_Try, TK_Type, TK_Typecase, TK_Unsafe,
  TK_Until, TK_Untraced, TK_Value, TK_Var, TK_While, TK_With;

FROM M3Lexer IMPORT TK,
  (* pragmas *)
  TK_Inline, TK_External, TK_Assert, TK_Unused,
  TK_Obsolete, <*NOWARN*>TK_Trace, TK_CallConv, TK_Fatal;

TYPE
  TKSet = SET OF TK;

TYPE
  State = RECORD
    scan     : M3Lexer.T;
    err      : ErrorHandler;
    ast      : T;
    head     : Chunk;
    tail     : Chunk;
    n_ops    : CARDINAL; (* next available node index *)
    base     : CARDINAL; (* first node index in tail *)
    cur      : CARDINAL; (* next available node slot in tail *)
    tok      : TK;
    n_texts  : INTEGER;
    n_ints   : INTEGER;
    n_floats : INTEGER;
  END;

TYPE
  Chunk = REF RECORD
    next  : Chunk := NIL;
    nodes : ARRAY [0..999] OF Node;
  END;

EXCEPTION
  Error;  (* => early bail out requested by client *)

PROCEDURE Parse (scan: M3Lexer.T;  err: ErrorHandler): T =
  VAR s: State;
  BEGIN
    s.scan     := scan;
    s.err      := err;
    s.ast      := NEW (T);
    s.head     := NEW (Chunk);
    s.tail     := s.head;
    s.n_ops    := 0;
    s.base     := 0;
    s.cur      := 0;
    s.n_texts  := 0;
    s.n_ints   := 0;
    s.n_floats := 0;

    s.ast.nodes     := NIL;
    s.ast.safe      := TRUE;
    s.ast.interface := FALSE;
    s.ast.texts     := NIL;
    s.ast.ints      := NIL;
    s.ast.floats    := NIL;

    TRY
      InitTarget (s);
      GetToken (s);
      Unit (s);
    EXCEPT Error =>
      (* early bail out... *)
    END;
    s.ast.nodes := FlattenChunks (s);

    (* make sure the collector has a chance... *)
    s.head := NIL;
    s.tail := NIL;
    s.scan := NIL;

    RETURN s.ast;
  END Parse;

PROCEDURE InitTarget (VAR s: State) RAISES {Error} =
  VAR sys: TEXT;
  BEGIN
    IF Target.System_name = NIL THEN
      sys := MxConfig.Get ("TARGET");
      IF (sys = NIL) THEN
        Err (s, "unknown target architecture");
      ELSIF NOT Target.Init (sys) THEN
        Err (s, "unsupported target architecture: ", sys);
      END;
    END;
  END InitTarget;

(*----------------------------------------------------- compilation units ---*)

PROCEDURE Unit (VAR s: State) RAISES {Error} =
  VAR id1, id2: M3ID.T;  z: CARDINAL;  id: M3ID.T;
  BEGIN
    IF (s.tok = TK_External) THEN
      s.ast.external := TRUE;
      ExternalPragma (s, id, s.ast.module_cc);
      IF (id # M3ID.NoID) THEN
        Err (s, "<*EXTERNAL*> module name ignored: ", M3ID.ToText (id));
      END;
    END;

    IF (s.tok = TK_Generic) THEN
      GetToken (s);  (* GENERIC *)
      UnitKind (s);  (* INTERFACE / MODULE *)

      id1 := MatchID (s);
      z := AddOp (s, OP_Generic, id1);
        GenericArgs (s);
        Match (s, TK_Semi);
        UnitBody (s);
      FixWidth (s, z);
    ELSE
      IF (s.tok = TK_Unsafe) THEN
        s.ast.safe := FALSE;
        GetToken (s);
      END;

      UnitKind (s);  (* INTERFACE / MODULE *)
      id1 := MatchID (s);
      z := AddOp (s, OP_Unit, id1);
        IF NOT s.ast.interface THEN Exports (s); END;
        IF (s.tok = TK_Semi) THEN
          GetToken (s);  (* ; *)
          UnitBody (s);
        ELSIF (s.tok = TK_Equal) THEN
          GetToken (s);  (* = *)
          FixOp (s, z, OP_GenInstance);
          EVAL AddOp (s, OP_Id, MatchID (s));
          GenericArgs (s);
          Match (s, TK_End);
        ELSE
          Err (s, "expected ';' or '=', found ", TokName (s));
        END;
      FixWidth (s, z);
    END;

    id2 := MatchID (s);
    IF (id1 # id2) THEN
      Err (s, "initial unit name \"", M3ID.ToText (id1),
           "\" doesn't match final name \"", M3ID.ToText (id2) & "\"");
    END;
    Match (s, TK_Dot);
    Match (s, TK_EOF);
  END Unit;

PROCEDURE UnitKind (VAR s: State) RAISES {Error} =
  BEGIN
    IF (s.tok = TK_Interface) THEN
      s.ast.interface := TRUE;
      GetToken (s);  (* INTERFACE *)
    ELSIF (s.tok = TK_Module) THEN
      s.ast.interface := FALSE;
      GetToken (s);  (* MODULE *)
    ELSE
      Err (s, "expected INTERFACE or MODULE keyword, found ", TokName (s));
    END;
  END UnitKind;

PROCEDURE UnitBody (VAR s: State) RAISES {Error} =
  VAR z: CARDINAL;
  BEGIN
    Imports (s);
    IF s.ast.interface THEN
      z := AddOp (s, OP_Block);
      Decls (s);
      FixWidth (s, z);
      Match (s, TK_End);
    ELSE
      Block (s);
    END;
  END UnitBody;

PROCEDURE GenericArgs (VAR s: State) RAISES {Error} =
  BEGIN
    Match (s, TK_L_paren);
    WHILE (s.tok = TK_Ident) DO
      EVAL AddOp (s, OP_GenericArg, MatchID (s));
      IF (s.tok # TK_Comma) THEN EXIT; END;
      GetToken (s); (* , *)
    END;
    Match (s, TK_R_paren);
  END GenericArgs;

PROCEDURE Exports (VAR s: State) RAISES {Error} =
  BEGIN
    IF (s.tok = TK_Exports) THEN
      GetToken (s);  (* EXPORTS *)
      EVAL AddOp (s, OP_Export, MatchID (s));
      WHILE (s.tok = TK_Comma) DO
        GetToken (s); (* , *)
        EVAL AddOp (s, OP_Export, MatchID (s));
      END;
    END;
  END Exports;

PROCEDURE Imports (VAR s: State) RAISES {Error} =
  VAR id, id2: M3ID.T;  z: CARDINAL;
  BEGIN
    LOOP
      IF (s.tok = TK_Import) THEN
        GetToken (s); (* IMPORT *)
        WHILE (s.tok = TK_Ident) DO
          id := MatchID (s);
          IF (s.tok = TK_As) THEN
            GetToken (s); (* AS *)
            id2 := MatchID (s);
            z := AddOp (s, OP_ImportAs, id2);
              EVAL AddOp (s, OP_Id, id);
            FixWidth (s, z);
          ELSE
            EVAL AddOp (s, OP_Import, id);
          END;
          IF (s.tok # TK_Comma) THEN EXIT; END;
          GetToken (s); (* , *)
        END;
        Match (s, TK_Semi);
      ELSIF (s.tok = TK_From) THEN
        GetToken (s); (* FROM *)
        id := MatchID (s);
        Match (s, TK_Import);
        WHILE (s.tok = TK_Ident) DO
          id2 := MatchID (s);
          z := AddOp (s, OP_FromImport, id2);
            EVAL AddOp (s, OP_Id, id);
          FixWidth (s, z);
          IF (s.tok # TK_Comma) THEN EXIT; END;
          GetToken (s); (* , *)
        END;
        Match (s, TK_Semi);
      ELSE
        EXIT;
      END;
    END;
  END Imports;

PROCEDURE Block (VAR s: State) RAISES {Error} =
  VAR z := AddOp (s, OP_Block);
  BEGIN
    Decls (s);
    Match (s, TK_Begin);
    Stmt (s);
    Match (s, TK_End);
    FixWidth (s, z);
  END Block;

(*---------------------------------------------------------- declarations ---*)

CONST
  DeclStart = TKSet {TK_Const, TK_Type, TK_Reveal, TK_Var,
                     TK_External, TK_Inline, TK_Unused, TK_Obsolete,
                     TK_Exception, TK_Procedure, TK_Fatal, TK_CallConv};

PROCEDURE Decls (VAR s: State) RAISES {Error} =
  VAR att: DeclAttributes;
  BEGIN
    WHILE (s.tok IN DeclStart) DO
      DeclPragmas (s, att);
      CASE s.tok OF
      | TK_Const     =>   ConstDecl (s, att);
      | TK_Type      =>   TypeDecl (s, att);
      | TK_Var       =>   VarDecl (s, att);
      | TK_Procedure =>   ProcDecl (s, att);
      | TK_Reveal    =>   Reveal (s, att);
      | TK_Exception =>   ExceptDecl (s, att);
      | TK_Fatal     =>   FatalPragma (s, att);
      ELSE IF att.gotSome THEN
             Err (s, "declaration pragma not followed by a declaration");
           END;
      END;
    END;
  END Decls;

PROCEDURE ConstDecl (VAR s: State;  READONLY att: DeclAttributes) RAISES {Error} =
  VAR id: M3ID.T;  z: CARDINAL;
  BEGIN
    Match (s, TK_Const);
    WHILE (s.tok = TK_Ident) DO
      id := MatchID (s);
      z := AddOp (s, OP_ConstDecl, id);
        IF (s.tok = TK_Colon) THEN
          GetToken (s); (* : *)
          Type (s);
        ELSE
          EVAL AddOp (s, OP_Empty);
        END;
        Match (s, TK_Equal);
        Expr (s);
        GenAttributes (s, att);
      FixWidth (s, z);
      Match (s, TK_Semi);
    END;
  END ConstDecl;

PROCEDURE TypeDecl (VAR s: State;  READONLY att: DeclAttributes) RAISES {Error} =
  VAR id: M3ID.T;  z: CARDINAL;
  BEGIN
    Match (s, TK_Type);
    WHILE (s.tok = TK_Ident) DO
      id := MatchID (s);
      z := AddOp (s, OP_TypeDecl, id);
        IF (s.tok = TK_Equal) THEN
          GetToken (s); (* = *)
        ELSIF (s.tok = TK_Subtype) THEN
          GetToken (s); (* <: *)
          FixOp (s, z, OP_OpaqueDecl);
        ELSE
          Err (s, "expected '=' or '<:', found ", TokName (s));
        END;
        Type (s);
        GenAttributes (s, att);
      FixWidth (s, z);
      Match (s, TK_Semi);
    END;
  END TypeDecl;

PROCEDURE VarDecl (VAR s: State;  READONLY att: DeclAttributes) RAISES {Error} =
  VAR id: M3ID.T;  z: CARDINAL;
  BEGIN
    Match (s, TK_Var);
    WHILE (s.tok = TK_Ident) DO
      z := AddOp (s, OP_VarDecl);
        WHILE (s.tok = TK_Ident) DO
          id := MatchID (s);
          EVAL AddOp (s, OP_VarDefn, id);
          IF (s.tok # TK_Comma) THEN EXIT; END;
          GetToken (s); (* , *)
        END;
        IF (s.tok = TK_Colon) THEN
          GetToken (s); (* : *)
          Type (s);
        ELSE
          EVAL AddOp (s, OP_Empty);
        END;
        IF (s.tok = TK_Assign) THEN
          GetToken (s); (* := *)
          Expr (s);
        ELSE
          EVAL AddOp (s, OP_Empty);
        END;
        GenAttributes (s, att);
      FixWidth (s, z);
      Match (s, TK_Semi);
    END;
  END VarDecl;

PROCEDURE ProcDecl (VAR s: State;  READONLY att: DeclAttributes) RAISES {Error} =
  VAR id: M3ID.T;  z: CARDINAL;
  BEGIN
    Match (s, TK_Procedure);
    id := MatchID (s);
    z := AddOp (s, OP_ProcDecl, id);
      ProcSignature (s, att.callingConv);

      IF (s.ast.interface) THEN
        IF (s.tok = TK_Equal) THEN
          Err (s, "procedure body is not allowed in an interface");
          GetToken (s); (* = *)
          ProcBody (s, id);
        END;
      ELSE (* NOT interface *)
        Match (s, TK_Equal);
        ProcBody (s, id);
      END;
      Match (s, TK_Semi);

      GenAttributes (s, att);
    FixWidth (s, z);
  END ProcDecl;

PROCEDURE ProcBody (VAR s: State;  proc_id: M3ID.T) RAISES {Error} =
  VAR end_id: M3ID.T;
  BEGIN
    Block (s);
    end_id := MatchID (s);
    IF (proc_id # end_id) THEN
       Err (s, "initial procedure name \"", M3ID.ToText (proc_id),
           "\" doesn't match final name \"", M3ID.ToText (end_id) & "\"");
    END;
  END ProcBody;

PROCEDURE Reveal (VAR s: State;  READONLY att: DeclAttributes) RAISES {Error} =
  VAR z: CARDINAL;
  BEGIN
    Match (s, TK_Reveal);
    WHILE (s.tok = TK_Ident) DO
      z := AddOp (s, OP_Reveal);
        QID (s);
        IF (s.tok = TK_Equal) THEN
          GetToken (s); (* = *)
          Type (s);
        ELSIF (s.tok = TK_Subtype) THEN
          FixOp (s, z, OP_RevealPartial);
          GetToken (s); (* <: *)
          Type (s);
        ELSE
          Err (s, "expected '=' or '<:', found ", TokName (s));
        END;
        GenAttributes (s, att);
      FixWidth (s, z);
      Match (s, TK_Semi);
    END;
  END Reveal;

PROCEDURE ExceptDecl (VAR s: State;  READONLY att: DeclAttributes) RAISES {Error} =
  VAR id: M3ID.T;  z: CARDINAL;
  BEGIN
    Match (s, TK_Exception);
    WHILE (s.tok = TK_Ident) DO
      id := MatchID (s);
      z := AddOp (s, OP_ExceptDecl, id);
      IF (s.tok = TK_L_paren) THEN
        GetToken (s);  (* ( *)
        Type (s);
        Match (s, TK_R_paren);
      ELSE
        EVAL AddOp (s, OP_Empty);
      END;
      GenAttributes (s, att);
      FixWidth (s, z);
      Match (s, TK_Semi);
    END;
  END ExceptDecl;

(*------------------------------------------------------------ statements ---*)

CONST
  StmtStart = TKSet {TK_Case, TK_Exit, TK_Eval, TK_For, TK_If, TK_Lock,
                     TK_Loop, TK_Raise, TK_Repeat, TK_Return, TK_Try,
                     TK_Typecase, TK_While, TK_With, TK_Begin, TK_Assert,
                     TK_Ident, TK_L_paren, TK_Array, TK_Record}
                     + DeclStart;

PROCEDURE Stmt (VAR s: State) RAISES {Error} =
  VAR z := AddOp (s, OP_StmtList);
  BEGIN
    LOOP
      CASE s.tok OF
      | TK_Const,
        TK_Type,
        TK_Reveal,
        TK_Var,
        TK_External,
        TK_Inline,
        TK_Unused,
        TK_Obsolete,
        TK_Exception,
        TK_CallConv,
        TK_Procedure,
        TK_Fatal,
        TK_Begin     => Block (s);
      | TK_Ident,
        TK_L_paren,
        TK_Array,
        TK_Record    => AssignStmt (s);
      | TK_Assert    => AssertStmt (s);
      | TK_Case      => CaseStmt (s);
      | TK_Exit      => ExitStmt (s);
      | TK_Eval      => EvalStmt (s);
      | TK_For       => ForStmt (s);
      | TK_If        => IfStmt (s);
      | TK_Lock      => LockStmt (s);
      | TK_Loop      => LoopStmt (s);
      | TK_Raise     => RaiseStmt (s);
      | TK_Repeat    => RepeatStmt (s);
      | TK_Return    => ReturnStmt (s);
      | TK_Try       => TryStmt (s);
      | TK_Typecase  => TypeCaseStmt (s);
      | TK_While     => WhileStmt (s);
      | TK_With      => WithStmt (s);
      ELSE EXIT;
      END;
      IF (s.tok = TK_Semi) THEN
        GetToken (s); (* ; *)
        EmptyStmts (s);
      ELSIF (s.tok IN StmtStart) THEN
        (* assume the simple mistake and keep going *)
        Err (s, "expected ';', found ", TokName (s));
      ELSE
        EXIT;
      END;
    END;
    FixWidth (s, z);
  END Stmt;

PROCEDURE EmptyStmts (VAR s: State) RAISES {Error} =
  (* try to handle empty statements gracefully *)
  VAR err_line := -1;
  BEGIN
    WHILE (s.tok = TK_Semi) DO
      IF (err_line # s.scan.line) THEN
        Err (s, "empty statement, ignored");
        err_line := s.scan.line;
      END;
      GetToken (s); (* ; *)
    END;
  END EmptyStmts;

PROCEDURE AssignStmt (VAR s: State) RAISES {Error} =
  VAR z: CARDINAL;
  BEGIN
    z := AddOp (s, OP_Assign);
      Expr (s);
      IF (s.tok = TK_Assign) THEN
        GetToken (s);  (* := *)
        Expr (s);
      ELSE
        FixOp (s, z, OP_CallStmt);
      END;
    FixWidth (s, z);
  END AssignStmt;

PROCEDURE AssertStmt (VAR s: State) RAISES {Error} =
  VAR z: CARDINAL;
  BEGIN
    Match (s, TK_Assert);
    z := AddOp (s, OP_Assert);
      Expr (s);
      IF (s.tok # TK_End_pragma) THEN
        Err (s, "expected '*>', found ", TokName (s));
      ELSE
        s.tok := TK_Semi;  (* fake out the Stmt() parser *)
      END;
    FixWidth (s, z);
  END AssertStmt;

PROCEDURE CaseStmt (VAR s: State) RAISES {Error} =
  VAR z, zz: CARDINAL;  bar: TK;
  BEGIN
    Match (s, TK_Case);
    z := AddOp (s, OP_Case);
      Expr (s);
      Match (s, TK_Of);
      bar := s.tok;
      IF (bar = TK_Bar) THEN GetToken (s); (* | *) END;
      WHILE (s.tok # TK_Else) AND (s.tok # TK_End) DO
        CaseBranch (s);
        bar := s.tok;
        IF (bar # TK_Bar) THEN EXIT END;
        GetToken (s);  (* | *)
      END;
      IF (bar = TK_Bar) THEN
        Err (s, "missing case branch");
      END;
      IF (s.tok = TK_Else) THEN
        GetToken (s);  (* ELSE *)
        zz := AddOp (s, OP_CaseElse);
          Stmt (s);
        FixWidth (s, zz);
      END;
      Match (s, TK_End);
    FixWidth (s, z);
  END CaseStmt;

PROCEDURE CaseBranch (VAR s: State) RAISES {Error} =
  VAR z, zz: CARDINAL;
  BEGIN
    z := AddOp (s, OP_CaseBranch);
      (* read the labels *)
      LOOP
        zz := AddOp (s, OP_CaseLabel);
          Expr (s);
          IF (s.tok = TK_Dot_dot) THEN
            FixOp (s, z, OP_CaseRange);
            GetToken (s); (* .. *)
            Expr (s);
          END;
        FixWidth (s, zz);
        IF (s.tok # TK_Comma) THEN EXIT END;
        GetToken (s);  (* , *)
      END;
      Match (s, TK_Implies);
      Stmt (s);
    FixWidth (s, z);
  END CaseBranch;

PROCEDURE ExitStmt (VAR s: State) RAISES {Error} =
  BEGIN
    Match (s, TK_Exit);
    EVAL AddOp (s, OP_Exit);
  END ExitStmt;

PROCEDURE EvalStmt (VAR s: State) RAISES {Error} =
  VAR z: CARDINAL;
  BEGIN
    Match (s, TK_Eval);
    z := AddOp (s, OP_Eval);
      Expr (s);
    FixWidth (s, z);
  END EvalStmt;

PROCEDURE ForStmt (VAR s: State) RAISES {Error} =
  VAR z: CARDINAL;  id: M3ID.T;
  BEGIN
    Match (s, TK_For);
    id := MatchID (s);
    z := AddOp (s, OP_For1, id);
      Match (s, TK_Assign);
      Expr (s);
      Match (s, TK_To);
      Expr (s);
      IF (s.tok = TK_By) THEN
        FixOp (s, z, OP_ForN);
        GetToken (s);  (* BY *)
        Expr (s);
      END;
      Match (s, TK_Do);
      Stmt (s);
    FixWidth (s, z);
    Match (s, TK_End);
  END ForStmt;

PROCEDURE IfStmt (VAR s: State) RAISES {Error} =
  VAR z, zz: CARDINAL;
  BEGIN
    Match (s, TK_If);
    z := AddOp (s, OP_If);
      zz := AddOp (s, OP_IfClause);
        Expr (s);
        Match (s, TK_Then);
        Stmt (s);
      FixWidth (s, zz);
      WHILE (s.tok = TK_Elsif) DO
        GetToken (s);  (* ELSIF *)
        zz := AddOp (s, OP_IfClause);
          Expr (s);
          Match (s, TK_Then);
          Stmt (s);
        FixWidth (s, zz);
      END;
      IF (s.tok = TK_Else) THEN
        GetToken (s);  (* ELSE *)
        zz := AddOp (s, OP_IfElse);
          Stmt (s);
        FixWidth (s, zz);
      END;
    FixWidth (s, z);
    Match (s, TK_End);
  END IfStmt;

PROCEDURE LockStmt (VAR s: State) RAISES {Error} =
  VAR z: CARDINAL;
  BEGIN
    Match (s, TK_Lock);
    z := AddOp (s, OP_Lock);
      Expr (s);
      Match (s, TK_Do);
      Stmt (s);
    FixWidth (s, z);
    Match (s, TK_End);
  END LockStmt;

PROCEDURE LoopStmt (VAR s: State) RAISES {Error} =
  VAR z: CARDINAL;
  BEGIN
    Match (s, TK_Loop);
    z := AddOp (s, OP_Loop);
      Stmt (s);
    FixWidth (s, z);
    Match (s, TK_End);
  END LoopStmt;

PROCEDURE RaiseStmt (VAR s: State) RAISES {Error} =
  VAR z: CARDINAL;
  BEGIN
    Match (s, TK_Raise);
    z := AddOp (s, OP_Raise);
      QID (s);
      IF (s.tok = TK_L_paren) THEN
        FixOp (s, z, OP_RaiseValue);
        GetToken (s);  (* ( *)
        Expr (s);
        Match (s, TK_R_paren);
      END;
    FixWidth (s, z);
  END RaiseStmt;

PROCEDURE RepeatStmt (VAR s: State) RAISES {Error} =
  VAR z: CARDINAL;
  BEGIN
    Match (s, TK_Repeat);
    z := AddOp (s, OP_Repeat);
      Stmt (s);
      Match (s, TK_Until);
      Expr (s);
    FixWidth (s, z);
  END RepeatStmt;

PROCEDURE ReturnStmt (VAR s: State) RAISES {Error} =
  VAR z: CARDINAL;
  BEGIN
    Match (s, TK_Return);
    z := AddOp (s, OP_Return);
      IF (s.tok IN ExprStart) THEN
        FixOp (s, z, OP_ReturnValue);
        Expr (s);
      END;
    FixWidth (s, z);
  END ReturnStmt;

PROCEDURE TryStmt (VAR s: State) RAISES {Error} =
  VAR z, zz: CARDINAL;  bar: TK;
  BEGIN
    Match (s, TK_Try);
    z := AddOp (s, OP_TryFinally);
      Stmt (s);
      IF (s.tok = TK_Finally) THEN
        GetToken (s);  (* FINALLY *)
        Stmt (s);
      ELSE
        FixOp (s, z, OP_TryExcept);
        Match (s, TK_Except);
        bar := s.tok;
        IF (bar = TK_Bar) THEN GetToken (s); (* | *) END;
        WHILE (s.tok # TK_Else) AND (s.tok # TK_End) DO
          TryHandler (s);
          bar := s.tok;
          IF (bar # TK_Bar) THEN  EXIT END;
          GetToken (s);  (* | *)
        END;
        IF (bar = TK_Bar) THEN
          Err (s, "missing TRY EXCEPT handler");
        END;
        IF (s.tok = TK_Else) THEN
          GetToken (s);  (* ELSE *)
          zz := AddOp (s, OP_TryElse);
            Stmt (s);
          FixWidth (s, zz);
        END;
      END;
    FixWidth (s, z);
    Match (s, TK_End);
  END TryStmt;

PROCEDURE TryHandler (VAR s: State) RAISES {Error} =
  VAR z: CARDINAL;  id: M3ID.T;
  BEGIN
    z := AddOp (s, OP_TryHandler);
      LOOP
        QID (s);
        IF (s.tok # TK_Comma) THEN EXIT; END;
        GetToken (s);  (* , *)
      END;
      IF (s.tok = TK_L_paren) THEN
        GetToken (s);  (* ( *)
        id := MatchID (s);
        FixOpInfo (s, z, OP_TryHandlerVar, id);
        Match (s, TK_R_paren);
      END;
      Match (s, TK_Implies);
      Stmt (s);
    FixWidth (s, z);
  END TryHandler;

PROCEDURE TypeCaseStmt (VAR s: State) RAISES {Error} =
  VAR z, zz: CARDINAL;  bar: TK;
  BEGIN
    Match (s, TK_Typecase);
    z := AddOp (s, OP_TypeCase);
      Expr (s);
      Match (s, TK_Of);
      bar := s.tok;
      IF (bar = TK_Bar) THEN GetToken (s);  (* | *) END;
      WHILE (s.tok # TK_Else) AND (s.tok # TK_End) DO
        TypeCaseArm (s);
        bar := s.tok;
        IF (bar # TK_Bar) THEN EXIT; END;
        GetToken (s);  (* | *)
      END;
      IF (bar = TK_Bar) THEN
        Err (s, "missing TYPECASE arm");
      END;
      IF (s.tok = TK_Else) THEN
        GetToken (s);  (* ELSE *)
        zz := AddOp (s, OP_TypeCaseElse);
          Stmt (s);
        FixWidth (s, zz);
      END;
    FixWidth (s, z);
    Match (s, TK_End);
  END TypeCaseStmt;

PROCEDURE TypeCaseArm (VAR s: State) RAISES {Error} =
  VAR z: CARDINAL;  id: M3ID.T;
  BEGIN
    z := AddOp (s, OP_TypeCaseArm);
      LOOP
        Type (s);
        IF (s.tok # TK_Comma) THEN EXIT; END;
        GetToken (s);  (* , *)
      END;
      IF (s.tok = TK_L_paren) THEN
        GetToken (s); (* ( *)
        id := MatchID (s);
        FixOpInfo (s, z, OP_TypeCaseVar, id);
        Match (s, TK_R_paren);
      END;
      Match (s, TK_Implies);
      Stmt (s);
    FixWidth (s, z);
  END TypeCaseArm;

PROCEDURE WhileStmt (VAR s: State) RAISES {Error} =
  VAR z: CARDINAL;
  BEGIN
    Match (s, TK_While);
    z := AddOp (s, OP_While);
      Expr (s);
      Match (s, TK_Do);
      Stmt (s);
    FixWidth (s, z);
    Match (s, TK_End);
  END WhileStmt;

PROCEDURE WithStmt (VAR s: State) RAISES {Error} =
  BEGIN
    Match (s, TK_With);
    WithTail (s);
  END WithStmt;

PROCEDURE WithTail (VAR s: State) RAISES {Error} =
  VAR z: CARDINAL;  id: M3ID.T;
  BEGIN
    id := MatchID (s);
    z := AddOp (s, OP_With, id);
      Match (s, TK_Equal);
      Expr (s);
      IF (s.tok = TK_Comma) THEN
        GetToken (s); (* , *)
        WithTail (s);
      ELSE
        Match (s, TK_Do);
        Stmt (s);
        Match (s, TK_End);
      END;
    FixWidth (s, z);
  END WithTail;

(*----------------------------------------------------------------- types ---*)

(****
CONST
  TypeStart = TKSet {TK_Ident, TK_Array, TK_Bits, TK_Branded, TK_L_brace,
                     TK_Untraced, TK_Object, TK_Procedure, TK_Record,
                     TK_Ref, TK_Set, TK_L_bracket, TK_L_paren};
***)

PROCEDURE Type (VAR s: State) RAISES {Error} =
  VAR z: CARDINAL;
  BEGIN
    CASE s.tok OF
    | TK_Ident     => NamedType (s);
    | TK_Array     => ArrayType (s);
    | TK_Bits      => PackedType (s);
    | TK_Branded   => RefType (s);
    | TK_L_brace   => EnumType (s);
    | TK_Untraced  => RefType (s);
    | TK_Object    => ObjectType (s);
    | TK_CallConv  => ProcType (s);
    | TK_Procedure => ProcType (s);
    | TK_Record    => RecordType (s);
    | TK_Ref       => RefType (s);
    | TK_Set       => SetType (s);
    | TK_L_bracket => SubrangeType (s);
    | TK_L_paren =>
        z := s.n_ops;
        GetToken (s); (* ( *)
        Type (s);
        Match (s, TK_R_paren);
        IF (s.tok = TK_Branded) OR (s.tok = TK_Object) THEN
          ObjectTail (s, z);
        END;
    ELSE
        Err (s, "bad type expression");
    END;
  END Type;

PROCEDURE ArrayType (VAR s: State) RAISES {Error} =
  VAR z: CARDINAL;
  BEGIN
    Match (s, TK_Array);
    IF (s.tok = TK_Of) THEN
      GetToken (s); (* OF *)
      z := AddOp (s, OP_OpenArray);
        Type (s);
      FixWidth (s, z);
    ELSE
      ArrayTail (s, AddOp (s, OP_Array));
    END;
  END ArrayType;

PROCEDURE ArrayTail (VAR s: State;  head: CARDINAL) RAISES {Error} =
  BEGIN
    Type (s);
    IF (s.tok = TK_Comma) THEN
      GetToken (s);  (* , *)
      ArrayTail (s, AddOp (s, OP_Array));
    ELSE
      Match (s, TK_Of);
      Type (s);
    END;
    FixWidth (s, head);
  END ArrayTail;

PROCEDURE EnumType (VAR s: State) RAISES {Error} =
  VAR z: CARDINAL;
  BEGIN
    Match (s, TK_L_brace);
    z := AddOp (s, OP_Enum);
      IF (s.tok = TK_Ident) THEN
        LOOP
          EVAL AddOp (s, OP_EnumDefn, MatchID (s));
          IF (s.tok # TK_Comma) THEN EXIT; END;
          GetToken (s); (* , *)
        END;
      END;
    FixWidth (s, z);
    Match (s, TK_R_brace);
  END EnumType;

PROCEDURE NamedType (VAR s: State) RAISES {Error} =
  VAR z := AddOp (s, OP_NamedType);
  BEGIN
    QID (s);
    FixWidth (s, z);
    IF (s.tok = TK_Branded) OR (s.tok = TK_Object) THEN
      ObjectTail (s, z);
    END;
  END NamedType;

PROCEDURE ObjectType (VAR s: State) RAISES {Error} =
  VAR z := AddOp (s, OP_Object);
  BEGIN
    ObjectBody (s, z);
    FixWidth (s, z);
  END ObjectType;

PROCEDURE ObjectTail (VAR s: State;  super: CARDINAL) RAISES {Error} =
  BEGIN
    InsertOp (s, super, OP_Object);
    Brand (s);
    ObjectBody (s, super);
    FixWidth (s, super);
  END ObjectTail;

PROCEDURE Brand (VAR s: State) RAISES {Error} =
  BEGIN
    IF (s.tok = TK_Branded) THEN
      GetToken (s);  (* BRANDED *)
      IF (s.tok IN ExprStart)
        THEN Expr (s);
        ELSE EVAL AddOp (s, OP_DefaultBrand);
      END;
    ELSE
      EVAL AddOp (s, OP_NoBrand);
    END;
  END Brand;

PROCEDURE ObjectBody (VAR s: State;  head: CARDINAL) RAISES {Error} =
  BEGIN
    Match (s, TK_Object);
    Fields (s);
    IF (s.tok = TK_Methods) THEN
      GetToken (s);  (* METHODS *)
      Methods (s);
    END;
    IF (s.tok = TK_Overrides) THEN
      GetToken (s);  (* OVERRIDES *)
      Overrides (s);
    END;
    Match (s, TK_End);

    IF (s.tok = TK_Branded) OR (s.tok = TK_Object) THEN
      ObjectTail (s, head);
    END;
  END ObjectBody;

PROCEDURE Fields (VAR s: State) RAISES {Error} =
  VAR z: CARDINAL;
  BEGIN
    WHILE (s.tok = TK_Ident) DO
      z := AddOp (s, OP_Field);
        FieldDecls (s, OP_FieldDefn);
      FixWidth (s, z);
      IF (s.tok # TK_Semi) THEN EXIT; END;
      GetToken (s);  (* ; *)
    END;
  END Fields;

PROCEDURE FieldDecls (VAR s: State;  defn_op: OP) RAISES {Error} =
  BEGIN
    LOOP
      EVAL AddOp (s, defn_op, MatchID (s));
      IF (s.tok # TK_Comma) THEN EXIT; END;
      GetToken (s);  (* , *)
    END;

    IF (s.tok = TK_Colon) THEN
      GetToken (s);  (* : *)
      Type (s);
    ELSE
      EVAL AddOp (s, OP_Empty);
    END;

    IF (s.tok = TK_Assign) THEN
      GetToken (s);  (* := *)
      Expr (s);
    ELSE
      EVAL AddOp (s, OP_Empty);
    END;
  END FieldDecls;

PROCEDURE Methods (VAR s: State) RAISES {Error} =
  VAR z: CARDINAL;  id: M3ID.T;
  BEGIN
    WHILE (s.tok = TK_Ident) DO
      id := MatchID (s);
      z := AddOp (s, OP_Method, id);
        ProcSignature (s, M3ID.NoID);
        IF (s.tok = TK_Assign) THEN
          GetToken (s);  (* := *)
          Expr (s);
        ELSE
          EVAL AddOp (s, OP_Empty);
        END;
      FixWidth (s, z);
      IF (s.tok # TK_Semi) THEN EXIT; END;
      GetToken (s);  (* ; *)
    END;
  END Methods;

PROCEDURE Overrides (VAR s: State) RAISES {Error} =
  VAR z: CARDINAL;  id: M3ID.T;
  BEGIN
    WHILE (s.tok = TK_Ident) DO
      id := MatchID (s);
      z := AddOp (s, OP_Override, id);
        Match (s, TK_Assign);
        Expr (s);
      FixWidth (s, z);
      IF (s.tok # TK_Semi) THEN EXIT; END;
      GetToken (s);  (* ; *)
    END;
  END Overrides;

PROCEDURE PackedType (VAR s: State) RAISES {Error} =
  VAR z := AddOp (s, OP_Packed);
  BEGIN
    Match (s, TK_Bits);
    Expr (s);
    Match (s, TK_For);
    Type (s);
    FixWidth (s, z);
  END PackedType;

PROCEDURE ProcType (VAR s: State) RAISES {Error} =
  VAR cc := M3ID.NoID;
  BEGIN
    IF (s.tok = TK_CallConv) THEN
      cc := s.scan.id;
      GetToken (s);  (* calling convention *)
      Match (s, TK_End_pragma);
    END;
    Match (s, TK_Procedure);
    ProcSignature (s, cc);
  END ProcType;

CONST FormalStart = TKSet {TK_Value, TK_Var, TK_Readonly, TK_Ident, TK_Unused};

PROCEDURE ProcSignature (VAR s: State;  cc: M3ID.T) RAISES {Error} =
  VAR z := AddOp (s, OP_ProcType, cc);
  BEGIN
    Match (s, TK_L_paren);
    WHILE (s.tok IN FormalStart) DO
      Formal (s);
      IF (s.tok # TK_Semi) THEN EXIT END;
      GetToken (s);  (* ; *)
    END;
    Match (s, TK_R_paren);

    IF (s.tok = TK_Colon) THEN
      GetToken (s); (* : *)
      Type (s);
    ELSE
      EVAL AddOp (s, OP_Empty);
    END;

    Raises (s);

    FixWidth (s, z);
  END ProcSignature;

PROCEDURE Formal (VAR s: State) RAISES {Error} =
  VAR z: CARDINAL;  mode: INTEGER;
  BEGIN
    IF    (s.tok = TK_Value)    THEN mode := 0;  GetToken (s);
    ELSIF (s.tok = TK_Var)      THEN mode := 1;  GetToken (s);
    ELSIF (s.tok = TK_Readonly) THEN mode := 2;  GetToken (s);
    ELSE                             mode := 0;
    END;
    z := AddOp (s, OP_Formal, mode);
      FieldDecls (s, OP_FormalDefn);
    FixWidth (s, z);
  END Formal;

PROCEDURE Raises (VAR s: State) RAISES {Error} =
  VAR z: CARDINAL;
  BEGIN
    IF (s.tok = TK_Raises) THEN
      GetToken (s);  (* RAISES *)
      IF (s.tok = TK_Any) THEN
        EVAL AddOp (s, OP_RaisesAny);
      ELSE
        z := AddOp (s, OP_Raises);
        Match (s, TK_L_brace);
          IF (s.tok = TK_Ident) THEN
            LOOP
              QID (s);
              IF (s.tok # TK_Comma) THEN EXIT END;
              GetToken (s);  (* , *)
            END;
          END;
        Match (s, TK_R_brace);
        FixWidth (s, z);
      END;
    ELSE
      EVAL AddOp (s, OP_Raises);
    END;
  END Raises;

PROCEDURE RecordType (VAR s: State) RAISES {Error} =
  VAR z := AddOp (s, OP_Record);
  BEGIN
    Match (s, TK_Record);
    Fields (s);
    Match (s, TK_End);
    FixWidth (s, z);
  END RecordType;

VAR root_id := M3ID.NoID;

PROCEDURE RefType (VAR s: State) RAISES {Error} =
  VAR z := s.n_ops;  id: M3ID.T;
  BEGIN
    IF (s.tok = TK_Untraced) THEN
      GetToken (s); (* UNTRACED *)
      IF (s.tok = TK_Ident) THEN
        id := MatchID (s);
        IF (root_id = M3ID.NoID) THEN root_id := M3ID.Add ("ROOT"); END;
        IF (id # root_id) THEN
          Err (s, "expected UNTRACED ROOT, found ", M3ID.ToText (id));
        END;
        ObjectTail (s, AddOp (s, OP_UntracedRoot));
        RETURN;
      END;
      z := AddOp (s, OP_UntracedRef);
    ELSE
      z := AddOp (s, OP_Ref);
    END;
    Brand (s);
    IF (s.tok = TK_Ref) THEN
      GetToken (s);  (* REF *)
      Type (s);
      FixWidth (s, z);
    ELSE (* must be: BRANDED "foo" OBJECT ... *)
      InsertOp (s, z, OP_Root);   (* before the brand *)
      InsertOp (s, z, OP_Object); (* before ROOT *)
      ObjectBody (s, z);
      FixWidth (s, z);
    END;
  END RefType;

PROCEDURE SetType (VAR s: State) RAISES {Error} =
  VAR z := AddOp (s, OP_Set);
  BEGIN
    Match (s, TK_Set);
    Match (s, TK_Of);
    Type (s);
    FixWidth (s, z);
  END SetType;

PROCEDURE SubrangeType (VAR s: State) RAISES {Error} =
  VAR z := AddOp (s, OP_Subrange);
  BEGIN
    Match (s, TK_L_bracket);
    Expr (s);
    Match (s, TK_Dot_dot);
    Expr (s);
    Match (s, TK_R_bracket);
    FixWidth (s, z);
  END SubrangeType;

(*----------------------------------------------------------- expressions ---*)

CONST
  ExprStart = TKSet {TK_Not, TK_Plus, TK_Minus, TK_Ident, TK_Card_const,
                     TK_Longreal_const, TK_Real_const, TK_Extended_const,
                     TK_Char_const, TK_Text_const, TK_L_paren,
                     TK_Array, TK_Bits, TK_Record, TK_Set};

PROCEDURE Expr (VAR s: State) RAISES {Error} =
  BEGIN
    E0 (s, FALSE);
  END Expr;

PROCEDURE E0 (VAR s: State;  types: BOOLEAN) RAISES {Error} =
  VAR z := s.n_ops;
  BEGIN
    E1 (s, types);
    WHILE (s.tok = TK_Or) DO
      GetToken (s); (* OR *)
      InsertOp (s, z, OP_Or);
      E1 (s, FALSE);
      FixWidth (s, z);
    END;
  END E0;

PROCEDURE E1 (VAR s: State;  types: BOOLEAN) RAISES {Error} =
  VAR z := s.n_ops;
  BEGIN
    E2 (s, types);
    WHILE (s.tok = TK_And) DO
      GetToken (s); (* AND *)
      InsertOp (s, z, OP_And);
      E2 (s, FALSE);
      FixWidth (s, z);
    END;
  END E1;

PROCEDURE E2 (VAR s: State;  types: BOOLEAN) RAISES {Error} =
  VAR n := 0;  z := s.n_ops;
  BEGIN
    WHILE (s.tok = TK_Not) DO
      GetToken (s); (* NOT *)
      EVAL AddOp (s, OP_Not);
      INC (n);
    END;
    E3 (s, types AND (n = 0));
    WHILE n > 0 DO
      FixWidth (s, z);
      INC (z);  DEC (n);
    END;
  END E2;

CONST RelOps = TKSet {TK_Equal, TK_Sharp, TK_Less, TK_Ls_equal,
                      TK_Greater, TK_Gr_equal, TK_In};

PROCEDURE E3 (VAR s: State;  types: BOOLEAN) RAISES {Error} =
  VAR z := s.n_ops;  op: OP;
  BEGIN
    E4 (s, types);
    WHILE (s.tok IN RelOps) DO
      CASE s.tok OF
      | TK_Equal    => op := OP_EQ;
      | TK_Sharp    => op := OP_NE;
      | TK_Less     => op := OP_LT;
      | TK_Ls_equal => op := OP_LE;
      | TK_Greater  => op := OP_GT;
      | TK_Gr_equal => op := OP_GE;
      | TK_In       => op := OP_Member;
      ELSE             <*ASSERT FALSE*>
      END;
      GetToken (s); (* operator *)
      InsertOp (s, z, op);
      E4 (s, FALSE);
      FixWidth (s, z);
    END;
  END E3;

CONST AddOps = TKSet {TK_Plus, TK_Minus, TK_Ampersand};

PROCEDURE E4 (VAR s: State;  types: BOOLEAN) RAISES {Error} =
  VAR z := s.n_ops;  op: OP;
  BEGIN
    E5 (s, types);
    WHILE (s.tok IN AddOps) DO
      CASE s.tok OF
      | TK_Plus      => op := OP_Add;
      | TK_Minus     => op := OP_Subtract;
      | TK_Ampersand => op := OP_Concat;
      ELSE               <*ASSERT FALSE*>
      END;
      GetToken (s); (* operator *)
      InsertOp (s, z, op);
      E5 (s, FALSE);
      FixWidth (s, z);
    END;
  END E4;

CONST MulOps = TKSet {TK_Asterisk, TK_Slash, TK_Div, TK_Mod};

PROCEDURE E5 (VAR s: State;  types: BOOLEAN) RAISES {Error} =
  VAR z := s.n_ops;  op: OP;
  BEGIN
    E6 (s, types);
    WHILE (s.tok IN MulOps) DO
      CASE s.tok OF
      | TK_Asterisk => op := OP_Multiply;
      | TK_Slash    => op := OP_Divide;
      | TK_Div      => op := OP_Div;
      | TK_Mod      => op := OP_Mod;
      ELSE              <*ASSERT FALSE*>
      END;
      GetToken (s); (* operator *)
      InsertOp (s, z, op);
      E6 (s, FALSE);
      FixWidth (s, z);
    END;
  END E5;

CONST SelectStart = TKSet {TK_Arrow, TK_Dot, TK_L_bracket, TK_L_paren,
                           TK_L_brace, TK_Branded, TK_Object};

PROCEDURE E6 (VAR s: State;  types: BOOLEAN) RAISES {Error} =
  VAR cnt := 0;  z := s.n_ops;
  BEGIN
    LOOP
      IF (s.tok = TK_Plus)  THEN
        GetToken (s);  INC (cnt);
        EVAL AddOp (s, OP_UnaryPlus);
      ELSIF (s.tok = TK_Minus) THEN
        GetToken (s);  INC (cnt);
        EVAL AddOp (s, OP_UnaryMinus);
      ELSE
        EXIT;
      END;
    END;
    E7 (s, types AND (cnt = 0));
    WHILE (cnt > 0) DO
      FixWidth (s, z);
      INC (z);  DEC (cnt);
    END;
  END E6;

PROCEDURE E7 (VAR s: State;  types: BOOLEAN) RAISES {Error} =
  VAR z := s.n_ops;
  BEGIN
    E8 (s, types);
    WHILE (s.tok IN SelectStart) DO
      CASE s.tok OF
      | TK_Arrow =>
          GetToken (s); (* ^ *)
          InsertOp (s, z, OP_Deref);
          FixWidth (s, z);
      | TK_Dot =>
          GetToken (s); (* . *)
          InsertOp (s, z, OP_Qualify);
          FixOpInfo (s, z, OP_Qualify, MatchID (s));
          FixWidth (s, z);
      | TK_L_bracket =>
          GetToken (s); (* [ *)
          LOOP
            InsertOp (s, z, OP_Subscript);
            E0 (s, FALSE);
            FixWidth (s, z);
            IF (s.tok # TK_Comma) THEN EXIT END;
            GetToken (s); (* , *)
          END;
          Match (s, TK_R_bracket);
      | TK_L_paren =>
          GetToken (s); (* ( *)
          InsertOp (s, z, OP_CallExpr);
          ArgList (s);
          Match (s, TK_R_paren);
          FixWidth (s, z);
      | TK_L_brace =>
          GetToken (s); (* { *)
          InsertOp (s, z, OP_ConsExpr);
          ConsList (s);
          Match (s, TK_R_brace);
          FixWidth (s, z);
      | TK_Branded, TK_Object =>
          IF (types) THEN ObjectTail (s, z); END;
          EXIT;
      ELSE Err (s, "unrecognized selector ", TokName (s));
      END;
    END;
  END E7;

PROCEDURE E8 (VAR s: State;  types: BOOLEAN) RAISES {Error} =
  BEGIN
    CASE s.tok OF
    | TK_Ident         => EVAL AddOp (s, OP_Id, s.scan.id);  GetToken (s);
    | TK_Char_const    => EVAL AddOp (s, OP_Char, s.scan.char);  GetToken (s);
    | TK_Card_const    => ScanInt (s);
    | TK_Text_const    => ScanText (s);
    | TK_Real_const    => ScanFloat (s);
    | TK_Longreal_const=> ScanFloat (s);
    | TK_Extended_const=> ScanFloat (s);

    | TK_L_paren =>
        GetToken (s); (* ( *)
        E0 (s, types);
        Match (s, TK_R_paren);

    | TK_Array, TK_Bits, TK_Record, TK_Set =>
        Type (s);
        IF (NOT types) AND (s.tok # TK_L_brace) THEN
          Err (s, "expected a constructor, found ", TokName (s));
        END;

    | TK_Branded, TK_L_brace, TK_Untraced, TK_Object,
      TK_Procedure, TK_Ref, TK_L_bracket, TK_CallConv =>
        IF NOT types THEN Err (s, "unexpected type expression") END;
        Type (s);

    ELSE
        Err (s, "unrecognized expression");
        EVAL AddOp (s, OP_Int, 0);
    END;
  END E8;

PROCEDURE ArgList (VAR s: State) RAISES {Error} =
  BEGIN
    IF (s.tok # TK_R_paren) THEN
      LOOP
        Actual (s);
        IF (s.tok # TK_Comma) THEN EXIT END;
        GetToken (s); (* , *)
      END;
    END;
  END ArgList;

PROCEDURE Actual (VAR s: State) RAISES {Error} =
  VAR z := s.n_ops;
  BEGIN
    E0 (s, TRUE);
    IF (s.tok = TK_Assign) THEN
      GetToken (s); (* := *)
      InsertOp (s, z, OP_NameBind);
      E0 (s, FALSE);
      FixWidth (s, z);
    END;
  END Actual;

PROCEDURE ConsList (VAR s: State) RAISES {Error} =
  BEGIN
    IF (s.tok # TK_R_brace) THEN
      LOOP
        IF (s.tok = TK_Dot_dot) THEN
          (* must be the end of an array constructor *)
          GetToken (s); (* .. *)
          EVAL AddOp (s, OP_Etc);
          EXIT;
        END;
        Constructor (s);
        IF (s.tok # TK_Comma) THEN EXIT END;
        GetToken (s); (* , *)
      END;
    END;
  END ConsList;

PROCEDURE Constructor (VAR s: State) RAISES {Error} =
  VAR z := s.n_ops;
  BEGIN
    E0 (s, FALSE);
    IF (s.tok = TK_Dot_dot) THEN
      GetToken (s); (* .. *)
      InsertOp (s, z, OP_RangeExpr);
      E0 (s, FALSE);
      FixWidth (s, z);
    ELSIF (s.tok = TK_Assign) THEN
      GetToken (s); (* := *)
      InsertOp (s, z, OP_NameBind);
      E0 (s, FALSE);
      FixWidth (s, z);
    END;
  END Constructor;

(*--------------------------------------------------------------- pragmas ---*)

TYPE
  DeclAttributes = RECORD
    gotSome     : BOOLEAN;
    isInline    : BOOLEAN;
    isExternal  : BOOLEAN;
    isUnused    : BOOLEAN;
    isObsolete  : BOOLEAN;
    alias       : M3ID.T;
    callingConv : M3ID.T;
  END;

PROCEDURE DeclPragmas (VAR s: State;  VAR att: DeclAttributes) RAISES {Error} =
  BEGIN
    att.gotSome     := FALSE;
    att.isInline    := FALSE;
    att.isExternal  := FALSE;
    att.isUnused    := FALSE;
    att.isObsolete  := FALSE;
    att.alias       := M3ID.NoID;
    att.callingConv := M3ID.NoID;

    LOOP
      CASE s.tok OF
      | TK_External =>
          IF NOT s.ast.interface THEN
            Err (s, "External declarations only allowed in interfaces");
          END;
          ExternalPragma (s, att.alias, att.callingConv);
          att.isExternal := TRUE;
          att.gotSome := TRUE;
      | TK_Inline   =>
          att.isInline := TRUE;
          GetToken (s); (* INLINE *)
          Match (s, TK_End_pragma);
          att.gotSome := TRUE;
      | TK_Unused   =>
          att.isUnused := TRUE;
          GetToken (s); (* UNUSED *)
          Match (s, TK_End_pragma);
          att.gotSome := TRUE;
      | TK_Obsolete =>
          att.isObsolete := TRUE;
          GetToken (s); (* OBSOLETE *)
          Match (s, TK_End_pragma);
          att.gotSome := TRUE;
      | TK_CallConv   =>
          att.callingConv := s.scan.id;
          GetToken (s); (* convention name *)
          Match (s, TK_End_pragma);
          att.gotSome := TRUE;
      ELSE EXIT;
      END;
    END;
  END DeclPragmas;

PROCEDURE ExternalPragma (VAR s: State;
                          VAR(*OUT*) alias: M3ID.T;
                          VAR(*OUT*) cc: M3ID.T) RAISES {Error} =
  BEGIN
    alias := M3ID.NoID;  (* default => use the Modula-3 name *)
    cc    := M3ID.NoID;

    Match (s, TK_External);

    IF (s.tok = TK_Ident) OR (s.tok = TK_Text_const) THEN
      IF (s.tok = TK_Ident)
        THEN alias := s.scan.id;
        ELSE alias := M3ID.Add (s.scan.text);
      END;
      GetToken (s);  (* Ident or Text_const *)

      IF (s.tok = TK_Colon) THEN
        GetToken (s); (* : *)
        IF (s.tok = TK_Ident)         THEN cc := s.scan.id;
        ELSIF (s.tok = TK_Text_const) THEN cc := M3ID.Add (s.scan.text);
        ELSE                               cc := M3ID.NoID;
        END;
        IF (cc # M3ID.NoID) THEN
          IF Target.FindConvention (M3ID.ToText (cc)) = NIL THEN
            Err (s, "unsupported language or calling convention: ",
                 M3ID.ToText (cc));
          END;
          GetToken (s); (* Ident or Text_const *)
        ELSE
          Err (s, "missing language after ':' in <*EXTERNAL*> pragma");
        END;
      END;
    END;

    Match (s, TK_End_pragma);
  END ExternalPragma;

PROCEDURE GenAttributes (VAR s: State;  READONLY att: DeclAttributes) =
  VAR z: CARDINAL;
  BEGIN
    IF att.gotSome THEN
      z := AddOp (s, OP_Attributes);
      IF att.isInline    THEN EVAL AddOp (s, OP_Inline); END;
      IF att.isUnused    THEN EVAL AddOp (s, OP_Unused); END;
      IF att.isObsolete  THEN EVAL AddOp (s, OP_Obsolete); END;
      IF att.isExternal  THEN EVAL AddOp (s, OP_External); END;
      IF att.alias # M3ID.NoID THEN
        EVAL AddOp (s, OP_Alias, att.alias);
      END;
      IF att.callingConv # M3ID.NoID THEN
        EVAL AddOp (s, OP_CallConv, att.callingConv);
      END;
      FixWidth (s, z);
    END;
  END GenAttributes;

PROCEDURE FatalPragma (VAR s: State;  READONLY att: DeclAttributes)
  RAISES {Error} =
  VAR any := FALSE;  started := FALSE;  z: CARDINAL;
  BEGIN
    IF (att.gotSome) THEN
      Err (s, "cannot attach pragma attributes to <*FATAL*> declaration");
    END;
    Match (s, TK_Fatal);
    LOOP
      IF (s.tok = TK_Any) THEN
        GetToken (s);  (* ANY *)
        any := TRUE;
      ELSIF (s.tok = TK_Ident) THEN
        IF NOT started THEN z := AddOp (s, OP_Fatal);  started := TRUE;  END;
        QID (s);
      ELSE
        EXIT;
      END;
      IF (s.tok # TK_Comma) THEN EXIT; END;
      GetToken (s);  (* , *)
    END;
    IF (started) THEN FixWidth (s, z); END;
    IF (any) THEN EVAL AddOp (s, OP_FatalAny); END;
    IF NOT (started OR any) THEN
      Err (s, "missing exception list or ANY in <*FATAL*> pragma");
    END;
    Match (s, TK_End_pragma);
  END FatalPragma;

(*------------------------------------------------------- token utilities ---*)

PROCEDURE ScanText (VAR s: State) RAISES {Error} =
  VAR index := s.n_texts;
  BEGIN
    IF (s.ast.texts = NIL) THEN
      s.ast.texts := NEW (REF ARRAY OF TEXT, 16);
    ELSIF (index >= NUMBER (s.ast.texts^)) THEN
      ExpandTexts (s);
    END;
    s.ast.texts [index] := s.scan.text;    INC (s.n_texts);
    EVAL AddOp (s, OP_Text, index);
    GetToken (s);
  END ScanText;

PROCEDURE ExpandTexts (VAR s: State) =
  VAR n := NUMBER (s.ast.texts^);  xx := NEW (REF ARRAY OF TEXT, n+n);
  BEGIN
    SUBARRAY (xx^, 0, n) := s.ast.texts^;
    s.ast.texts := xx;
  END ExpandTexts;

PROCEDURE ScanInt (VAR s: State) RAISES {Error} =
  VAR index := s.n_ints;  val: INTEGER;
  BEGIN
    IF TInt.ToInt (s.scan.int, val) THEN
      EVAL AddOp (s, OP_Int, val);
    ELSE
      IF (s.ast.ints = NIL) THEN
        s.ast.ints := NEW (REF ARRAY OF Target.Int, 16);
      ELSIF (index >= NUMBER (s.ast.ints^)) THEN
        ExpandInts (s);
      END;
      s.ast.ints [index] := s.scan.int;  INC (s.n_ints);
      EVAL AddOp (s, OP_BigInt, index);
    END;
    GetToken (s);
  END ScanInt;

PROCEDURE ExpandInts (VAR s: State) =
  VAR n := NUMBER (s.ast.ints^);  xx := NEW (REF ARRAY OF Target.Int, n+n);
  BEGIN
    SUBARRAY (xx^, 0, n) := s.ast.ints^;
    s.ast.ints := xx;
  END ExpandInts;

PROCEDURE ScanFloat (VAR s: State) RAISES {Error} =
  VAR  index := s.n_floats;  op: OP;
  BEGIN
    IF    (s.tok = TK_Real_const)     THEN op := OP_Real;
    ELSIF (s.tok = TK_Longreal_const) THEN op := OP_LReal;
    ELSIF (s.tok = TK_Extended_const) THEN op := OP_EReal;
    ELSE <*ASSERT FALSE*>
    END;
    IF (s.ast.floats = NIL) THEN
      s.ast.floats := NEW (REF ARRAY OF Target.Float, 16);
    ELSIF (s.n_floats >= NUMBER (s.ast.floats^)) THEN
      ExpandFloats (s);
    END;
    s.ast.floats [index] := s.scan.float;  INC (s.n_floats);
    EVAL AddOp (s, op, index);
    GetToken (s);
  END ScanFloat;

PROCEDURE ExpandFloats (VAR s: State) =
  VAR n := NUMBER (s.ast.floats^);  xx := NEW (REF ARRAY OF Target.Float, n+n);
  BEGIN
    SUBARRAY (xx^, 0, n) := s.ast.floats^;
    s.ast.floats := xx;
  END ExpandFloats;

PROCEDURE QID (VAR s: State)  RAISES {Error} =
  VAR id1, id2: M3ID.T;  z: CARDINAL;
  BEGIN
    id1 := MatchID (s);
    IF (s.tok = TK_Dot) THEN
      GetToken (s); (* . *)
      id2 := MatchID (s);
      z := AddOp (s, OP_Qualify, id2);
        EVAL AddOp (s, OP_Id, id1);
      FixWidth (s, z);
    ELSE
      EVAL AddOp (s, OP_Id, id1);
    END;
  END QID;

PROCEDURE MatchID (VAR s: State): M3ID.T  RAISES {Error} =
  VAR id: M3ID.T;
  BEGIN
    IF (s.tok # TK_Ident) THEN
      Err (s, "expected an identifier, but found ", TokName (s));
    END;
    id := s.scan.id;
    GetToken (s);
    RETURN id;
  END MatchID;

PROCEDURE Match (VAR s: State;  tk: TK) RAISES {Error} =
  BEGIN
    IF (s.tok # tk) THEN
      Err (s, "expected ", s.scan.className (tk), ", but found ", TokName (s));
    END;
    GetToken (s);
  END Match;

PROCEDURE GetToken (VAR s: State) RAISES {Error} =
  BEGIN
    REPEAT
      s.scan.next ();
      s.tok := s.scan.token;
    UNTIL (s.tok # TK_Comment);
    IF (s.tok = TK_Error) THEN
      Err (s, "unrecognized input token: ", TokName (s));
    END;
  END GetToken;

PROCEDURE TokName (VAR s: State): TEXT =
  VAR txt := s.scan.toText ();
  BEGIN
    <*ASSERT txt # NIL*>
    IF Text.Length (txt) > 27 THEN
      txt := Text.Sub (txt, 0, 24) & "...";
    END;
    RETURN txt;
  END TokName;

PROCEDURE Err (VAR s: State;  a, b, c, d: TEXT := NIL) RAISES {Error} =
  VAR msg := "";
  BEGIN
    IF (a # NIL) THEN msg := msg & a; END;
    IF (b # NIL) THEN msg := msg & b; END;
    IF (c # NIL) THEN msg := msg & c; END;
    IF (d # NIL) THEN msg := msg & d; END;
    IF s.err (msg, s.scan) THEN RAISE Error; END;
  END Err;

(*------------------------------------------------------- Chunk utilities ---*)

PROCEDURE AddOp (VAR s: State;  op: OP;  info := 0): CARDINAL =
  BEGIN
    IF (s.cur > LAST (s.tail.nodes)) THEN
      s.tail.next := NEW (Chunk);
      s.tail := s.tail.next;
      INC (s.base, NUMBER (s.tail.nodes));
      s.cur := 0;
    END;
    WITH n = s.tail.nodes[s.cur] DO
      n.op    := op;
      n.info  := info;
      n.width := 1; (* self *)
    END;
    INC (s.n_ops);
    INC (s.cur);
    RETURN s.n_ops - 1;
  END AddOp;

PROCEDURE FixOp (VAR s: State;  n: CARDINAL;  op: OP) =
  VAR c := s.tail;
  BEGIN
    IF (n >= s.base) THEN
      DEC (n, s.base);
    ELSE
      c := s.head;
      WHILE (n > LAST (c.nodes)) DO
        c := c.next;  DEC (n, NUMBER (c.nodes));
      END;
    END;
    c.nodes[n].op := op;
  END FixOp;

PROCEDURE FixOpInfo (VAR s: State;  n: CARDINAL;  op: OP;  info: INTEGER) =
  VAR c := s.tail;
  BEGIN
    IF (n >= s.base) THEN
      DEC (n, s.base);
    ELSE
      c := s.head;
      WHILE (n > LAST (c.nodes)) DO
        c := c.next;  DEC (n, NUMBER (c.nodes));
      END;
    END;
    WITH z = c.nodes[n] DO  z.op := op;  z.info := info;  END;
  END FixOpInfo;

PROCEDURE FixWidth (VAR s: State;  n: CARDINAL) =
  VAR c := s.tail;  width := s.n_ops - n;
  BEGIN
    IF (n >= s.base) THEN
      DEC (n, s.base);
    ELSE
      c := s.head;
      WHILE (n > LAST (c.nodes)) DO
        c := c.next;  DEC (n, NUMBER (c.nodes));
      END;
    END;
    <*ASSERT width > 0 *>
    c.nodes[n].width := width;
  END FixWidth;

PROCEDURE InsertOp (VAR s: State;  n: CARDINAL;  op: OP) =
  BEGIN
    EVAL AddOp (s, OP_Empty); (* make room for a new node *)
    OpenGap (s, n);
    FixOp (s, n, op);
  END InsertOp;

PROCEDURE OpenGap (VAR s: State;  n: CARDINAL) =
  VAR c := s.tail;  cnt := s.n_ops - n - 1;  tmp, tmp2: Node;
  BEGIN
    (* find the 'n'th node *)
    IF (n >= s.base) THEN
      DEC (n, s.base);
    ELSE
      c := s.head;
      WHILE (n > LAST (c.nodes)) DO
        c := c.next;  DEC (n, NUMBER (c.nodes));
      END;
    END;

    tmp.op     := OP_Empty;
    tmp.width  := 1;
    tmp.info   := 0;
    tmp.client := 0;

    WHILE (cnt > 0) DO
      WITH z = c.nodes[n] DO
        tmp2 := z;  z := tmp;  tmp := tmp2;
      END;
      DEC (cnt);
      INC (n);
      IF (n >= NUMBER (c.nodes)) THEN
        c := c.next;
        n := 0;
      END;
    END;
    c.nodes[n] := tmp;

  END OpenGap;

PROCEDURE FlattenChunks (VAR s: State): REF ARRAY OF Node =
  (* build the final, flat list of nodes *)
  VAR
    nn := NEW (REF ARRAY OF Node, s.base + s.cur);
    c  := s.head;
    x  := 0;
  BEGIN
    WHILE c # s.tail DO
      SUBARRAY (nn^, x, NUMBER (c.nodes)) := c.nodes;
      c := c.next;  INC (x, NUMBER (c.nodes));
    END;
    IF (s.cur > 0) THEN
      SUBARRAY (nn^, x, s.cur) := SUBARRAY (c.nodes, 0, s.cur);
    END;
    RETURN nn;
  END FlattenChunks;

BEGIN
END M3Parse.
