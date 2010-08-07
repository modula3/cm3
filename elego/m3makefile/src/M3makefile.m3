(*--------------------------------------------------------------------------*)
MODULE M3makefile;

IMPORT Pathname, Text, TextSeq, OSError;
IMPORT File, Fmt, M3ID;
IMPORT QToken, QScanner, QCode, Quake;
IMPORT SMsg AS Msg;
IMPORT (* FSFixed AS *) FS;


(*--------------------------------------------------------------------------*)
REVEAL
  T = Public BRANDED "M3makefile type rev. 0.0" OBJECT
    mElements   : M3DeclarationList;
    mTargetName : TEXT;
    mTargetType : TargetType;
    mUnknown    : BOOLEAN;
  OVERRIDES
    init := Init;
    imports := Imports;
    elements := Elements;
    declList := DeclList;
    decls := Declarations;
    targetName := GetTargetName;
    targetType := GetTargetType;
    unknownElements := UnknownElements;
  END;

(*--------------------------------------------------------------------------*)
PROCEDURE Copy(self, from : M3DeclarationList) : M3DeclarationList =
  BEGIN
    self.type := from.type;
    self.name := from.name;
    self.args := from.args;
    self.next := from.next;
    RETURN self;
  END Copy;

(*--------------------------------------------------------------------------*)
PROCEDURE New(fn : Pathname.T) : T RAISES {OSError.E, ParseError} =
  BEGIN
    RETURN NEW(T).init(fn);
  END New;

(*--------------------------------------------------------------------------*)
TYPE
  TK = QToken.T;
  QC = QCode.Op;

(*--------------------------------------------------------------------------*)
TYPE
  State = RECORD
    lexer : QScanner.T;
    file  : M3ID.T;
    sElements   : M3DeclarationList;
    sTargetName : TEXT;
    sTargetType : TargetType;
    sUnknown    : BOOLEAN;
    isCall      : BOOLEAN;
  END;

(*--------------------------------------------------------------------------*)
CONST
  StatementStartTokens = SET OF TK {
    TK.Foreach, TK.If, TK.Local, TK.Readonly, TK.Return,
    TK.Greater, TK.Name, TK.Proc
  };
  ExpressionStartTokens = SET OF TK {
    TK.Not, TK.Dollar, TK.LParen, TK.LSquare, TK.LBrace,
    TK.Cardinal, TK.Name, TK.String, TK.At
  };

(*--------------------------------------------------------------------------*)
PROCEDURE Init(self : T; fn : Pathname.T) : T RAISES {OSError.E, ParseError} =
  (*
    Init() is supposed to do minimal lexical and syntactial parsing of
    quake declarations, that is quake procedure calls of the form
    `func (arg1, arg2, ...)'. All arguments are considered to be of
    type text, they may or may not be (double)quoted. Alle other
    elements of the quake language (assignments, statements, procedure
    definitions, etc.) should be ignored, but they should not raise an
    exception; rather self.mUnknwon should be set. After the
    termination of Init(), self.mElements, self.mTargetType, and
    self.mTargetName should have their proper values corresponding to
    the m3makefile read in.  
  *)
  VAR
    s: State;  f: File.T;
    map := Quake.NewIDMap (Str2ID, Txt2ID, ID2Txt);
  BEGIN
    self.mUnknown := TRUE;
    self.mElements := NIL;
    f := FS.OpenFileReadonly (fn);
    TRY
      s.lexer      := NEW (QScanner.T).init (f, map);
      s.file       := M3ID.Add (fn);
      s.sElements  := NIL;
      s.sUnknown   := FALSE;
      s.isCall     := FALSE;
      s.lexer.next ();  (* prime the input symbol *)
      Msg.D("parsing...");
      WHILE s.lexer.token IN StatementStartTokens DO
        Statement (s);
      END;
      Match (s, TK.EOF);
    FINALLY
      TRY f.close (); EXCEPT OSError.E => (*ignore*) END;
    END;
    self.mElements   := s.sElements;
    self.mTargetName := s.sTargetName;
    self.mTargetType := s.sTargetType;
    self.mUnknown    := s.sUnknown;
    RETURN self;
  END Init;

(*--------------------------------------------------------------------------*)
PROCEDURE Str2ID (READONLY x: ARRAY OF CHAR): Quake.ID =
  BEGIN
    RETURN M3ID.FromStr (x);
  END Str2ID;

(*--------------------------------------------------------------------------*)
PROCEDURE Txt2ID (t: TEXT): Quake.ID =
  BEGIN
    RETURN M3ID.Add (t);
  END Txt2ID;

(*--------------------------------------------------------------------------*)
PROCEDURE ID2Txt (i: Quake.ID): TEXT =
  BEGIN
    RETURN M3ID.ToText (i);
  END ID2Txt;

(*--------------------------------------------------------------------------*)
PROCEDURE Imports(self : T) : TextSeq.T =
  VAR
    res := NEW(TextSeq.T).init();
    m := self.mElements;
  BEGIN
    WHILE m # NIL DO
      IF Text.Equal(m.type, "import") THEN
        res.addhi(m.name);
      END;
      m := m.next;
    END;
    RETURN res;
  END Imports;

(*--------------------------------------------------------------------------*)
PROCEDURE Elements(self : T) : M3DeclarationList =
  BEGIN
    RETURN self.mElements;
  END Elements;

(*--------------------------------------------------------------------------*)
PROCEDURE DeclList(self : T; type : TEXT) : M3DeclarationList =
  VAR
    res : M3DeclarationList := NIL;
    act : M3DeclarationList := NIL;
    m := self.mElements;
  BEGIN
    WHILE m # NIL DO
      IF Text.Equal(m.type, type) THEN
        IF act = NIL THEN
          res := NEW(M3DeclarationList).copy(m);
          act := res;
        ELSE
          act.next := NEW(M3DeclarationList).copy(m);
          act := act.next;
        END;
        act.next := NIL;
      END;
      m := m.next;
    END;
    RETURN res;
  END DeclList;

(*--------------------------------------------------------------------------*)
PROCEDURE Declarations(self : T; type : TEXT) : TextSeq.T =
  VAR
    res := NEW(TextSeq.T).init();
    m := self.mElements;
  BEGIN
    WHILE m # NIL DO
      IF Text.Equal(m.type, type) THEN
        res.addhi(m.name);
      END;
      m := m.next;
    END;
    RETURN res;
  END Declarations;

(*--------------------------------------------------------------------------*)
PROCEDURE GetTargetName(self : T) : TEXT =
  BEGIN
    RETURN self.mTargetName;
  END GetTargetName;

(*--------------------------------------------------------------------------*)
PROCEDURE GetTargetType(self : T) : TargetType = 
  BEGIN
    RETURN self.mTargetType;
  END GetTargetType;

(*--------------------------------------------------------------------------*)
PROCEDURE UnknownElements(self : T) : BOOLEAN =
  BEGIN
    RETURN self.mUnknown;
  END UnknownElements;

(*--------------------------------------------------------------------------*)
PROCEDURE Block (VAR s: State) RAISES {ParseError} =
  BEGIN
    Msg.D("  block");
    (* s.code.emit (QC.PushScope, 0); *)
    WHILE s.lexer.token IN StatementStartTokens DO
      Statement (s);
    END;
    (* s.code.emit (QC.PopScope, 0); *)
  END Block;

(*--------------------------------------------------------------------------*)
PROCEDURE Statement (VAR s: State) RAISES {ParseError} =
  BEGIN
    Msg.D("  statement");
    (* s.code.emit (QC.SetLine, s.lexer.line); *)
    CASE s.lexer.token OF
    | TK.Foreach   => ForeachStmt (s);
    | TK.If        => IfStmt (s);
    | TK.Local     => Decl (s);
    | TK.Readonly  => Decl (s);
    | TK.Proc      => ProcDeclStmt (s, FALSE, FALSE);
    | TK.Name      => AssignOrProcCallStmt (s, FALSE, FALSE);
    | TK.Return    => ReturnStmt (s);
    | TK.Greater   => RedirectionStmt (s);
    ELSE <* ASSERT FALSE *>(* StatementStartTokens is wrong! *)
    END;
  END Statement;

(*--------------------------------------------------------------------------*)
PROCEDURE Decl (VAR s: State) RAISES {ParseError} =
  VAR local, readonly := FALSE;
  BEGIN
    Msg.D("  decl");
    LOOP
      IF    s.lexer.token = TK.Local    THEN local := TRUE;
      ELSIF s.lexer.token = TK.Readonly THEN readonly := TRUE;
      ELSE  EXIT; (* LOOP *)
      END;
      s.lexer.next ();
    END;

    IF s.lexer.token = TK.Proc THEN
      ProcDeclStmt (s, local, readonly);
    ELSIF s.lexer.token = TK.Name THEN
      AssignOrProcCallStmt (s, local, readonly);
    ELSE
      Err (s, "\"proc\", \"func\", or a name expected after \"readonly\" or \"local\"");
    END;
  END Decl;

(*--------------------------------------------------------------------------*)
PROCEDURE ForeachStmt (VAR s: State) RAISES {ParseError} =
  VAR id: M3ID.T;  (* top, bot: INTEGER; *)
  BEGIN
    Msg.D("  foreach stmt");
    s.sUnknown := TRUE;
    Match (s, TK.Foreach);
    id := MatchName (s);
    Match (s, TK.In);
    Expression (s);
(*       s.code.emit (QC.InitForeach, id); *)
(*       top := s.code.cursor; *)
(*       s.code.emit (QC.NextForeach, 0); *)
      Block (s);
(*       bot := s.code.cursor; *)
(*       s.code.emit  (QC.Goto, top - bot - 1); *)
(*       s.code.patch (top,  QC.NextForeach, bot - top); *)
    Match (s, TK.End);
  END ForeachStmt;

(*--------------------------------------------------------------------------*)
PROCEDURE IfStmt (VAR s: State) RAISES {ParseError} =
(*   VAR test, exit: INTEGER; *)
  BEGIN
    Msg.D("  if stmt");
    s.sUnknown := TRUE;
    Match (s, TK.If);
    Expression (s);
(*     test := s.code.cursor; *)
(*     s.code.emit (QC.IfFalse, 0); *)
    Block (s);
(*     exit := s.code.cursor; *)
    IF (s.lexer.token = TK.End) THEN
(*       s.code.patch (test, QC.IfFalse, exit - test - 1); *)
      Match (s, TK.End);
    ELSIF (s.lexer.token = TK.Else) THEN
(*       s.code.emit (QC.Goto, 0); *)
(*       s.code.patch (test, QC.IfFalse, exit - test); *)
      Match (s, TK.Else);
      Block (s);
(*       s.code.patch (exit, QC.Goto, s.code.cursor - exit - 1); *)
      Match (s, TK.End);
    ELSE
      Err (s, "\"end\" or \"else\" expected after \"if\"");
    END;
  END IfStmt;

(*--------------------------------------------------------------------------*)
PROCEDURE ReturnStmt (VAR s: State) RAISES {ParseError} =
  BEGIN
    Msg.D("  return stmt");
    s.sUnknown := TRUE;
    Match (s, TK.Return);
    IF s.lexer.token IN ExpressionStartTokens THEN
      Expression (s);
(*       s.code.emit (QC.ReturnValue, 0); *)
    ELSE
(*       s.code.emit (QC.Return, 0); *)
    END;
  END ReturnStmt;

(*--------------------------------------------------------------------------*)
PROCEDURE RedirectionStmt (VAR s: State) RAISES {ParseError} =
  VAR op := QC.StartRedirect;
  BEGIN
    Msg.D("  redirection stmt");
    s.sUnknown := TRUE;
    Match (s, TK.Greater);
    IF s.lexer.token = TK.Greater THEN
      Match (s, TK.Greater);
      op := QC.StartAppendRedirect;
    END;
    Expression (s);
    Match (s, TK.In);
(*     s.code.emit (op, 0); *)
    Block (s);
(*     s.code.emit (QC.EndRedirect, 0); *)
    Match (s, TK.End);
  END RedirectionStmt;

(*--------------------------------------------------------------------------*)
PROCEDURE ProcDeclStmt (VAR s: State; local, readonly: BOOLEAN)
  RAISES {ParseError}=
  VAR
    n_formals := 0;
    formals   : ARRAY [0..19] OF M3ID.T;
(*     push      : INTEGER; *)
(*     goto      : INTEGER; *)
(*     entry     : INTEGER; *)
    id        : M3ID.T;
(*     proc_id   : INTEGER; *)
  BEGIN
    Msg.D("  proc decl stmt");
    s.sUnknown := TRUE;
    Match (s, TK.Proc);
    id := MatchName (s);

    (* define the symbol at runtime *)
(*     push := s.code.cursor; *)
(*     s.code.emit (QC.PushProc, 0); *)
    EmitDefine (s, id, local, readonly);

    (* skip over the procedure's body *)
(*     goto := s.code.cursor; *)
(*     s.code.emit (QC.Goto, 0); *)

(*     proc_id := s.code.add_proc (id); *)
(*     s.code.patch (push, QC.PushProc, proc_id); *)
(*     entry := s.code.cursor; *)
(*     s.code.emit (QC.SetLine, s.lexer.line); *)

    (* parse the formals *)
    Match (s, TK.LParen);
    WHILE (s.lexer.token = TK.Name) DO
      IF (n_formals >= NUMBER (formals)) THEN
        Err (s, "too many formal parameters");
      END;
      formals [n_formals] := s.lexer.string;
      INC (n_formals);
      s.lexer.next ();
      IF s.lexer.token # TK.Comma THEN EXIT; END;
      Match (s, TK.Comma);
    END;
    Match (s, TK.RParen);
    Match (s, TK.Is);
(*     s.code.procs [proc_id].n_args := n_formals; *)

    (* define the formal parameters *)
    FOR i := n_formals-1 TO 0 BY -1 DO
      EmitDefine (s, formals[i], local := TRUE, readonly := FALSE);
    END;

    Block (s);
(*     s.code.emit (QC.SetLine, s.lexer.line); *)
    Match (s, TK.End);
(*     s.code.emit (QC.Return, 0); *)

    (* fix the branch around the body *)
(*     s.code.patch (goto, QC.Goto, s.code.cursor - entry); *)
  END ProcDeclStmt;

(*--------------------------------------------------------------------------*)
PROCEDURE EmitDefine (<*UNUSED*> VAR s: State;  
                      <*UNUSED*> id: M3ID.T;
                      <*UNUSED*> local, readonly: BOOLEAN) =
(*   TYPE Z = ARRAY BOOLEAN OF QC; *)
(*   CONST ZZ = ARRAY BOOLEAN OF Z { Z {QC.DefineG, QC.DefineGR}, *)
(*                                   Z {QC.DefineL, QC.DefineLR} }; *)
  BEGIN
(*     s.code.emit (ZZ[local][readonly], id); *)
  END EmitDefine;

(*--------------------------------------------------------------------------*)
PROCEDURE AssignOrProcCallStmt (VAR s: State; local, readonly: BOOLEAN)
  RAISES {ParseError} =
  VAR 
    pcall : M3DeclarationList;
    id := MatchName (s);  op: QC;  arg: INTEGER;
  BEGIN
    Msg.D("  assign or proc call stmt");
    IF s.lexer.token = TK.LParen THEN
      (* It's a procedure call *)
      IF local OR readonly THEN
        Err (s, "\"local\" and \"readonly\" not valid before a procedure call");
      END;
(*       s.code.emit (QC.LoadVar, id); *)
(*       s.code.emit (QC.StartCall, 0); *)
      s.isCall := TRUE;
      pcall := NEW(M3DeclarationList);
      pcall.args := NEW(TextSeq.T).init();
      pcall.next := s.sElements;
      pcall.type := M3ID.ToText(s.lexer.string);
      s.sElements := pcall; 
      Match (s, TK.LParen);
(*       s.code.emit (QC.CallProc, ExprList (s)); *)
      EVAL ExprList(s);
      Match (s, TK.RParen);
      s.isCall := FALSE;
      IF Text.Equal(pcall.type, "program") THEN
        s.sTargetName := pcall.name;
        s.sTargetType := TargetType.Program;
      ELSIF Text.Equal(pcall.type, "Program") THEN
        s.sTargetName := pcall.name;
        s.sTargetType := TargetType.Program;
      ELSIF Text.Equal(pcall.type, "library") THEN
        s.sTargetName := pcall.name;
        s.sTargetType := TargetType.Library;
      ELSIF Text.Equal(pcall.type, "Library") THEN
        s.sTargetName := pcall.name;
        s.sTargetType := TargetType.Library;
      END;
    ELSIF (local) OR (readonly) THEN
      s.sUnknown := TRUE; (* FIXME *)
      Match (s, TK.Equal);
      Expression (s);
      EmitDefine (s, id, local, readonly);
    ELSE
      (* It's an lvalue *)
      s.sUnknown := TRUE; (* FIXME *)
      op := QC.LoadVar;  arg := id;
      LOOP
        IF (s.lexer.token = TK.LSquare) THEN
(*           s.code.emit (op, arg); *)
          Match (s, TK.LSquare);
          Expression (s);
          Match (s, TK.RSquare);
          op := QC.SubscriptArray;  arg := 0;
        ELSIF (s.lexer.token = TK.LBrace) THEN
(*           s.code.emit (op, arg); *)
          Match (s, TK.LBrace);
          Expression (s);
          Match (s, TK.RBrace);
          op := QC.IndexTable;  arg := 0; 
        ELSE EXIT;
        END;
      END;
      IF s.lexer.token = TK.Plus THEN
(*         s.code.emit (op, arg); *)
        Match (s, TK.Plus);               op := QC.Append;  arg := 0;
      ELSIF (op = QC.LoadVar)        THEN op := QC.Assign;
      ELSIF (op = QC.SubscriptArray) THEN op := QC.AssignArray;
      ELSE (*op = QC.IndexTable*)         op := QC.AssignTable;
      END;
      Match (s, TK.Equal);
      Expression (s);
(*       s.code.emit (op, arg); *)
    END;
  END AssignOrProcCallStmt;

(*----------------------------------------------------------- expressions ---*)

(*--------------------------------------------------------------------------*)
PROCEDURE ExprList (VAR s: State): INTEGER RAISES {ParseError} =
  VAR cnt := 0;
  BEGIN
    Msg.D("  expr list");
    IF s.lexer.token = TK.Name OR s.lexer.token = TK.String THEN
      IF s.isCall THEN
        s.sElements.name := M3ID.ToText(s.lexer.string); 
      END;
    END;
    WHILE (s.lexer.token IN ExpressionStartTokens) DO
      Expression (s);
      INC (cnt);
      IF s.lexer.token # TK.Comma THEN EXIT; END;
      s.lexer.next ();
    END;
    RETURN cnt;
  END ExprList;

(*--------------------------------------------------------------------------*)
PROCEDURE Expression (VAR s: State) RAISES {ParseError} =
  BEGIN
    Msg.D("  expr");
    E1 (s);
    WHILE s.lexer.token = TK.Or DO
      Match (s, TK.Or);
      E1 (s);
(*       s.code.emit (QC.Or, 0); *)
    END;
  END Expression;

(*--------------------------------------------------------------------------*)
PROCEDURE E1 (VAR s: State) RAISES {ParseError} =
  BEGIN
    E2 (s);
    WHILE s.lexer.token = TK.And DO
      Match (s, TK.And);
      E2 (s);
(*       s.code.emit (QC.And, 0); *)
    END;
  END E1;

(*--------------------------------------------------------------------------*)
PROCEDURE E2 (VAR s: State) RAISES {ParseError} =
  VAR n := 0;
  BEGIN
    WHILE s.lexer.token = TK.Not DO Match (s, TK.Not); INC(n); END;
    E3 (s);
    IF (n MOD 2) = 1 THEN (* s.code.emit (QC.Not, 0)  *)END;
  END E2;

(*--------------------------------------------------------------------------*)
PROCEDURE E3 (VAR s: State) RAISES {ParseError} =
  BEGIN
    E4 (s);
    WHILE s.lexer.token = TK.Contains DO
      Match (s, TK.Contains);
      E4 (s);
(*       s.code.emit (QC.IsMember, 0); *)
    END;
  END E3;

(*--------------------------------------------------------------------------*)
PROCEDURE E4 (VAR s: State) RAISES {ParseError} =
  BEGIN
    E5 (s);
    WHILE s.lexer.token = TK.Ampersand DO
      Match (s, TK.Ampersand);
      E5 (s);
(*       s.code.emit (QC.Concat, 0); *)
    END;
  END E4;

(*--------------------------------------------------------------------------*)
PROCEDURE E5 (VAR s: State) RAISES {ParseError} =
  BEGIN
    E6 (s);
    LOOP
      IF (s.lexer.token = TK.LBrace) THEN
        Match (s, TK.LBrace);
        Expression (s);
        Match (s, TK.RBrace);
(*         s.code.emit (QC.IndexTable, 0); *)
      ELSIF (s.lexer.token = TK.LSquare) THEN
        Match (s, TK.LSquare);
        Expression (s);
        Match (s, TK.RSquare);
(*         s.code.emit (QC.SubscriptArray, 0); *)
      ELSIF (s.lexer.token = TK.LParen) THEN
(*         s.code.emit (QC.StartCall, 0); *)
        Match (s, TK.LParen);
(*         s.code.emit (QC.CallFunc, ExprList (s)); *)
        EVAL ExprList(s);
        Match (s, TK.RParen);
      ELSE EXIT;
      END;
    END;
  END E5;

(*--------------------------------------------------------------------------*)
PROCEDURE E6 (VAR s: State) RAISES {ParseError} =
  BEGIN
    CASE s.lexer.token OF
    | TK.Name =>
(*         s.code.emit (QC.LoadVar, s.lexer.string); *)
        IF s.sElements # NIL AND s.sElements.args # NIL THEN
          s.sElements.args.addhi(M3ID.ToText(s.lexer.string));
        END;
        s.lexer.next ();
    | TK.Dollar =>
        Match (s, TK.Dollar);
(*         s.code.emit (QC.GetEnv, MatchName (s)); *)
        EVAL MatchName(s);
    | TK.String =>
(*         s.code.emit (QC.String, s.lexer.string); *)
        IF s.sElements # NIL AND s.sElements.args # NIL THEN
          s.sElements.args.addhi(M3ID.ToText(s.lexer.string));
        END;
        s.lexer.next ();
    | TK.Cardinal =>
(*         s.code.emit (QC.Integer, s.lexer.cardinal); *)
        s.lexer.next ();
    | TK.LParen =>
        Match (s, TK.LParen);
        Expression (s);
        Match (s, TK.RParen);
    | TK.LSquare =>
        Match (s, TK.LSquare);
(*         s.code.emit (QC.BuildArray, ExprList (s)); *)
        EVAL ExprList(s);
        Match (s, TK.RSquare);
    | TK.LBrace =>
        Match (s, TK.LBrace);
(*         s.code.emit (QC.BuildTable, KeyValueList (s)); *)
        EVAL KeyValueList(s);
        Match (s, TK.RBrace);
    ELSE Err (s, "expected expression");
    END;
  END E6;

(*--------------------------------------------------------------------------*)
PROCEDURE KeyValueList (VAR s: State): INTEGER RAISES {ParseError} =
  VAR cnt := 0;
  BEGIN
    Msg.D("  key value list");
    WHILE (s.lexer.token IN ExpressionStartTokens) DO
      Expression (s);
      INC(cnt);
      IF s.lexer.token = TK.Colon THEN
        Match (s, TK.Colon);
        Expression (s);
      ELSE
        (* Pad the stack to get a even number of elements *)
(*         s.code.emit (QC.Integer, 0); *)
      END;
      INC (cnt);
      IF s.lexer.token # TK.Comma THEN EXIT; END;
      s.lexer.next ();
    END;
    RETURN cnt;
  END KeyValueList;

(*------------------------------------------------------------------ misc ---*)

(*--------------------------------------------------------------------------*)
PROCEDURE MatchName (VAR s: State): M3ID.T RAISES {ParseError} =
  VAR nm := s.lexer.string;
  BEGIN
    Match (s, TK.Name);
    RETURN nm;
  END MatchName;

(*--------------------------------------------------------------------------*)
PROCEDURE Match (VAR s: State;  tok: TK) RAISES {ParseError} =
  BEGIN
    Msg.D("  match");
    IF s.lexer.token # tok THEN
      Err (s, Fmt.F("missing: %s (found: %s)",
                    QToken.Name[tok],
                    QToken.Name[s.lexer.token]));
    END;
    s.lexer.next();
  END Match;

(*--------------------------------------------------------------------------*)
PROCEDURE Err (VAR s: State;  msg: TEXT) RAISES {ParseError} =
  BEGIN
    msg := Fmt.F ("%s, line %s: syntax error: %s",
                   M3ID.ToText (s.file), Fmt.Int (s.lexer.line), msg);
    RAISE ParseError (msg);
  END Err;

(*--------------------------------------------------------------------------*)
BEGIN
END M3makefile.
