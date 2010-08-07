(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Feb 21 16:53:00 PST 1995 by kalsow     *)
(*      modified on Fri Apr  1 13:49:15 PST 1994 by harrison   *)

MODULE QCompiler;

IMPORT File, Fmt, M3ID, FS, OSError;
IMPORT QToken, QScanner, QCode;
FROM Quake IMPORT Error;

TYPE
  TK = QToken.T;
  QC = QCode.Op;

TYPE
  State = RECORD
    lexer : QScanner.T;
    code  : QCode.Stream;
    file  : M3ID.T;
  END;

CONST
  StatementStartTokens = SET OF TK {
    TK.Foreach, TK.If, TK.Local, TK.Readonly, TK.Return,
    TK.Greater, TK.Name, TK.Proc
  };
  ExpressionStartTokens = SET OF TK {
    TK.Not, TK.Dollar, TK.LParen, TK.LSquare, TK.LBrace,
    TK.Cardinal, TK.Name, TK.String, TK.At
  };

PROCEDURE CompileFile (path: TEXT): QCode.Stream  RAISES {Error} =
  VAR s: State;  f: File.T;
  BEGIN
    TRY
      f := FS.OpenFileReadonly (path);
    EXCEPT OSError.E =>
      RAISE Error ("unable to open \"" & path & "\" for reading");
    END;

    TRY
      s.lexer      := NEW (QScanner.T).init (f);
      s.file       := M3ID.Add (path);
      s.code       := NEW (QCode.Stream, source_file := s.file);

      s.code.emit (QC.SetLine, 1);
      s.lexer.next ();  (* prime the input symbol *)
      WHILE s.lexer.token IN StatementStartTokens DO
        Statement (s);
      END;
      Match (s, TK.EOF);
      s.code.emit (QC.Halt, 0);
    FINALLY
      TRY f.close (); EXCEPT OSError.E => (*ignore*) END;
    END;

    
    RETURN s.code;
  END CompileFile;

PROCEDURE Block (VAR s: State) RAISES {Error} =
  BEGIN
    s.code.emit (QC.PushScope, 0);
    WHILE s.lexer.token IN StatementStartTokens DO
      Statement (s);
    END;
    s.code.emit (QC.PopScope, 0);
  END Block;

PROCEDURE Statement (VAR s: State) RAISES {Error} =
  BEGIN
    s.code.emit (QC.SetLine, s.lexer.line);
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

PROCEDURE Decl (VAR s: State) RAISES {Error} =
  VAR local, readonly := FALSE;
  BEGIN
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

PROCEDURE ForeachStmt (VAR s: State) RAISES {Error} =
  VAR id: M3ID.T;  top, bot: INTEGER;
  BEGIN
    Match (s, TK.Foreach);
    id := MatchName (s);
    Match (s, TK.In);
    Expression (s);
      s.code.emit (QC.InitForeach, id);
      top := s.code.cursor;
      s.code.emit (QC.NextForeach, 0);
      Block (s);
      bot := s.code.cursor;
      s.code.emit  (QC.Goto, top - bot - 1);
      s.code.patch (top,  QC.NextForeach, bot - top);
    Match (s, TK.End);
  END ForeachStmt;

PROCEDURE IfStmt (VAR s: State) RAISES {Error} =
  VAR test, exit: INTEGER;
  BEGIN
    Match (s, TK.If);
    Expression (s);
    test := s.code.cursor;
    s.code.emit (QC.IfFalse, 0);
    Block (s);
    exit := s.code.cursor;
    IF (s.lexer.token = TK.End) THEN
      s.code.patch (test, QC.IfFalse, exit - test - 1);
      Match (s, TK.End);
    ELSIF (s.lexer.token = TK.Else) THEN
      s.code.emit (QC.Goto, 0);
      s.code.patch (test, QC.IfFalse, exit - test);
      Match (s, TK.Else);
      Block (s);
      s.code.patch (exit, QC.Goto, s.code.cursor - exit - 1);
      Match (s, TK.End);
    ELSE
      Err (s, "\"end\" or \"else\" expected after \"if\"");
    END;
  END IfStmt;

PROCEDURE ReturnStmt (VAR s: State) RAISES {Error} =
  BEGIN
    Match (s, TK.Return);
    IF s.lexer.token IN ExpressionStartTokens THEN
      Expression (s);
      s.code.emit (QC.ReturnValue, 0);
    ELSE
      s.code.emit (QC.Return, 0);
    END;
  END ReturnStmt;

PROCEDURE RedirectionStmt (VAR s: State) RAISES {Error} =
  VAR op := QC.StartRedirect;
  BEGIN
    Match (s, TK.Greater);
    IF s.lexer.token = TK.Greater THEN
      Match (s, TK.Greater);
      op := QC.StartAppendRedirect;
    END;
    Expression (s);
    Match (s, TK.In);
    s.code.emit (op, 0);
    Block (s);
    s.code.emit (QC.EndRedirect, 0);
    Match (s, TK.End);
  END RedirectionStmt;

PROCEDURE ProcDeclStmt (VAR s: State; local, readonly: BOOLEAN)
  RAISES {Error}=
  VAR
    n_formals := 0;
    formals   : ARRAY [0..19] OF M3ID.T;
    push      : INTEGER;
    goto      : INTEGER;
    entry     : INTEGER;
    id        : M3ID.T;
    proc_id   : INTEGER;
  BEGIN
    Match (s, TK.Proc);
    id := MatchName (s);

    (* define the symbol at runtime *)
    push := s.code.cursor;
    s.code.emit (QC.PushProc, 0);
    EmitDefine (s, id, local, readonly);

    (* skip over the procedure's body *)
    goto := s.code.cursor;
    s.code.emit (QC.Goto, 0);

    proc_id := s.code.add_proc (id);
    s.code.patch (push, QC.PushProc, proc_id);
    entry := s.code.cursor;
    s.code.emit (QC.SetLine, s.lexer.line);

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
    s.code.procs [proc_id].n_args := n_formals;

    (* define the formal parameters *)
    FOR i := n_formals-1 TO 0 BY -1 DO
      EmitDefine (s, formals[i], local := TRUE, readonly := FALSE);
    END;

    Block (s);
    s.code.emit (QC.SetLine, s.lexer.line);
    Match (s, TK.End);
    s.code.emit (QC.Return, 0);

    (* fix the branch around the body *)
    s.code.patch (goto, QC.Goto, s.code.cursor - entry);
  END ProcDeclStmt;

PROCEDURE EmitDefine (VAR s: State;  id: M3ID.T;  local, readonly: BOOLEAN) =
  TYPE Z = ARRAY BOOLEAN OF QC;
  CONST ZZ = ARRAY BOOLEAN OF Z { Z {QC.DefineG, QC.DefineGR},
                                  Z {QC.DefineL, QC.DefineLR} };
  BEGIN
    s.code.emit (ZZ[local][readonly], id);
  END EmitDefine;

PROCEDURE AssignOrProcCallStmt (VAR s: State; local, readonly: BOOLEAN)
  RAISES {Error} =
  VAR id := MatchName (s);  op: QC;  arg: INTEGER;
  BEGIN
    IF s.lexer.token = TK.LParen THEN
      (* It's a procedure call *)
      IF local OR readonly THEN
        Err (s, "\"local\" and \"readonly\" not valid before a procedure call");
      END;
      s.code.emit (QC.LoadVar, id);
      s.code.emit (QC.StartCall, 0);
      Match (s, TK.LParen);
      s.code.emit (QC.CallProc, ExprList (s));
      Match (s, TK.RParen);
    ELSIF (local) OR (readonly) THEN
      Match (s, TK.Equal);
      Expression (s);
      EmitDefine (s, id, local, readonly);
    ELSE
      (* It's an lvalue *)
      op := QC.LoadVar;  arg := id;
      LOOP
        IF (s.lexer.token = TK.LSquare) THEN
          s.code.emit (op, arg);
          Match (s, TK.LSquare);
          Expression (s);
          Match (s, TK.RSquare);
          op := QC.SubscriptArray;  arg := 0;
        ELSIF (s.lexer.token = TK.LBrace) THEN
          s.code.emit (op, arg);
          Match (s, TK.LBrace);
          Expression (s);
          Match (s, TK.RBrace);
          op := QC.IndexTable;  arg := 0;
        ELSE EXIT;
        END;
      END;
      IF s.lexer.token = TK.Plus THEN
        s.code.emit (op, arg);
        Match (s, TK.Plus);               op := QC.Append;  arg := 0;
      ELSIF (op = QC.LoadVar)        THEN op := QC.Assign;
      ELSIF (op = QC.SubscriptArray) THEN op := QC.AssignArray;
      ELSE (*op = QC.IndexTable*)         op := QC.AssignTable;
      END;
      Match (s, TK.Equal);
      Expression (s);
      s.code.emit (op, arg);
    END;
  END AssignOrProcCallStmt;

(*----------------------------------------------------------- expressions ---*)

PROCEDURE ExprList (VAR s: State): INTEGER RAISES {Error} =
  VAR cnt := 0;
  BEGIN
    WHILE (s.lexer.token IN ExpressionStartTokens) DO
      Expression (s);
      INC (cnt);
      IF s.lexer.token # TK.Comma THEN EXIT; END;
      s.lexer.next ();
    END;
    RETURN cnt;
  END ExprList;

PROCEDURE Expression (VAR s: State) RAISES {Error} =
  BEGIN
    E1 (s);
    WHILE s.lexer.token = TK.Or DO
      Match (s, TK.Or);
      E1 (s);
      s.code.emit (QC.Or, 0);
    END;
  END Expression;

PROCEDURE E1 (VAR s: State) RAISES {Error} =
  BEGIN
    E2 (s);
    WHILE s.lexer.token = TK.And DO
      Match (s, TK.And);
      E2 (s);
      s.code.emit (QC.And, 0);
    END;
  END E1;

PROCEDURE E2 (VAR s: State) RAISES {Error} =
  VAR n := 0;
  BEGIN
    WHILE s.lexer.token = TK.Not DO Match (s, TK.Not); INC(n); END;
    E3 (s);
    IF (n MOD 2) = 1 THEN s.code.emit (QC.Not, 0) END;
  END E2;

PROCEDURE E3 (VAR s: State) RAISES {Error} =
  BEGIN
    E4 (s);
    WHILE s.lexer.token = TK.Contains DO
      Match (s, TK.Contains);
      E4 (s);
      s.code.emit (QC.IsMember, 0);
    END;
  END E3;

PROCEDURE E4 (VAR s: State) RAISES {Error} =
  BEGIN
    E5 (s);
    WHILE s.lexer.token = TK.Ampersand DO
      Match (s, TK.Ampersand);
      E5 (s);
      s.code.emit (QC.Concat, 0);
    END;
  END E4;

PROCEDURE E5 (VAR s: State) RAISES {Error} =
  BEGIN
    E6 (s);
    LOOP
      IF (s.lexer.token = TK.LBrace) THEN
        Match (s, TK.LBrace);
        Expression (s);
        Match (s, TK.RBrace);
        s.code.emit (QC.IndexTable, 0);
      ELSIF (s.lexer.token = TK.LSquare) THEN
        Match (s, TK.LSquare);
        Expression (s);
        Match (s, TK.RSquare);
        s.code.emit (QC.SubscriptArray, 0);
      ELSIF (s.lexer.token = TK.LParen) THEN
        s.code.emit (QC.StartCall, 0);
        Match (s, TK.LParen);
        s.code.emit (QC.CallFunc, ExprList (s));
        Match (s, TK.RParen);
      ELSE EXIT;
      END;
    END;
  END E5;

PROCEDURE E6 (VAR s: State) RAISES {Error} =
  BEGIN
    CASE s.lexer.token OF
    | TK.Name =>
        s.code.emit (QC.LoadVar, s.lexer.string);
        s.lexer.next ();
    | TK.Dollar =>
        Match (s, TK.Dollar);
        s.code.emit (QC.GetEnv, MatchName (s));
    | TK.String =>
        s.code.emit (QC.String, s.lexer.string);
        s.lexer.next ();
    | TK.Cardinal =>
        s.code.emit (QC.Integer, s.lexer.cardinal);
        s.lexer.next ();
    | TK.LParen =>
        Match (s, TK.LParen);
        Expression (s);
        Match (s, TK.RParen);
    | TK.LSquare =>
        Match (s, TK.LSquare);
        s.code.emit (QC.BuildArray, ExprList (s));
        Match (s, TK.RSquare);
    | TK.LBrace =>
        Match (s, TK.LBrace);
        s.code.emit (QC.BuildTable, KeyValueList (s));
        Match (s, TK.RBrace);
    ELSE Err (s, "expected expression");
    END;
  END E6;

PROCEDURE KeyValueList (VAR s: State): INTEGER RAISES {Error} =
  VAR cnt := 0;
  BEGIN
    WHILE (s.lexer.token IN ExpressionStartTokens) DO
      Expression (s);
      INC(cnt);
      IF s.lexer.token = TK.Colon THEN
        Match (s, TK.Colon);
        Expression (s);
      ELSE
        (* Pad the stack to get a even number of elements *)
        s.code.emit (QC.Integer, 0);
      END;
      INC (cnt);
      IF s.lexer.token # TK.Comma THEN EXIT; END;
      s.lexer.next ();
    END;
    RETURN cnt;
  END KeyValueList;

(*------------------------------------------------------------------ misc ---*)

PROCEDURE MatchName (VAR s: State): M3ID.T RAISES {Error} =
  VAR nm := s.lexer.string;
  BEGIN
    Match (s, TK.Name);
    RETURN nm;
  END MatchName;

PROCEDURE Match (VAR s: State;  tok: TK) RAISES {Error} =
  BEGIN
    IF s.lexer.token # tok THEN
      Err (s, Fmt.F("missing: %s (found: %s)",
                    QToken.Name[tok],
                    QToken.Name[s.lexer.token]));
    END;
    s.lexer.next();
  END Match;

PROCEDURE Err (VAR s: State;  msg: TEXT) RAISES {Error} =
  BEGIN
    msg := Fmt.F ("%s, line %s: syntax error: %s",
                   M3ID.ToText (s.file), Fmt.Int (s.lexer.line), msg);
    RAISE Error (msg);
  END Err;

BEGIN
END QCompiler.
