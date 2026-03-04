(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

(* Recursive-descent parser for the ESC specification sub-language.

   The grammar is reconstructed from surviving annotated M3 files:
   - Fmt.i3: simple ENSURES RES # NIL
   - Sequence.ig: full procedure specs with REQUIRES/ENSURES/MODIFIES,
     ALL quantifiers, primed variables, NUMBER, FRESH
   - SequenceRep.ig: ABSTRACT, DEPEND, FUNC, AXIOM
   - Thread.i3: set operations (MEMBER, INSERT, DELETE, sup),
     partial order (MUT_LT, MUT_LE), LL ghost variable
   - Simplex.m3: INVARIANT, REP, complex invariants

   The parser consumes the pragma text (after the SPEC keyword)
   character by character via a simple scanner. *)

MODULE ESCSpecParse;

IMPORT ESCSpec, RefList, Atom, Text;

(* ---- Scanner ---- *)

TYPE
  Scanner = RECORD
    text: TEXT;
    pos: INTEGER;
    len: INTEGER;
    tok: Token;
    tokText: TEXT;      (* for identifiers, integers *)
    tokInt: INTEGER;    (* for integer literals *)
  END;

  Token = {
    Eof, Ident, Int, LParen, RParen, LBracket, RBracket,
    Comma, Colon, Dot, Prime, Hash, Eq, Neq,
    Lt, Le, Gt, Ge, Plus, Minus, Star, Caret,
    And, Or, Not, Implies, Iff,
    All, Exists,
    Requires, Ensures, Modifies,
    Res, Nil, Fresh, Number, True, False,
    Abstract, Depend, Func, Axiom, Invariant, Rep,
    Var, Map, Seq, To, If, Then, Else,
    Member, Insert, Delete, Sup,
    Concat, Subarray, Min, Max
  };

PROCEDURE ScanInit(VAR s: Scanner; t: TEXT) =
  BEGIN
    s.text := t;
    s.pos := 0;
    s.len := Text.Length(t);
    s.tok := Token.Eof;
    s.tokText := NIL;
    s.tokInt := 0;
    Advance(s);
  END ScanInit;

PROCEDURE Peek(VAR s: Scanner): CHAR =
  BEGIN
    IF s.pos >= s.len THEN RETURN '\000' END;
    RETURN Text.GetChar(s.text, s.pos);
  END Peek;

PROCEDURE SkipWhitespace(VAR s: Scanner) =
  BEGIN
    WHILE s.pos < s.len AND
          (Peek(s) = ' ' OR Peek(s) = '\t' OR Peek(s) = '\n' OR
           Peek(s) = '\r') DO
      INC(s.pos);
    END;
  END SkipWhitespace;

PROCEDURE IsAlpha(c: CHAR): BOOLEAN =
  BEGIN
    RETURN (c >= 'a' AND c <= 'z') OR (c >= 'A' AND c <= 'Z') OR c = '_';
  END IsAlpha;

PROCEDURE IsDigit(c: CHAR): BOOLEAN =
  BEGIN RETURN c >= '0' AND c <= '9'; END IsDigit;

PROCEDURE IsAlNum(c: CHAR): BOOLEAN =
  BEGIN RETURN IsAlpha(c) OR IsDigit(c); END IsAlNum;

PROCEDURE ScanIdent(VAR s: Scanner) =
  VAR start := s.pos;
  BEGIN
    WHILE s.pos < s.len AND (IsAlNum(Peek(s)) OR Peek(s) = '$') DO
      INC(s.pos);
    END;
    s.tokText := Text.Sub(s.text, start, s.pos - start);
    s.tok := LookupKeyword(s.tokText);
  END ScanIdent;

PROCEDURE LookupKeyword(t: TEXT): Token =
  BEGIN
    IF Text.Equal(t, "AND") THEN RETURN Token.And
    ELSIF Text.Equal(t, "OR") THEN RETURN Token.Or
    ELSIF Text.Equal(t, "NOT") THEN RETURN Token.Not
    ELSIF Text.Equal(t, "IMPLIES") THEN RETURN Token.Implies
    ELSIF Text.Equal(t, "IFF") THEN RETURN Token.Iff
    ELSIF Text.Equal(t, "ALL") THEN RETURN Token.All
    ELSIF Text.Equal(t, "EX") THEN RETURN Token.Exists
    ELSIF Text.Equal(t, "REQUIRES") THEN RETURN Token.Requires
    ELSIF Text.Equal(t, "ENSURES") THEN RETURN Token.Ensures
    ELSIF Text.Equal(t, "MODIFIES") THEN RETURN Token.Modifies
    ELSIF Text.Equal(t, "RES") THEN RETURN Token.Res
    ELSIF Text.Equal(t, "NIL") THEN RETURN Token.Nil
    ELSIF Text.Equal(t, "FRESH") THEN RETURN Token.Fresh
    ELSIF Text.Equal(t, "NUMBER") THEN RETURN Token.Number
    ELSIF Text.Equal(t, "TRUE") THEN RETURN Token.True
    ELSIF Text.Equal(t, "FALSE") THEN RETURN Token.False
    ELSIF Text.Equal(t, "ABSTRACT") THEN RETURN Token.Abstract
    ELSIF Text.Equal(t, "DEPEND") THEN RETURN Token.Depend
    ELSIF Text.Equal(t, "FUNC") THEN RETURN Token.Func
    ELSIF Text.Equal(t, "AXIOM") THEN RETURN Token.Axiom
    ELSIF Text.Equal(t, "INVARIANT") THEN RETURN Token.Invariant
    ELSIF Text.Equal(t, "REP") THEN RETURN Token.Rep
    ELSIF Text.Equal(t, "VAR") THEN RETURN Token.Var
    ELSIF Text.Equal(t, "MAP") THEN RETURN Token.Map
    ELSIF Text.Equal(t, "SEQ") THEN RETURN Token.Seq
    ELSIF Text.Equal(t, "TO") THEN RETURN Token.To
    ELSIF Text.Equal(t, "IF") THEN RETURN Token.If
    ELSIF Text.Equal(t, "THEN") THEN RETURN Token.Then
    ELSIF Text.Equal(t, "ELSE") THEN RETURN Token.Else
    ELSIF Text.Equal(t, "MEMBER") THEN RETURN Token.Member
    ELSIF Text.Equal(t, "INSERT") THEN RETURN Token.Insert
    ELSIF Text.Equal(t, "DELETE") THEN RETURN Token.Delete
    ELSIF Text.Equal(t, "sup") THEN RETURN Token.Sup
    ELSIF Text.Equal(t, "CONCAT") THEN RETURN Token.Concat
    ELSIF Text.Equal(t, "SUBARRAY") THEN RETURN Token.Subarray
    ELSIF Text.Equal(t, "MIN") THEN RETURN Token.Min
    ELSIF Text.Equal(t, "MAX") THEN RETURN Token.Max
    ELSE RETURN Token.Ident
    END;
  END LookupKeyword;

PROCEDURE ScanInt(VAR s: Scanner) =
  VAR start := s.pos;
  BEGIN
    WHILE s.pos < s.len AND IsDigit(Peek(s)) DO INC(s.pos) END;
    s.tokText := Text.Sub(s.text, start, s.pos - start);
    s.tokInt := TextToInt(s.tokText);
    s.tok := Token.Int;
  END ScanInt;

PROCEDURE TextToInt(t: TEXT): INTEGER =
  VAR n := 0;
  BEGIN
    FOR i := 0 TO Text.Length(t) - 1 DO
      n := n * 10 + ORD(Text.GetChar(t, i)) - ORD('0');
    END;
    RETURN n;
  END TextToInt;

PROCEDURE Advance(VAR s: Scanner) =
  VAR c: CHAR;
  BEGIN
    SkipWhitespace(s);
    IF s.pos >= s.len THEN
      s.tok := Token.Eof;
      RETURN;
    END;
    c := Peek(s);
    IF IsAlpha(c) THEN
      ScanIdent(s);
    ELSIF IsDigit(c) THEN
      ScanInt(s);
    ELSE
      INC(s.pos);
      CASE c OF
      | '(' => s.tok := Token.LParen;
      | ')' => s.tok := Token.RParen;
      | '[' => s.tok := Token.LBracket;
      | ']' => s.tok := Token.RBracket;
      | ',' => s.tok := Token.Comma;
      | ':' => s.tok := Token.Colon;
      | '.' => s.tok := Token.Dot;
      | '\'' => s.tok := Token.Prime;
      | '+' => s.tok := Token.Plus;
      | '-' =>
        IF s.pos < s.len AND IsDigit(Peek(s)) THEN
          ScanInt(s);
          s.tokInt := -s.tokInt;
        ELSE
          s.tok := Token.Minus;
        END;
      | '*' => s.tok := Token.Star;
      | '^' => s.tok := Token.Caret;
      | '#' => s.tok := Token.Hash;
      | '=' =>
        IF s.pos < s.len AND Peek(s) = '>' THEN
          INC(s.pos);
          s.tok := Token.Implies;
        ELSE
          s.tok := Token.Eq;
        END;
      | '<' =>
        IF s.pos < s.len AND Peek(s) = '=' THEN
          INC(s.pos);
          s.tok := Token.Le;
        ELSE
          s.tok := Token.Lt;
        END;
      | '>' =>
        IF s.pos < s.len AND Peek(s) = '=' THEN
          INC(s.pos);
          s.tok := Token.Ge;
        ELSE
          s.tok := Token.Gt;
        END;
      ELSE
        s.tok := Token.Eof;  (* unknown char, treat as EOF *)
      END;
    END;
  END Advance;

PROCEDURE Expect(VAR s: Scanner; t: Token) RAISES {ParseError} =
  BEGIN
    IF s.tok # t THEN
      RAISE ParseError("expected " & TokenName(t) &
                        " but got " & TokenName(s.tok));
    END;
    Advance(s);
  END Expect;

PROCEDURE TokenName(t: Token): TEXT =
  BEGIN
    CASE t OF
    | Token.Eof => RETURN "EOF";
    | Token.Ident => RETURN "identifier";
    | Token.Int => RETURN "integer";
    | Token.LParen => RETURN "'('";
    | Token.RParen => RETURN "')'";
    | Token.LBracket => RETURN "'['";
    | Token.RBracket => RETURN "']'";
    | Token.Comma => RETURN "','";
    | Token.Colon => RETURN "':'";
    | Token.Dot => RETURN "'.'";
    | Token.Prime => RETURN "'''";
    | Token.Hash => RETURN "'#'";
    | Token.Eq => RETURN "'='";
    | Token.Neq => RETURN "'#'";
    | Token.Lt => RETURN "'<'";
    | Token.Le => RETURN "'<='";
    | Token.Gt => RETURN "'>'";
    | Token.Ge => RETURN "'>='";
    | Token.Plus => RETURN "'+'";
    | Token.Minus => RETURN "'-'";
    | Token.Star => RETURN "'*'";
    | Token.Caret => RETURN "'^'";
    | Token.And => RETURN "'AND'";
    | Token.Or => RETURN "'OR'";
    | Token.Not => RETURN "'NOT'";
    | Token.Implies => RETURN "'IMPLIES'";
    | Token.Iff => RETURN "'IFF'";
    | Token.All => RETURN "'ALL'";
    | Token.Exists => RETURN "'EX'";
    | Token.Requires => RETURN "'REQUIRES'";
    | Token.Ensures => RETURN "'ENSURES'";
    | Token.Modifies => RETURN "'MODIFIES'";
    | Token.Res => RETURN "'RES'";
    | Token.Nil => RETURN "'NIL'";
    | Token.Fresh => RETURN "'FRESH'";
    | Token.Number => RETURN "'NUMBER'";
    | Token.True => RETURN "'TRUE'";
    | Token.False => RETURN "'FALSE'";
    | Token.Abstract => RETURN "'ABSTRACT'";
    | Token.Depend => RETURN "'DEPEND'";
    | Token.Func => RETURN "'FUNC'";
    | Token.Axiom => RETURN "'AXIOM'";
    | Token.Invariant => RETURN "'INVARIANT'";
    | Token.Rep => RETURN "'REP'";
    | Token.Var => RETURN "'VAR'";
    | Token.Map => RETURN "'MAP'";
    | Token.Seq => RETURN "'SEQ'";
    | Token.To => RETURN "'TO'";
    | Token.If => RETURN "'IF'";
    | Token.Then => RETURN "'THEN'";
    | Token.Else => RETURN "'ELSE'";
    | Token.Member => RETURN "'MEMBER'";
    | Token.Insert => RETURN "'INSERT'";
    | Token.Delete => RETURN "'DELETE'";
    | Token.Sup => RETURN "'sup'";
    | Token.Concat => RETURN "'CONCAT'";
    | Token.Subarray => RETURN "'SUBARRAY'";
    | Token.Min => RETURN "'MIN'";
    | Token.Max => RETURN "'MAX'";
    END;
  END TokenName;

(* ---- Parser ---- *)

(* Parse a qualified name: "Name" or "Module.Name" or "T.method" *)
PROCEDURE ParseQualName(VAR s: Scanner; VAR prefix, name: Atom.T)
  RAISES {ParseError} =
  BEGIN
    IF s.tok # Token.Ident THEN
      RAISE ParseError("expected identifier");
    END;
    name := Atom.FromText(s.tokText);
    prefix := NIL;
    Advance(s);
    IF s.tok = Token.Dot THEN
      Advance(s);
      IF s.tok # Token.Ident THEN
        RAISE ParseError("expected identifier after '.'");
      END;
      prefix := name;
      name := Atom.FromText(s.tokText);
      Advance(s);
    END;
  END ParseQualName;

(* Parse formals list: (x, y, z) *)
PROCEDURE ParseFormals(VAR s: Scanner): RefList.T RAISES {ParseError} =
  VAR result: RefList.T := NIL;
  BEGIN
    Expect(s, Token.LParen);
    IF s.tok # Token.RParen THEN
      IF s.tok # Token.Ident THEN
        RAISE ParseError("expected formal name");
      END;
      result := RefList.List1(Atom.FromText(s.tokText));
      Advance(s);
      WHILE s.tok = Token.Comma DO
        Advance(s);
        IF s.tok # Token.Ident THEN
          RAISE ParseError("expected formal name after ','");
        END;
        result := RefList.AppendD(result,
                    RefList.List1(Atom.FromText(s.tokText)));
        Advance(s);
      END;
    END;
    Expect(s, Token.RParen);
    RETURN result;
  END ParseFormals;

(* Parse a SPEC pragma body *)
PROCEDURE ParseSpec(VAR s: Scanner): ESCSpec.Spec RAISES {ParseError} =
  BEGIN
    CASE s.tok OF
    | Token.Var =>      RETURN ParseGhostVar(s);
    | Token.Abstract => RETURN ParseAbstract(s);
    | Token.Depend =>   RETURN ParseDepend(s);
    | Token.Func =>     RETURN ParseFunc(s);
    | Token.Axiom =>    RETURN ParseAxiom(s);
    | Token.Invariant => RETURN ParseInvariant(s, ESCSpec.InvariantKind.Invariant);
    | Token.Rep =>       RETURN ParseInvariant(s, ESCSpec.InvariantKind.Rep);
    ELSE
      (* Must be a procedure spec: name(formals) clauses *)
      RETURN ParseProcSpec(s);
    END;
  END ParseSpec;

(* <*SPEC VAR name: MAP T TO U *>  or  <*SPEC VAR name: SEQ[T] *> *)
PROCEDURE ParseGhostVar(VAR s: Scanner): ESCSpec.GhostVar RAISES {ParseError} =
  VAR
    name: Atom.T;
    kind: ESCSpec.GhostVarKind;
    domType, rangeType: Atom.T := NIL;
  BEGIN
    Advance(s);  (* consume VAR *)
    IF s.tok # Token.Ident THEN RAISE ParseError("expected var name") END;
    name := Atom.FromText(s.tokText);
    Advance(s);
    Expect(s, Token.Colon);

    IF s.tok = Token.Map THEN
      Advance(s);
      kind := ESCSpec.GhostVarKind.Map;
      IF s.tok # Token.Ident THEN
        RAISE ParseError("expected domain type after MAP");
      END;
      domType := Atom.FromText(s.tokText);
      Advance(s);
      Expect(s, Token.To);
      IF s.tok # Token.Ident THEN
        RAISE ParseError("expected range type after TO");
      END;
      rangeType := Atom.FromText(s.tokText);
      Advance(s);
    ELSIF s.tok = Token.Seq THEN
      Advance(s);
      kind := ESCSpec.GhostVarKind.Seq;
      Expect(s, Token.LBracket);
      IF s.tok # Token.Ident THEN
        RAISE ParseError("expected element type in SEQ[T]");
      END;
      domType := Atom.FromText(s.tokText);
      Advance(s);
      (* Handle qualified types like Elem.T *)
      IF s.tok = Token.Dot THEN
        Advance(s);
        IF s.tok # Token.Ident THEN
          RAISE ParseError("expected type name after '.'");
        END;
        domType := Atom.FromText(Atom.ToText(domType) & "." &
                                 s.tokText);
        Advance(s);
      END;
      Expect(s, Token.RBracket);
    ELSE
      kind := ESCSpec.GhostVarKind.Plain;
      IF s.tok # Token.Ident THEN
        RAISE ParseError("expected type name");
      END;
      domType := Atom.FromText(s.tokText);
      Advance(s);
    END;

    RETURN NEW(ESCSpec.GhostVar,
               name := name, kind := kind,
               domainType := domType, rangeType := rangeType);
  END ParseGhostVar;

(* <*SPEC ABSTRACT M.V[x: T]: body *> *)
PROCEDURE ParseAbstract(VAR s: Scanner): ESCSpec.AbstractDef
  RAISES {ParseError} =
  VAR
    prefix, name: Atom.T;
    fullName: Atom.T;
    body: ESCSpec.Expr;
  BEGIN
    Advance(s);  (* consume ABSTRACT *)
    ParseQualName(s, prefix, name);
    IF prefix # NIL THEN
      fullName := Atom.FromText(Atom.ToText(prefix) & "." &
                                Atom.ToText(name));
    ELSE
      fullName := name;
    END;
    (* Skip [x: T] binding *)
    IF s.tok = Token.LBracket THEN
      Advance(s);
      WHILE s.tok # Token.RBracket AND s.tok # Token.Eof DO
        Advance(s);
      END;
      IF s.tok = Token.RBracket THEN Advance(s) END;
    END;
    Expect(s, Token.Colon);
    body := ParseExpr(s);
    RETURN NEW(ESCSpec.AbstractDef, name := fullName, body := body);
  END ParseAbstract;

(* <*SPEC DEPEND M.V[x: T]: field1, field2, ... *> *)
PROCEDURE ParseDepend(VAR s: Scanner): ESCSpec.DependDecl
  RAISES {ParseError} =
  VAR
    prefix, name: Atom.T;
    fullName: Atom.T;
    fields: RefList.T := NIL;
  BEGIN
    Advance(s);  (* consume DEPEND *)
    ParseQualName(s, prefix, name);
    IF prefix # NIL THEN
      fullName := Atom.FromText(Atom.ToText(prefix) & "." &
                                Atom.ToText(name));
    ELSE
      fullName := name;
    END;
    (* Skip [x: T] binding *)
    IF s.tok = Token.LBracket THEN
      Advance(s);
      WHILE s.tok # Token.RBracket AND s.tok # Token.Eof DO
        Advance(s);
      END;
      IF s.tok = Token.RBracket THEN Advance(s) END;
    END;
    Expect(s, Token.Colon);
    (* Parse comma-separated field names *)
    IF s.tok # Token.Ident THEN
      RAISE ParseError("expected field name");
    END;
    fields := RefList.List1(Atom.FromText(s.tokText));
    Advance(s);
    (* Handle qualified field names like t.st *)
    IF s.tok = Token.Dot THEN
      Advance(s);
      IF s.tok = Token.Ident THEN Advance(s) END;
    END;
    WHILE s.tok = Token.Comma DO
      Advance(s);
      IF s.tok # Token.Ident THEN
        RAISE ParseError("expected field name after ','");
      END;
      fields := RefList.AppendD(fields,
                  RefList.List1(Atom.FromText(s.tokText)));
      Advance(s);
      (* Handle qualified field names *)
      IF s.tok = Token.Dot THEN
        Advance(s);
        IF s.tok = Token.Ident THEN Advance(s) END;
      END;
    END;
    RETURN NEW(ESCSpec.DependDecl,
               abstractVar := fullName,
               concreteFields := fields);
  END ParseDepend;

(* <*SPEC FUNC name(formals): body *> *)
PROCEDURE ParseFunc(VAR s: Scanner): ESCSpec.SpecFunc RAISES {ParseError} =
  VAR
    name: Atom.T;
    formals: RefList.T;
    body: ESCSpec.Expr;
  BEGIN
    Advance(s);  (* consume FUNC *)
    IF s.tok # Token.Ident THEN RAISE ParseError("expected func name") END;
    name := Atom.FromText(s.tokText);
    Advance(s);
    formals := ParseTypedFormals(s);
    body := NIL;
    (* FUNC declarations may or may not have a body *)
    IF s.tok = Token.Colon THEN
      Advance(s);
      body := ParseExpr(s);
    END;
    RETURN NEW(ESCSpec.SpecFunc,
               name := name, formals := formals, body := body);
  END ParseFunc;

(* Parse typed formals: (x: T, y: U) *)
PROCEDURE ParseTypedFormals(VAR s: Scanner): RefList.T RAISES {ParseError} =
  VAR result: RefList.T := NIL;
  BEGIN
    Expect(s, Token.LParen);
    IF s.tok # Token.RParen THEN
      IF s.tok # Token.Ident THEN
        RAISE ParseError("expected formal name");
      END;
      result := RefList.List1(Atom.FromText(s.tokText));
      Advance(s);
      (* Skip : Type *)
      IF s.tok = Token.Colon THEN
        Advance(s);
        IF s.tok = Token.Ident THEN Advance(s) END;
        (* Handle qualified types *)
        IF s.tok = Token.Dot THEN Advance(s); IF s.tok = Token.Ident THEN Advance(s) END END;
      END;
      WHILE s.tok = Token.Comma DO
        Advance(s);
        IF s.tok # Token.Ident THEN
          RAISE ParseError("expected formal name after ','");
        END;
        result := RefList.AppendD(result,
                    RefList.List1(Atom.FromText(s.tokText)));
        Advance(s);
        (* Skip : Type *)
        IF s.tok = Token.Colon THEN
          Advance(s);
          IF s.tok = Token.Ident THEN Advance(s) END;
          IF s.tok = Token.Dot THEN Advance(s); IF s.tok = Token.Ident THEN Advance(s) END END;
        END;
      END;
    END;
    Expect(s, Token.RParen);
    RETURN result;
  END ParseTypedFormals;

(* <*SPEC AXIOM body *> *)
PROCEDURE ParseAxiom(VAR s: Scanner): ESCSpec.SpecAxiom RAISES {ParseError} =
  BEGIN
    Advance(s);  (* consume AXIOM *)
    RETURN NEW(ESCSpec.SpecAxiom, body := ParseExpr(s));
  END ParseAxiom;

(* <*SPEC INVARIANT body *>  or  <*SPEC REP body *> *)
PROCEDURE ParseInvariant(VAR s: Scanner;
                         kind: ESCSpec.InvariantKind): ESCSpec.Invariant
  RAISES {ParseError} =
  BEGIN
    Advance(s);  (* consume INVARIANT or REP *)
    RETURN NEW(ESCSpec.Invariant, kind := kind, body := ParseExpr(s));
  END ParseInvariant;

(* name(formals) REQUIRES ... MODIFIES ... ENSURES ... *)
PROCEDURE ParseProcSpec(VAR s: Scanner): ESCSpec.ProcSpec RAISES {ParseError} =
  VAR
    prefix, name: Atom.T;
    fullName: Atom.T;
    formals, requires, ensures, modifies: RefList.T := NIL;
  BEGIN
    ParseQualName(s, prefix, name);
    IF prefix # NIL THEN
      fullName := Atom.FromText(Atom.ToText(prefix) & "." &
                                Atom.ToText(name));
    ELSE
      fullName := name;
    END;
    formals := ParseFormals(s);

    (* Parse clauses in any order *)
    LOOP
      IF s.tok = Token.Requires THEN
        Advance(s);
        requires := RefList.AppendD(requires,
                      RefList.List1(ParseExpr(s)));
      ELSIF s.tok = Token.Ensures THEN
        Advance(s);
        ensures := RefList.AppendD(ensures,
                     RefList.List1(ParseExpr(s)));
      ELSIF s.tok = Token.Modifies THEN
        Advance(s);
        modifies := ParseModifiesList(s, modifies);
      ELSE
        EXIT;
      END;
    END;

    RETURN NEW(ESCSpec.ProcSpec,
               name := fullName,
               formals := formals,
               requires := requires,
               ensures := ensures,
               modifies := modifies);
  END ParseProcSpec;

(* Parse comma-separated modifies targets *)
PROCEDURE ParseModifiesList(VAR s: Scanner;
                            existing: RefList.T): RefList.T
  RAISES {ParseError} =
  VAR result := existing;
  BEGIN
    result := RefList.AppendD(result,
                RefList.List1(ParseModifiesTarget(s)));
    WHILE s.tok = Token.Comma DO
      Advance(s);
      result := RefList.AppendD(result,
                  RefList.List1(ParseModifiesTarget(s)));
    END;
    RETURN result;
  END ParseModifiesList;

PROCEDURE ParseModifiesTarget(VAR s: Scanner): ESCSpec.ModifiesClause
  RAISES {ParseError} =
  BEGIN
    RETURN NEW(ESCSpec.ModifiesClause, target := ParsePostfixExpr(s));
  END ParseModifiesTarget;

(* ---- Expression parser (precedence climbing) ---- *)

(* Precedence (lowest to highest):
   IFF
   IMPLIES / =>
   OR
   AND
   NOT
   comparison: = # < <= > >=
   additive: + -
   multiplicative: *
   postfix: . [] ' ^ *)

PROCEDURE ParseExpr(VAR s: Scanner): ESCSpec.Expr RAISES {ParseError} =
  BEGIN
    RETURN ParseIff(s);
  END ParseExpr;

PROCEDURE ParseIff(VAR s: Scanner): ESCSpec.Expr RAISES {ParseError} =
  VAR left := ParseImplies(s);
  BEGIN
    WHILE s.tok = Token.Iff DO
      Advance(s);
      left := NEW(ESCSpec.EBinOp,
                  op := ESCSpec.BinOp.Eq, (* IFF is logical equality *)
                  left := left, right := ParseImplies(s));
    END;
    RETURN left;
  END ParseIff;

PROCEDURE ParseImplies(VAR s: Scanner): ESCSpec.Expr RAISES {ParseError} =
  VAR left := ParseOr(s);
  BEGIN
    IF s.tok = Token.Implies THEN
      Advance(s);
      (* Right-associative *)
      RETURN NEW(ESCSpec.EBinOp,
                 op := ESCSpec.BinOp.Implies,
                 left := left, right := ParseImplies(s));
    END;
    RETURN left;
  END ParseImplies;

PROCEDURE ParseOr(VAR s: Scanner): ESCSpec.Expr RAISES {ParseError} =
  VAR left := ParseAnd(s);
  BEGIN
    WHILE s.tok = Token.Or DO
      Advance(s);
      left := NEW(ESCSpec.EBinOp,
                  op := ESCSpec.BinOp.Or,
                  left := left, right := ParseAnd(s));
    END;
    RETURN left;
  END ParseOr;

PROCEDURE ParseAnd(VAR s: Scanner): ESCSpec.Expr RAISES {ParseError} =
  VAR left := ParseNot(s);
  BEGIN
    WHILE s.tok = Token.And DO
      Advance(s);
      left := NEW(ESCSpec.EBinOp,
                  op := ESCSpec.BinOp.And,
                  left := left, right := ParseNot(s));
    END;
    RETURN left;
  END ParseAnd;

PROCEDURE ParseNot(VAR s: Scanner): ESCSpec.Expr RAISES {ParseError} =
  BEGIN
    IF s.tok = Token.Not THEN
      Advance(s);
      RETURN NEW(ESCSpec.EUnOp,
                 op := ESCSpec.UnOp.Not,
                 operand := ParseNot(s));
    END;
    RETURN ParseComparison(s);
  END ParseNot;

PROCEDURE ParseComparison(VAR s: Scanner): ESCSpec.Expr RAISES {ParseError} =
  VAR left := ParseAdditive(s);
      op: ESCSpec.BinOp;
  BEGIN
    CASE s.tok OF
    | Token.Eq   => op := ESCSpec.BinOp.Eq;
    | Token.Hash => op := ESCSpec.BinOp.Neq;
    | Token.Lt   => op := ESCSpec.BinOp.Lt;
    | Token.Le   => op := ESCSpec.BinOp.Le;
    | Token.Gt   => op := ESCSpec.BinOp.Gt;
    | Token.Ge   => op := ESCSpec.BinOp.Ge;
    ELSE
      RETURN left;
    END;
    Advance(s);
    RETURN NEW(ESCSpec.EBinOp, op := op,
               left := left, right := ParseAdditive(s));
  END ParseComparison;

PROCEDURE ParseAdditive(VAR s: Scanner): ESCSpec.Expr RAISES {ParseError} =
  VAR left := ParseMultiplicative(s);
  BEGIN
    WHILE s.tok = Token.Plus OR s.tok = Token.Minus DO
      VAR op: ESCSpec.BinOp; BEGIN
        IF s.tok = Token.Plus
          THEN op := ESCSpec.BinOp.Add
          ELSE op := ESCSpec.BinOp.Sub
        END;
        Advance(s);
        left := NEW(ESCSpec.EBinOp, op := op,
                     left := left, right := ParseMultiplicative(s));
      END;
    END;
    RETURN left;
  END ParseAdditive;

PROCEDURE ParseMultiplicative(VAR s: Scanner): ESCSpec.Expr
  RAISES {ParseError} =
  VAR left := ParsePostfixExpr(s);
  BEGIN
    WHILE s.tok = Token.Star DO
      Advance(s);
      left := NEW(ESCSpec.EBinOp, op := ESCSpec.BinOp.Mul,
                   left := left, right := ParsePostfixExpr(s));
    END;
    RETURN left;
  END ParseMultiplicative;

PROCEDURE ParsePostfixExpr(VAR s: Scanner): ESCSpec.Expr RAISES {ParseError} =
  VAR e := ParsePrimaryExpr(s);
  BEGIN
    LOOP
      IF s.tok = Token.Dot THEN
        Advance(s);
        IF s.tok # Token.Ident THEN
          RAISE ParseError("expected field name after '.'");
        END;
        e := NEW(ESCSpec.EField, base := e,
                 field := Atom.FromText(s.tokText));
        Advance(s);
      ELSIF s.tok = Token.LBracket THEN
        Advance(s);
        e := NEW(ESCSpec.ESelect, base := e, index := ParseExpr(s));
        Expect(s, Token.RBracket);
      ELSIF s.tok = Token.Prime THEN
        Advance(s);
        e := NEW(ESCSpec.EPrimed, base := e);
      ELSIF s.tok = Token.Caret THEN
        Advance(s);
        (* Dereference -- treat as field access to "^" *)
        e := NEW(ESCSpec.EField, base := e,
                 field := Atom.FromText("^"));
      ELSE
        EXIT;
      END;
    END;
    RETURN e;
  END ParsePostfixExpr;

PROCEDURE ParsePrimaryExpr(VAR s: Scanner): ESCSpec.Expr RAISES {ParseError} =
  VAR name: Atom.T;
  BEGIN
    CASE s.tok OF
    | Token.Ident =>
      name := Atom.FromText(s.tokText);
      Advance(s);
      IF s.tok = Token.LParen THEN
        (* Function call *)
        RETURN ParseCallArgs(s, name);
      END;
      RETURN NEW(ESCSpec.EIdent, name := name);

    | Token.Int =>
      WITH val = s.tokInt DO
        Advance(s);
        RETURN NEW(ESCSpec.EConst, val := val);
      END;

    | Token.Res =>
      Advance(s);
      RETURN NEW(ESCSpec.ERes);

    | Token.Nil =>
      Advance(s);
      RETURN NEW(ESCSpec.ENil);

    | Token.True =>
      Advance(s);
      RETURN NEW(ESCSpec.EIdent, name := Atom.FromText("TRUE"));

    | Token.False =>
      Advance(s);
      RETURN NEW(ESCSpec.EIdent, name := Atom.FromText("FALSE"));

    | Token.Fresh =>
      Advance(s);
      Expect(s, Token.LParen);
      WITH arg = ParseExpr(s) DO
        Expect(s, Token.RParen);
        RETURN NEW(ESCSpec.EFresh, arg := arg);
      END;

    | Token.Number =>
      Advance(s);
      Expect(s, Token.LParen);
      WITH arg = ParseExpr(s) DO
        Expect(s, Token.RParen);
        RETURN NEW(ESCSpec.ENumber, arg := arg);
      END;

    | Token.Member, Token.Insert, Token.Delete, Token.Sup,
      Token.Concat, Token.Subarray, Token.Min, Token.Max =>
      name := Atom.FromText(s.tokText);
      Advance(s);
      IF s.tok = Token.LParen THEN
        RETURN ParseCallArgs(s, name);
      END;
      RETURN NEW(ESCSpec.EIdent, name := name);

    | Token.All =>
      RETURN ParseQuantifier(s, ESCSpec.QuantKind.All);

    | Token.Exists =>
      RETURN ParseQuantifier(s, ESCSpec.QuantKind.Exists);

    | Token.LParen =>
      Advance(s);
      WITH e = ParseExpr(s) DO
        Expect(s, Token.RParen);
        RETURN e;
      END;

    | Token.Not =>
      Advance(s);
      RETURN NEW(ESCSpec.EUnOp, op := ESCSpec.UnOp.Not,
                 operand := ParsePrimaryExpr(s));

    | Token.Minus =>
      Advance(s);
      RETURN NEW(ESCSpec.EUnOp, op := ESCSpec.UnOp.Neg,
                 operand := ParsePrimaryExpr(s));

    ELSE
      RAISE ParseError("unexpected token: " & TokenName(s.tok));
    END;
  END ParsePrimaryExpr;

PROCEDURE ParseCallArgs(VAR s: Scanner; name: Atom.T): ESCSpec.ECall
  RAISES {ParseError} =
  VAR args: RefList.T := NIL;
  BEGIN
    Expect(s, Token.LParen);
    IF s.tok # Token.RParen THEN
      args := RefList.List1(ParseExpr(s));
      WHILE s.tok = Token.Comma DO
        Advance(s);
        args := RefList.AppendD(args, RefList.List1(ParseExpr(s)));
      END;
    END;
    Expect(s, Token.RParen);
    RETURN NEW(ESCSpec.ECall, func := name, args := args);
  END ParseCallArgs;

(* ALL [x: T] body   or   EX [x: T] body *)
PROCEDURE ParseQuantifier(VAR s: Scanner;
                          kind: ESCSpec.QuantKind): ESCSpec.EQuant
  RAISES {ParseError} =
  VAR
    varName, typeName: Atom.T;
  BEGIN
    Advance(s);  (* consume ALL or EX *)
    Expect(s, Token.LBracket);
    IF s.tok # Token.Ident THEN
      RAISE ParseError("expected variable name in quantifier");
    END;
    varName := Atom.FromText(s.tokText);
    Advance(s);
    Expect(s, Token.Colon);
    IF s.tok # Token.Ident THEN
      RAISE ParseError("expected type name in quantifier");
    END;
    typeName := Atom.FromText(s.tokText);
    Advance(s);
    Expect(s, Token.RBracket);
    RETURN NEW(ESCSpec.EQuant,
               kind := kind,
               var := varName,
               type := typeName,
               body := ParseExpr(s));
  END ParseQuantifier;

(* ---- Public interface ---- *)

PROCEDURE Parse(raw: ESCSpec.RawPragma): ESCSpec.Spec RAISES {ParseError} =
  VAR s: Scanner;
  BEGIN
    IF raw.text = NIL THEN
      RAISE ParseError("empty pragma text");
    END;

    CASE raw.kind OF
    | ESCSpec.PragmaKind.Spec =>
      ScanInit(s, raw.text);
      RETURN ParseSpec(s);
    | ESCSpec.PragmaKind.LoopInv =>
      ScanInit(s, raw.text);
      RETURN NEW(ESCSpec.LoopInv, body := ParseExpr(s));
    | ESCSpec.PragmaKind.PragmaSpec =>
      (* <*PRAGMA SPEC*> -- not a real spec *)
      RETURN NIL;
    END;
  END Parse;

PROCEDURE ParseAll(raws: RefList.T): RefList.T RAISES {ParseError} =
  VAR
    result: RefList.T := NIL;
    last: RefList.T := NIL;
    p := raws;
    raw: ESCSpec.RawPragma;
    spec: ESCSpec.Spec;
  BEGIN
    WHILE p # NIL DO
      raw := NARROW(p.head, ESCSpec.RawPragma);
      spec := Parse(raw);
      IF spec # NIL THEN
        WITH cell = RefList.List1(spec) DO
          IF last = NIL THEN
            result := cell;
          ELSE
            last.tail := cell;
          END;
          last := cell;
        END;
      END;
      p := p.tail;
    END;
    RETURN result;
  END ParseAll;

BEGIN
END ESCSpecParse.
