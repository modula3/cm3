(*---------------------------------------------------------------------------*)
MODULE FileClassification;

IMPORT Rd, TextRd, FileRd, Text, TextSeq, TextTextTbl, Fmt, Pathname;
IMPORT Glob, GlobTree, RegEx, TextUtils, SimpleScanner, ScanToken, PathRepr;
IMPORT FindExpr, FindExprSeq;

REVEAL
  T = Public BRANDED "FileClassification 0.0" OBJECT
    lhs   : FindExprSeq.T;
    rhs   : TextSeq.T;
    idirs : FindExprSeq.T;
    uppn  : BOOLEAN;
  OVERRIDES
    init := Init;
    size := Size;
    addFromText := AddFromText;
    addFromFile := AddFromFile;
    patterns := Patterns;
    classSpecs := ClassSpecs;
    pattern := Pattern;
    classSpec := ClassSpec;
    ignoreDir := IgnoreDir;
    matches := Matches;
    match := Match;
    substClassSpec := SubstClassSpec;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE Init(self : T; usePosixPathnames := FALSE) : T =
  BEGIN
    self.idirs := NEW(FindExprSeq.T).init();
    self.lhs := NEW(FindExprSeq.T).init();
    self.rhs := NEW(TextSeq.T).init();
    self.uppn := usePosixPathnames;
    RETURN self;
  END Init;

(*---------------------------------------------------------------------------*)
PROCEDURE Size(self : T) : CARDINAL =
  BEGIN
    RETURN self.lhs.size();
  END Size;

(*---------------------------------------------------------------------------*)
PROCEDURE ParseRules(self : T; rd : Rd.T) RAISES {E} =
  VAR
    actToken    :  SimpleScanner.Token := NIL;
    scanner     := NEW(SimpleScanner.T);
    subExprs    := 0;
    defaultSeen := FALSE;

  (*-------------------------------------------------------------------------*)
  PROCEDURE ParseError(e : TEXT) RAISES {E} =
    VAR t : TEXT;
    BEGIN
      t := Fmt.F("parse error at token `%s': %s", actToken.repr, e);
      RAISE E(t);
    END ParseError;

  (*-------------------------------------------------------------------------*)
  PROCEDURE GetNext() RAISES {E} =
    BEGIN
      TRY
        actToken := scanner.nextToken();
      EXCEPT
        SimpleScanner.Error(e) => ParseError("scanner error: " & e);
      END;
    END GetNext;

  (*-------------------------------------------------------------------------*)
  PROCEDURE Matches(t1 : TEXT; t2 : TEXT := NIL;
                    kind := ScanToken.Kind.Keyword) : BOOLEAN 
    RAISES {E} =

    (*-----------------------------------------------------------------------*)
    PROCEDURE Matches1(t : TEXT) : BOOLEAN =
      BEGIN
        RETURN actToken.kind = kind AND Text.Equal(actToken.repr, t);
      END Matches1;

    VAR ok := FALSE; memo := actToken;
    BEGIN
      IF Matches1(t1) THEN
        GetNext();
        IF t2 = NIL THEN
          ok := TRUE;
        ELSE
          IF Matches1(t2) THEN
            GetNext();
            ok := TRUE;
          ELSE
            scanner.pushBack(actToken);
            actToken := memo;
          END;
        END;
      END;
      RETURN ok;
    END Matches;

  (*-------------------------------------------------------------------------*)
  PROCEDURE MatchesSpecial(t1 : TEXT; t2 : TEXT := NIL) : BOOLEAN
    RAISES {E} = 

    (*-----------------------------------------------------------------------*)
    PROCEDURE Matches1(t : TEXT) : BOOLEAN =
      BEGIN
        RETURN (actToken.kind = ScanToken.Kind.CompoundSymbol OR
                actToken.kind = ScanToken.Kind.Other) AND
               Text.Equal(actToken.repr, t);
      END Matches1;

    VAR ok := FALSE; memo := actToken;
    BEGIN
      IF Matches1(t1) THEN
        GetNext();
        IF t2 = NIL THEN
          ok := TRUE;
        ELSE
          IF Matches1(t2) THEN
            GetNext();
            ok := TRUE;
          ELSE
            scanner.pushBack(actToken);
            actToken := memo;
          END;
        END;
      END;
      RETURN ok;
    END MatchesSpecial;

  (*-------------------------------------------------------------------------*)
  PROCEDURE GetStringOrIdentifier() : TEXT RAISES {E} =
    VAR id := actToken.repr;
    BEGIN
      IF actToken.kind = ScanToken.Kind.String OR
         actToken.kind = ScanToken.Kind.Ident THEN
        GetNext();
        RETURN id;
      END;
      ParseError("expected string or identifier"); <* NOWARN *>
    END GetStringOrIdentifier; 

  (*-------------------------------------------------------------------------*)
  PROCEDURE Options(s : TEXT) : Glob.MatchOptions =
    BEGIN
      IF s # NIL AND Text.Length(s) > 0 THEN
        IF Text.GetChar(s, 0) = FindExpr.RegularExpressionPrefix THEN
          RETURN Glob.MatchOptions{Glob.MatchOption.UseSimpleRE};
        END;
      END;
      RETURN Glob.MatchOptions{};
    END Options;

  (*-------------------------------------------------------------------------*)
  PROCEDURE StripREPrefix(s : TEXT) : TEXT =
    BEGIN
      IF s # NIL AND Text.Length(s) > 0 THEN
        IF Text.GetChar(s, 0) = FindExpr.RegularExpressionPrefix THEN
          RETURN Text.Sub(s, 1);
        END;
      END;
      RETURN s;
    END StripREPrefix; 

  (*-------------------------------------------------------------------------*)
  PROCEDURE ParseExpr() : FindExpr.T RAISES {E} =
    VAR start : FindExpr.T;
    BEGIN
      start := ParseTerm();
      IF Matches("or") THEN
        RETURN GlobTree.Or(start, ParseExpr());
      ELSIF MatchesSpecial(")") THEN
        IF subExprs > 0 THEN
          DEC(subExprs);
          RETURN start;
        ELSE
          ParseError("too many `)'");
        END;
      END;
      RETURN start;
    END ParseExpr;  

  (*-------------------------------------------------------------------------*)
  PROCEDURE ParseTerm() : FindExpr.T RAISES {E} =
    VAR start : FindExpr.T;
    BEGIN
      start := ParseFactor();
      IF Matches("and") THEN
        RETURN GlobTree.And(start, ParseTerm());
      ELSIF MatchesSpecial(")") THEN
        IF subExprs > 0 THEN
          DEC(subExprs);
          RETURN start;
        ELSE
          ParseError("too many `)'");
        END;
      END;
      RETURN start;
    END ParseTerm;  

  (*-------------------------------------------------------------------------*)
  PROCEDURE ParseFactor() : FindExpr.T RAISES {E} =
    VAR factor : TEXT;
    BEGIN
      IF Matches("true") THEN
        RETURN GlobTree.True;
      ELSIF Matches("false") THEN
        RETURN GlobTree.False;
      ELSIF Matches("not") THEN
        RETURN GlobTree.Not(ParseFactor());
      ELSIF MatchesSpecial("(") THEN
        INC(subExprs);
        RETURN ParseExpr();
      ELSIF MatchesSpecial(")") THEN
        ParseError("unexpected `)'");
      ELSE
        factor := GetStringOrIdentifier();
        RETURN GlobTree.Match(StripREPrefix(factor),  <*NOWARN*>
                              Options(factor));
      END;
    END ParseFactor;

  (*-------------------------------------------------------------------------*)
  PROCEDURE ParseRule() RAISES {E} =
    VAR
      lhs : FindExpr.T;
      rhs : TEXT;
    BEGIN
      IF Matches("default") THEN
        IF NOT MatchesSpecial("=>") THEN
          ParseError("expected `=>'");
        END;
        rhs := GetStringOrIdentifier();
        IF rhs = NIL THEN
          ParseError("expected string or identifier");
        END;
        IF defaultSeen THEN
          ParseError("multiple default declarations");
        END;
        defaultSeen := TRUE;
        TRY
          lhs := FindExpr.New("\"*\"");
        EXCEPT ELSE END;
        self.lhs.addhi(lhs);
        self.rhs.addhi(rhs);
      ELSE
        lhs := ParseExpr();
        IF lhs = NIL THEN
          ParseError("unparseable expression");
        END;
        IF NOT MatchesSpecial("=>") THEN
          ParseError("expected `=>'");
        END;
        IF Matches("ignoredir") OR Matches("skip") THEN
          self.idirs.addhi(lhs);
        ELSE
          IF Matches("ignore") THEN
            rhs := NIL;
          ELSE
            rhs := GetStringOrIdentifier();
            IF rhs = NIL THEN
              ParseError("expected string or identifier");
            END;
          END;
          self.lhs.addhi(lhs);
          self.rhs.addhi(rhs);
        END;
      END;
    END ParseRule;

  (*-------------------------------------------------------------------------*)
  PROCEDURE ParseAll() RAISES {E} =
    BEGIN
      WHILE NOT scanner.eof() DO
        ParseRule();
      END;
    END ParseAll;

  (*-------------------------------------------------------------------------*)
  BEGIN (* New *)
    EVAL scanner.init(rd);
    scanner.skipComments    := TRUE;
    scanner.nestingComments := TRUE;
    scanner.commentOpenSym  := "(*";
    scanner.commentCloseSym := "*)";
    scanner.lineCommentSym  := "#";
    EVAL scanner.keywordToken.insert("default");
    EVAL scanner.keywordToken.insert("ignore");
    EVAL scanner.keywordToken.insert("ignoredir");
    EVAL scanner.keywordToken.insert("skip");
    EVAL scanner.keywordToken.insert("and");
    EVAL scanner.keywordToken.insert("or");
    EVAL scanner.keywordToken.insert("not");
    EVAL scanner.keywordToken.insert("true");
    EVAL scanner.keywordToken.insert("false");
    EVAL scanner.compoundToken.insert("=>");
    GetNext();
    ParseAll();
    IF NOT scanner.eof() THEN
      ParseError("premature end of file classification");
    END;
  END ParseRules;

(*---------------------------------------------------------------------------*)
PROCEDURE AddFromText(self : T; t : TEXT) RAISES {E} =
  VAR rd := TextRd.New(t);
  BEGIN
    ParseRules(self, rd);
    TRY
      Rd.Close(rd);
    EXCEPT ELSE END; (* we are not interested any more *)
  END AddFromText;

(*---------------------------------------------------------------------------*)
PROCEDURE AddFromFile(self : T; fn : Pathname.T) RAISES {E} =
  VAR rd : FileRd.T;
  BEGIN
    TRY
      rd := FileRd.Open(fn);
    EXCEPT ELSE
      RAISE E("cannot open file " & fn);
    END;
    ParseRules(self, rd);
    TRY
      Rd.Close(rd);
    EXCEPT ELSE END; (* we are not interested any more *)
  END AddFromFile;

(*---------------------------------------------------------------------------*)
PROCEDURE Patterns(self : T) : FindExprSeq.T =
  BEGIN
    RETURN self.lhs;
  END Patterns;

(*---------------------------------------------------------------------------*)
PROCEDURE ClassSpecs(self : T) : TextSeq.T =
  BEGIN
    RETURN self.rhs;
  END ClassSpecs;

(*---------------------------------------------------------------------------*)
PROCEDURE Pattern(self : T; n : CARDINAL) : FindExpr.T =
  BEGIN
    RETURN self.lhs.get(n);
  END Pattern;

(*---------------------------------------------------------------------------*)
PROCEDURE ClassSpec(self : T; n : CARDINAL) : TEXT =
  BEGIN
    RETURN self.rhs.get(n);
  END ClassSpec;

(*---------------------------------------------------------------------------*)
PROCEDURE IgnoreDir(self : T; dir : Pathname.T) : BOOLEAN =
  VAR 
    match   := FALSE;
    dirlast := Pathname.Last(dir);
  BEGIN
    FOR i := 0 TO self.idirs.size() - 1 DO
      WITH expr = self.idirs.get(i) DO
        TRY 
          match := expr.test(dirlast);
        EXCEPT
          RegEx.Error => (* skip *)
        END;
        IF match THEN
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END IgnoreDir; 

(*---------------------------------------------------------------------------*)
PROCEDURE Matches(self : T; fn : Pathname.T) : INTEGER RAISES {E} =
  VAR 
    rhs : TEXT;
    match : BOOLEAN;
    fnlast := Pathname.Last(fn);
  BEGIN
    FOR i := 0 TO self.lhs.size() - 1 DO
      WITH expr = self.lhs.get(i) DO
        IF expr = NIL THEN
          rhs := self.rhs.get(i);
          IF rhs = NIL THEN rhs := "NIL"; END;
          RAISE E("undefined expression for rule " &
                Fmt.Int(i) & ", rhs = `" & rhs & "'");
        END;
        TRY 
          match := expr.test(fnlast);
        EXCEPT
          RegEx.Error => (* skip *)
        END;
        IF match THEN
          IF self.rhs.get(i) = NIL THEN
            (* ignore rule *)
            RETURN -1;
          ELSE
            RETURN i;
          END;
        END;
      END;
    END;
    RETURN -1;
  END Matches;

(*---------------------------------------------------------------------------*)
PROCEDURE Match(self : T; fn : Pathname.T; 
                env : TextTextTbl.T := NIL) : TEXT RAISES {E} =
  VAR
    n : INTEGER;
  BEGIN
    n := Matches(self, fn);
    IF n = -1 THEN
      RETURN NIL;
    END;
    RETURN SubstClassSpec(self, n, fn, env);
  END Match;

(*---------------------------------------------------------------------------*)
PROCEDURE SubstClassSpec(self : T; n : CARDINAL; fn : Pathname.T;
                         env : TextTextTbl.T := NIL) : TEXT RAISES {E} =
  VAR res, spec : TEXT;
  BEGIN
    IF env = NIL THEN
      env := NEW(TextTextTbl.Default).init();
    END;
    IF self.uppn THEN
      EVAL env.put("fn", PathRepr.Posix(fn));
      EVAL env.put("dir", PathRepr.Posix(Pathname.Prefix(fn)));
      EVAL env.put("last", PathRepr.Posix(Pathname.Last(fn)));
      EVAL env.put("base", PathRepr.Posix(Pathname.Base(fn)));
      EVAL env.put("lastbase", PathRepr.Posix(Pathname.LastBase(fn)));
      EVAL env.put("ext", PathRepr.Posix(Pathname.LastExt(fn)));
    ELSE
      EVAL env.put("fn", fn);
      EVAL env.put("dir", Pathname.Prefix(fn));
      EVAL env.put("last", Pathname.Last(fn));
      EVAL env.put("base", Pathname.Base(fn));
      EVAL env.put("lastbase", Pathname.LastBase(fn));
      EVAL env.put("ext", Pathname.LastExt(fn));
    END;
    IF n < 0 OR n >= self.rhs.size() THEN
      RAISE E("no such classification rule");
    END;
    spec := self.rhs.get(n);
    IF spec = NIL THEN
      RETURN NIL; (* ignore rule *)
    END;
    TRY
      res := TextUtils.SubstituteVariables(spec, env);
    EXCEPT
      TextUtils.Error(e) => RAISE E("substitution failed: " & e);
    END;
    RETURN res;
  END SubstClassSpec;

(*---------------------------------------------------------------------------*)
PROCEDURE New(t : TEXT; usePosixPathnames := FALSE) : T RAISES {E} =
  VAR fc := NEW(T).init(usePosixPathnames);
  BEGIN
    fc.addFromText(t);
    RETURN fc;
  END New;


(*---------------------------------------------------------------------------*)
PROCEDURE Read(fn : Pathname.T; usePosixPathnames := FALSE) : T RAISES {E} =
  VAR fc := NEW(T).init(usePosixPathnames);
  BEGIN
    fc.addFromFile(fn);
    RETURN fc;
  END Read; 

BEGIN
END FileClassification.
