(*---------------------------------------------------------------------------*)
MODULE FindExpr;

IMPORT Text, TextRd, Fmt;
IMPORT Glob, GlobTree, SimpleScanner, ScanToken;
IMPORT FSFindError;

(*---------------------------------------------------------------------------*)
(* The revelation of SimpleScanner.Token has been commented out because
   multiple revelations in one program are not allowed. So the best
   solution seems to be to reveal the exact type once per program in
   the program package.
REVEAL
  SimpleScanner.Token = ScanToken.T;
*)

(*---------------------------------------------------------------------------*)
PROCEDURE New(t : TEXT) : T RAISES {FSFindError.E} =
  VAR
    rd := TextRd.New(t);
    actToken :  SimpleScanner.Token := NIL;
    scanner  := NEW(SimpleScanner.T);
    res : T;
    subExprs := 0;

  (*-------------------------------------------------------------------------*)
  PROCEDURE ParseError(e : TEXT) RAISES {FSFindError.E} =
    VAR t : TEXT;
    BEGIN
      t := Fmt.F("parse error at token `%s': %s", actToken.repr, e);
      RAISE FSFindError.E(t);
    END ParseError;

  (*-------------------------------------------------------------------------*)
  PROCEDURE GetNext() RAISES {FSFindError.E} =
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
    RAISES {FSFindError.E} =

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
    RAISES {FSFindError.E} = 

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
  PROCEDURE GetStringOrIdentifier() : TEXT RAISES {FSFindError.E} =
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
        IF Text.GetChar(s, 0) = RegularExpressionPrefix THEN
          RETURN Glob.MatchOptions{Glob.MatchOption.UseSimpleRE};
        END;
      END;
      RETURN Glob.MatchOptions{};
    END Options;

  (*-------------------------------------------------------------------------*)
  PROCEDURE StripREPrefix(s : TEXT) : TEXT =
    BEGIN
      IF s # NIL AND Text.Length(s) > 0 THEN
        IF Text.GetChar(s, 0) = RegularExpressionPrefix THEN
          RETURN Text.Sub(s, 1);
        END;
      END;
      RETURN s;
    END StripREPrefix; 

  (*-------------------------------------------------------------------------*)
  PROCEDURE ParseExpr() : T RAISES {FSFindError.E} =
    VAR start : T;
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
  PROCEDURE ParseTerm() : T RAISES {FSFindError.E} =
    VAR start : T;
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
  PROCEDURE ParseFactor() : T RAISES {FSFindError.E} =
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
        RETURN GlobTree.Match(StripREPrefix(factor), Options(factor));
      END;
      RETURN NIL; (* make the compiler happy *)
    END ParseFactor;

  BEGIN (* New *)
    EVAL scanner.init(rd);
    scanner.skipComments    := TRUE;
    scanner.nestingComments := TRUE;
    scanner.commentOpenSym  := "(*";
    scanner.commentCloseSym := "*)";
    scanner.lineCommentSym  := "#";
    EVAL scanner.keywordToken.insert("and");
    EVAL scanner.keywordToken.insert("or");
    EVAL scanner.keywordToken.insert("not");
    EVAL scanner.keywordToken.insert("true");
    EVAL scanner.keywordToken.insert("false");
    GetNext();
    res := ParseExpr();
    IF NOT scanner.eof() THEN
      ParseError("premature end of expression");
    END;
    RETURN res;
  END New;

BEGIN
END FindExpr.
