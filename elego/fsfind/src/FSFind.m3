(*---------------------------------------------------------------------------*)
MODULE FSFind;

IMPORT Pathname, TextSeq, FS, OSError, Wr, TextWr, Thread, Text;
IMPORT FSUtils, SMsg AS Msg, RegEx, PathRepr, TextUtils;
IMPORT FSFindError, FindExpr, FindExprSeq, FileClassification;

(*---------------------------------------------------------------------------*)
PROCEDURE SimplifyPN(p : Pathname.T; stripPrefix := TRUE) : Pathname.T =
  VAR start := Pathname.Current & PathRepr.PathSep;
  BEGIN
    IF stripPrefix AND TextUtils.Pos(p, start) = 0 THEN
      RETURN Text.Sub(p, Text.Length(start));
    ELSE
      RETURN p;
    END;
  END SimplifyPN;

(*---------------------------------------------------------------------------*)
PROCEDURE List(root : Pathname.T; expr : FindExpr.T;
               ignoreDirs : FindExpr.T; stripPrefix := TRUE) : TextSeq.T
  RAISES {FSFindError.E} =
  VAR
    res := NEW(TextSeq.T).init();

  (*-------------------------------------------------------------------------*)
  PROCEDURE ListRec(current : Pathname.T) RAISES {FSFindError.E} =
    VAR fn : Pathname.T;
    BEGIN
      TRY
        IF FSUtils.IsFile(current) THEN
          IF expr.test(Pathname.Last(current)) THEN
            res.addhi(SimplifyPN(current, stripPrefix));
          END;
        ELSIF FSUtils.IsDir(current) AND NOT
          (ignoreDirs # NIL AND ignoreDirs.test(Pathname.Last(current))) THEN
          TRY
            WITH it = FS.Iterate(current) DO
              TRY
                WHILE it.next(fn) DO
                  ListRec(Pathname.Join(current, fn, NIL));
                END;
              EXCEPT ELSE
                Msg.Error("cannot get status of file " & fn  & 
                  " in directory " & current & ", iteration aborted");
              END;
            END;
          EXCEPT ELSE
            Msg.Error("cannot iterate directory" & current);
          END;
        END;
      EXCEPT
        RegEx.Error(e) => RAISE FSFindError.E("regex error: " & e);
      END;
    END ListRec;

  BEGIN (* List *)
    ListRec(root);
    RETURN res;
  END List;

(*---------------------------------------------------------------------------*)
PROCEDURE SimpleClassify(
    root       : Pathname.T;        (* root of tree traversal *)
    patterns   : FindExprSeq.T;     (* list of expressions to select files *)
    ignoreDirs : FindExpr.T := NIL; (* selects which directories to ignore *)
    recursive  := TRUE;
    stripPrefix := TRUE;
  ) : REF ARRAY OF TextSeq.T RAISES {FSFindError.E} =

  (*-------------------------------------------------------------------------*)
  PROCEDURE ClassifyRec(current : Pathname.T; level := 0) 
    RAISES {FSFindError.E} =
    VAR fn : Pathname.T;
    BEGIN
      TRY
        IF FSUtils.IsFile(current) THEN
          FOR i := 0 TO rules - 1 DO
            WITH expr = patterns.get(i) DO
              IF expr.test(Pathname.Last(current)) THEN
                res[i].addhi(SimplifyPN(current, stripPrefix));
              END;
            END;
          END;
        ELSIF (recursive OR level = 0) AND FSUtils.IsDir(current) AND NOT
          (ignoreDirs # NIL AND ignoreDirs.test(Pathname.Last(current))) THEN
          TRY
            WITH it = FS.Iterate(current) DO
              TRY
                WHILE it.next(fn) DO
                  ClassifyRec(Pathname.Join(current, fn, NIL), level + 1);
                END;
              EXCEPT ELSE
                Msg.Error("cannot get status of file " & fn  & 
                  " in directory " & current & ", iteration aborted");
              END;
            END;
          EXCEPT ELSE
            Msg.Error("cannot iterate directory" & current);
          END;
        END;
      EXCEPT
        RegEx.Error(e) => RAISE FSFindError.E("regex error: " & e);
      END;
    END ClassifyRec; 

  (*-------------------------------------------------------------------------*)
  VAR (* SimpleClassify *)
    rules := patterns.size();
    res := NEW(REF ARRAY OF TextSeq.T, rules);
  BEGIN
    FOR i := 0 TO rules - 1 DO
      res[i] := NEW(TextSeq.T).init();
    END;
    ClassifyRec(root);
    RETURN res;
  END SimpleClassify;

(*---------------------------------------------------------------------------*)
PROCEDURE ClassifyToWr(
    root : Pathname.T;             (* root of tree traversal *)
    fc   : FileClassification.T;   (* file classification rules *)
    wr   : Wr.T;
    recursive  := TRUE;
    flush  := FALSE;
    stripPrefix := TRUE;
  ) RAISES {FSFindError.E} =

  (*-------------------------------------------------------------------------*)
  PROCEDURE ClassifyRec(current : Pathname.T; level := 0) 
    RAISES {FSFindError.E} =
    VAR fn : Pathname.T;
    BEGIN
      IF FSUtils.IsFile(current) THEN
        TRY
          WITH line = fc.match(SimplifyPN(current, stripPrefix)) DO
            IF line # NIL THEN
              TRY
                Wr.PutText(wr, line & "\n");
                IF flush THEN
                  Wr.Flush(wr);
                END;
              EXCEPT
                Thread.Alerted => RAISE FSFindError.E("interrupted");
              | Wr.Failure => RAISE FSFindError.E("write error");
              END;
            END;
          END;
        EXCEPT
          FileClassification.E(e) => 
          RAISE FSFindError.E("classification error: " & e);
        END;
      ELSIF (recursive OR level = 0) AND FSUtils.IsDir(current) AND 
        NOT fc.ignoreDir(current) THEN
        TRY
          WITH it = FS.Iterate(current) DO
            WHILE it.next(fn) DO
              ClassifyRec(Pathname.Join(current, fn, NIL), level + 1);
            END;
          END;
        EXCEPT 
          OSError.E =>
          Msg.Error("cannot iterate directory" & current);
        END;
      END;
    END ClassifyRec; 

  (*-------------------------------------------------------------------------*)
  BEGIN (* ClassifyToWr *)
    ClassifyRec(root);
  END ClassifyToWr;

(*---------------------------------------------------------------------------*)
PROCEDURE Classify(
    root : Pathname.T;             (* root of tree traversal *)
    fc   : FileClassification.T;   (* file classification rules *)
    recursive  := TRUE;
    stripPrefix := TRUE;
  ) : TEXT RAISES {FSFindError.E} =
  VAR
    wr := TextWr.New();
  BEGIN
    ClassifyToWr(root, fc, wr, recursive, FALSE, stripPrefix);
    RETURN TextWr.ToText(wr);
  END Classify;

BEGIN
END FSFind.
