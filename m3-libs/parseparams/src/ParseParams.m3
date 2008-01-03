(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Jan 30 15:01:03 PST 1995 by kalsow     *)
(*      modified on Fri Sep  2 03:32:09 PDT 1994 by stolfi     *)
(*      modified on Wed Mar 13 01:30:31 1991 by muller         *)
(*      modified on Fri Jun  2 18:25:43 1989 by ellis          *)

MODULE ParseParams;

IMPORT Text, Wr, Fmt, Scan, Lex, Thread, Params, FloatMode;

REVEAL
  T = Public BRANDED OBJECT
      wr: Wr.T; (* Writer for error messages *)
    OVERRIDES
      init := Init;
      keywordPresent := KeywordPresent;
      getKeyword := GetKeyword;
      getNext := GetNext;
      testNext := TestNext;
      getNextInt := GetNextInt;
      getNextReal := GetNextReal;
      getNextLongReal := GetNextLongReal;
      error := PrintError;
      skipParsed := SkipParsed;
      finish := Finish;
    END;

PROCEDURE Init(t: T; wr: Wr.T): T =
  BEGIN
    t.wr := wr;
    WITH num = Params.Count DO
      t.arg := NEW(REF ARRAY OF TEXT, num);
      t.parsed := NEW(REF ARRAY OF BOOLEAN, num);
      WITH a = t.arg^, p = t.parsed^ DO
        FOR i := 0 TO num-1 DO 
          a[i] := Params.Get(i); p[i] := FALSE
        END;
	p[0] := TRUE;
	t.next := 1
      END;
    END;
    RETURN t
  END Init;

PROCEDURE KeywordPresent(t: T; key: TEXT): BOOLEAN =
  BEGIN
    WITH a = t.arg^, p = t.parsed^ DO
      FOR i := 0 TO LAST(a) DO
        IF NOT p[i] AND Text.Equal(key, a[i]) THEN
          t.next := i + 1;
          p[i] := TRUE;
          RETURN TRUE;
        END
      END
    END;
    RETURN FALSE
  END KeywordPresent;

PROCEDURE GetKeyword(t: T; key: TEXT) RAISES {Error} =
  BEGIN
    IF NOT t.keywordPresent(key) THEN
      t.error("keyword \"" & key & "\" not found.");
    END;
  END GetKeyword;

PROCEDURE GetNext(t: T): TEXT RAISES {Error} =
  BEGIN
    WITH a = t.arg^, p = t.parsed^ DO
      IF (t.next > LAST(a)) OR p[t.next] THEN
        t.error("missing argument after argument " &
          Fmt.Int(t.next-1) & " = \"" &
          a[t.next-1] & "\"."
	)
      END;
      p[t.next] := TRUE;
      INC(t.next);
      RETURN a[t.next-1]
    END;
  END GetNext;

PROCEDURE TestNext (t: T; key: TEXT): BOOLEAN RAISES {} =
  BEGIN
    WITH a = t.arg^, p = t.parsed^ DO
      IF (t.next > LAST(a)) OR p[t.next]
      OR NOT Text.Equal(key, a[t.next]) THEN
        RETURN FALSE
      ELSE
        p[t.next] := TRUE;
        INC(t.next);
        RETURN TRUE
      END
    END
  END TestNext;

PROCEDURE GetNextInt(
    t: T; 
    min := FIRST(INTEGER);  
    max := LAST(INTEGER)
  ): INTEGER RAISES {Error} =
  VAR nn: INTEGER;
  BEGIN
    WITH txt = t.getNext() DO
      TRY
        nn := Scan.Int(txt);
      EXCEPT 
        Lex.Error, FloatMode.Trap =>
          t.error(
	    "parameter " & Fmt.Int(t.next-1) & " = \"" & txt &
	    "\" should be an integer."
	  )
      END;
      IF (nn < min) OR (nn > max) THEN
        t.error (
	  "parameter " & Fmt.Int(t.next-1) & " = " & Fmt.Int(nn) &
          " should be in [" & Fmt.Int(min) & ".." & Fmt.Int(max) & "]."
	)
      END;
    END;
    RETURN nn
  END GetNextInt;

PROCEDURE GetNextReal(
    t: T;
    min := FIRST(REAL);  
    max := LAST(REAL)
  ): REAL RAISES {Error} =
  VAR x: REAL;
  BEGIN
    WITH txt = t.getNext() DO
      TRY
        x := Scan.Real(txt);
      EXCEPT 
        Lex.Error, FloatMode.Trap =>
        t.error(
	  "parameter " & Fmt.Int(t.next-1) & " = \"" & txt &
	  "\" should be a real number."
	)
      END;
      IF (x < min) OR (x > max) THEN
        t.error (
	  "parameter " & Fmt.Int(t.next-1) & " = " & Fmt.Real(x) &
          " should be in [" & Fmt.Real(min) &
	  " __ " & Fmt.Real(max) & "]."
	)
      END
    END;
    RETURN x
  END GetNextReal;
  
PROCEDURE GetNextLongReal(
    t: T;
    min := FIRST(LONGREAL); 
    max := LAST(LONGREAL)
  ): LONGREAL RAISES {Error} =
  VAR x: LONGREAL;
  BEGIN
    WITH txt = t.getNext() DO
      TRY
        x := Scan.LongReal(txt);
      EXCEPT 
        Lex.Error, FloatMode.Trap =>
        t.error(
	  "parameter " & Fmt.Int(t.next-1) & " = \"" & txt &
	  "\" should be a real number."
	)
      END;
      IF (x < min) OR (x > max) THEN
        t.error (
	  "parameter " & Fmt.Int(t.next-1) & " = " & Fmt.LongReal(x) &
          " should be in [" & Fmt.LongReal(min) &
	  " __ " & Fmt.LongReal(max) & "]."
	)
      END
    END;
    RETURN x
  END GetNextLongReal;

PROCEDURE SkipParsed(t: T) RAISES {Error} =
  CONST MaxBogus = 5;
  VAR bogus: CARDINAL := 0;
  BEGIN
    WITH a = t.arg^, p = t.parsed^ DO
      t.next := NUMBER(a);
      WHILE (t.next > 0) AND NOT p[t.next-1] DO DEC(t.next) END;
      (* Check for unparsed arguments: *)
      FOR i := 0 TO t.next-1 DO
        IF NOT p[i] THEN
          INC (bogus);
          IF bogus <= 5 THEN
            Message(
	      t.wr,
	      "parameter " & Fmt.Int(i) & " = \"" & a[i] & 
	      "\" extraneous or misplaced."
	    );
          END;
        END;
      END;
      IF bogus > MaxBogus THEN
        Message(t.wr, "(and " & Fmt.Int (bogus - MaxBogus) & " more).");
      END;
      IF bogus > 0 THEN RAISE Error END;
    END
  END SkipParsed;

PROCEDURE Finish(t: T) RAISES {Error} =
  CONST MaxBogus = 5;
  VAR bogus: CARDINAL := 0;
  BEGIN
    WITH a = t.arg^, p = t.parsed^ DO
      FOR i := 0 TO LAST(a) DO
        IF NOT p[i] THEN
          INC (bogus);
          IF bogus <= 5 THEN
            Message(
	      t.wr,
	      "parameter " & Fmt.Int(i) & " = \"" & a[i] & 
	      "\" extraneous or misplaced."
	    );
          END;
        END;
      END;
      IF bogus > MaxBogus THEN
        Message(t.wr, "(and " & Fmt.Int (bogus - MaxBogus) & " more).");
      END;
      t.parsed := NIL;
      t.arg := NIL;
      t.wr := NIL;
      IF bogus > 0 THEN RAISE Error END;
    END
  END Finish;

PROCEDURE Message(wr: Wr.T; msg: TEXT) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  BEGIN
    IF (wr # NIL) THEN
      Wr.PutText(wr, "ParseParams: ");
      Wr.PutText(wr, msg);
      Wr.PutChar(wr, '\n');
      Wr.Flush(wr);
    END
  END Message;

PROCEDURE PrintError (t: T; msg: TEXT) RAISES {Error} =
  BEGIN
    Message(t.wr, msg);
    RAISE Error
  END PrintError;

BEGIN
END ParseParams.

