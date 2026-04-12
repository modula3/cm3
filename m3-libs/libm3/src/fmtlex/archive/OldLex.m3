(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Thu Jan 26 14:08:08 PST 1995 by kalsow                   *)
(*      modified on Tue Feb 11 17:00:40 PST 1992 by muller                   *)

MODULE OldLex;
IMPORT Text, Rd, TextRd, Convert, Thread;
<*FATAL Thread.Alerted, Rd.EndOfFile*>

  PROCEDURE Scan(rd: Rd.T; READONLY cs: SET OF CHAR := NonBlanks): TEXT 
    RAISES {Rd.Failure} =
  CONST BufSize=256;
  VAR c: CHAR; i: INTEGER; t: TEXT;
    buf: ARRAY [0..BufSize-1] OF CHAR;
  BEGIN
    t := "";
    i := 0;
    LOOP
      IF Rd.EOF(rd) THEN EXIT END;
      c := Rd.GetChar(rd);
      IF c IN cs THEN
	IF i=BufSize THEN
	  t := t & Text.FromChars(buf);
	  i := 0;
	END;
        buf[i] := c; INC(i);
      ELSE
	Rd.UnGetChar(rd);
	EXIT;
      END;
    END;
    RETURN t & Text.FromChars(SUBARRAY(buf, 0, i));
  END Scan;

  PROCEDURE Skip(rd: Rd.T; READONLY cs: SET OF CHAR := Blanks) 
    RAISES {Rd.Failure} =
  VAR c: CHAR;
  BEGIN
    LOOP
      IF Rd.EOF(rd) THEN EXIT END;
      c := Rd.GetChar(rd);
      IF NOT (c IN cs) THEN
	Rd.UnGetChar(rd);
	EXIT;
      END;
    END;
  END Skip;

  PROCEDURE Match(rd: Rd.T; t: TEXT) RAISES {Rd.Failure, Error} =
  VAR rd1: Rd.T;
  BEGIN
    rd1 := TextRd.New(t);
    LOOP
      IF Rd.EOF(rd1) THEN EXIT END;
      IF Rd.EOF(rd) THEN RAISE Error END;
      IF Rd.GetChar(rd) # Rd.GetChar(rd1) THEN RAISE Error END;
    END;
  END Match;

  PROCEDURE Bool(rd: Rd.T; READONLY cs: SET OF CHAR := NonBlanks): BOOLEAN 
    RAISES {Rd.Failure, Error} =
  CONST BufSize=6 (* Max(Length("true"),Length("false"))+1 *);
  VAR c: CHAR; i: INTEGER;
    buf: ARRAY [0..BufSize-1] OF CHAR;
  BEGIN
    i := 0;
    LOOP
      IF Rd.EOF(rd) THEN EXIT END;
      c := Rd.GetChar(rd);
      IF c IN cs THEN
	IF i<BufSize THEN
	  buf[i] := c; INC(i);
	ELSE (* Just keep going *) END;
      ELSE
	Rd.UnGetChar(rd);
	EXIT;
      END;
    END;
    IF (buf[0]='T') OR (buf[0]='t')
    THEN
      IF i=1 THEN RETURN TRUE ELSE RAISE Error END;
    END;
    IF (buf[0]='F') OR (buf[0]='f')
    THEN
      IF i=1 THEN RETURN FALSE ELSE RAISE Error END;
    END;
    IF (SUBARRAY(buf, 0, 4)=ARRAY[0..3] OF CHAR {'T','R','U','E'})
       OR (SUBARRAY(buf, 0, 4)=ARRAY[0..3] OF CHAR {'T','r','u','e'})
       OR (SUBARRAY(buf, 0, 4)=ARRAY[0..3] OF CHAR {'t','r','u','e'})
    THEN
      IF i=4 THEN RETURN TRUE ELSE RAISE Error END;
    END;
    IF (SUBARRAY(buf, 0, 5)=ARRAY[0..4] OF CHAR {'F','A','L','S','E'})
       OR (SUBARRAY(buf, 0, 5)=ARRAY[0..4] OF CHAR {'F','a','l','s','e'})
       OR (SUBARRAY(buf, 0, 5)=ARRAY[0..4] OF CHAR {'f','a','l','s','e'})
    THEN
      IF i=5 THEN RETURN FALSE ELSE RAISE Error END;
    END;
    RAISE Error;
  END Bool;

  PROCEDURE Int(rd: Rd.T; base: Convert.Base := 10; READONLY cs: SET OF CHAR := NonBlanks)
    : INTEGER 
    RAISES {Rd.Failure, Error} =
  CONST BufSize=256;
  VAR c: CHAR; i, used: INTEGER; n: INTEGER;
    buf: ARRAY [0..BufSize-1] OF CHAR;
  BEGIN
    i := 0;
    LOOP
      IF Rd.EOF(rd) THEN EXIT END;
      c := Rd.GetChar(rd);
      IF c IN cs THEN
	IF i<BufSize THEN
	  buf[i] := c; INC(i);
	ELSE (* Just keep going *) END;
      ELSE
	Rd.UnGetChar(rd);
	EXIT;
      END;
    END;
    n := Convert.ToInt(SUBARRAY(buf, 0, i), (*VAR*) used, base);
    IF used # i THEN RAISE Error END;
    RETURN n;
  END Int;

  PROCEDURE Unsigned(rd: Rd.T; base: Convert.Base := 10; 
    READONLY cs: SET OF CHAR := NonBlanks): INTEGER 
    RAISES {Rd.Failure, Error} =
  CONST BufSize=256;
  VAR c: CHAR; i, used: INTEGER; n: INTEGER;
    buf: ARRAY [0..BufSize-1] OF CHAR;
  BEGIN
    i := 0;
    LOOP
      IF Rd.EOF(rd) THEN EXIT END;
      c := Rd.GetChar(rd);
      IF c IN cs THEN
	IF i<BufSize THEN
	  buf[i] := c; INC(i);
	ELSE (* Just keep going *) END;
      ELSE
	Rd.UnGetChar(rd);
	EXIT;
      END;
    END;
    n := Convert.ToUnsigned(SUBARRAY(buf, 0, i), (*VAR*) used, base);
    IF used # i THEN RAISE Error END;
    RETURN n;
  END Unsigned;

  PROCEDURE Real(rd: Rd.T; READONLY cs: SET OF CHAR := NonBlanks): REAL 
    RAISES {Rd.Failure, Convert.Failed, Error} =
  CONST BufSize=256;
  VAR c: CHAR; i, used: INTEGER; r: REAL;
    buf: ARRAY [0..BufSize-1] OF CHAR;
  BEGIN
    i := 0;
    LOOP
      IF Rd.EOF(rd) THEN EXIT END;
      c := Rd.GetChar(rd);
      IF c IN cs THEN
	IF i<BufSize THEN
	  buf[i] := c; INC(i);
	ELSE (* Just keep going *) END;
      ELSE
	Rd.UnGetChar(rd);
	EXIT;
      END;
    END;
    r := Convert.ToFloat(SUBARRAY(buf, 0, i), (*VAR*) used);
    IF used # i THEN RAISE Error END;
    RETURN r;
  END Real;

  PROCEDURE LongReal(rd: Rd.T; READONLY cs: SET OF CHAR := NonBlanks): LONGREAL 
    RAISES {Rd.Failure, Convert.Failed, Error} =
  CONST BufSize=256;
  VAR c: CHAR; i, used: INTEGER; r: LONGREAL;
    buf: ARRAY [0..BufSize-1] OF CHAR;
  BEGIN
    i := 0;
    LOOP
      IF Rd.EOF(rd) THEN EXIT END;
      c := Rd.GetChar(rd);
      IF c IN cs THEN
	IF i<BufSize THEN
	  buf[i] := c; INC(i);
	ELSE (* Just keep going *) END;
      ELSE
	Rd.UnGetChar(rd);
	EXIT;
      END;
    END;
    r := Convert.ToLongFloat(SUBARRAY(buf, 0, i), (*VAR*) used);
    IF used # i THEN RAISE Error END;
    RETURN r;
  END LongReal;

BEGIN
END OldLex.
