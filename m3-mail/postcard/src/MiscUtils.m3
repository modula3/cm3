(* Copyright 1989 Digital Equipment Corporation.  *) (* Distributed
only by permission.  *) (* OSUtils.mod *) (* Last modified on Tue Aug
11 14:56:45 PDT 1992 by birrell *)

MODULE MiscUtils;

IMPORT ASCII, Rd, Text,
       TextExtras, TextUtils, Thread, Wr;

(* *)
(* Operations on TEXT, Rd.T and Wr.T *)
(* *)

PROCEDURE ToInt(t: TEXT): INTEGER RAISES { BadFormat } =
  VAR i := 0; pos := 0; len := Text.Length(t); neg := FALSE;
      c : REF ARRAY OF CHAR;
  CONST White = SET OF CHAR{' ', '\n', '\t'};
  BEGIN
    c := NEW(REF ARRAY OF CHAR, len);
    Text.SetChars(c^,t);
    LOOP
      IF pos >= len THEN RAISE BadFormat END;
      IF NOT (c^[pos] IN White) THEN EXIT END;
      INC(pos);
    END;
    IF c^[pos] = '+' THEN INC(pos)
    ELSIF c^[pos] = '-' THEN neg := TRUE; INC(pos)
    END;
    LOOP
      IF pos >= len THEN RAISE BadFormat END;
      IF NOT (c^[pos] IN White) THEN EXIT END;
      INC(pos);
    END;
    LOOP
      IF c^[pos] >= '0' AND c^[pos] <= '9' THEN
        IF neg THEN (* get most-negative integer correct *)
          i := i * 10 - (ORD(c^[pos]) - ORD('0'));
        ELSE
          i := i * 10 + (ORD(c^[pos]) - ORD('0'));
        END;
        INC(pos);
      ELSIF c^[pos] IN White THEN
        EXIT
      ELSE
        RAISE BadFormat
      END;
      IF pos >= len THEN EXIT END;
    END;
    RETURN i
  END ToInt;

PROCEDURE Replace(dest: TEXT; offset, length: CARDINAL; source: TEXT): TEXT =
  BEGIN
    RETURN Text.Sub(dest, 0, offset) & source &
           Text.Sub(dest, offset+length, LAST(CARDINAL))
  END Replace;

PROCEDURE PutTextSub(wr: Wr.T; t: TEXT; start: CARDINAL;
                     length: CARDINAL := LAST(CARDINAL))
        RAISES { Wr.Failure, Thread.Alerted } =
  VAR
    txt : TEXT;
    c : REF ARRAY OF CHAR;
  BEGIN
    c := NEW(REF ARRAY OF CHAR, Text.Length(t));
    txt := Text.Sub(t,start,MIN(length,Text.Length(t)));
    Text.SetChars(c^,txt);
    Wr.PutString(wr, c^);
  END PutTextSub;

PROCEDURE Equal(t, u: TEXT; ignoreCase: BOOLEAN := FALSE): BOOLEAN =
(* Return TRUE iff t and u have the same length and would have the same
   contents if all their upper case letters were mapped to lower case by
   indexing ASCII.Lower *)
  BEGIN
    IF ignoreCase THEN
      RETURN TextExtras.Compare(t,u) # 0;
    ELSE
      RETURN Text.Equal(t, u)
    END;
  END Equal;

PROCEDURE Find(txt: TEXT; start: CARDINAL; pat: TEXT;
               ignoreCase: BOOLEAN := FALSE): INTEGER =
  BEGIN
    IF start > 0 THEN
      txt := Text.Sub(txt,start);
    END;
    RETURN TextUtils.Pos(txt,pat,NOT ignoreCase);
  END Find;

PROCEDURE FindInSub(READONLY sub: ARRAY OF CHAR; pat: TEXT;
                    ignoreCase: BOOLEAN := FALSE): INTEGER =
  VAR t : TEXT;
  BEGIN
    t := Text.FromChars(sub);
    RETURN TextUtils.Pos(t,pat, NOT ignoreCase);
  END FindInSub;

PROCEDURE RdFindChar(rd: Rd.T; pat: CHAR;
                     ignoreCase: BOOLEAN := FALSE): INTEGER
                     RAISES {Rd.Failure, Thread.Alerted} =
(* Finds the first occurrence of pat, reading forward from the
   current position of rd.  If no match is found, Find returns -1 and
   leaves rd positioned at the end.  If Failure or Error is raised while
   reading characters from rd, the exception propagates through to the
   caller of Find and the position of rd is undefined.  If a match is
   found, Find returns the index of the matching character and
   leaves rd positioned to read the character following the match.  If
   ignoreCase, Find treats upper case characters as lower case in both rd
   and pat. *)
  VAR c: CHAR; lowerPat := ASCII.Lower[pat];
  BEGIN
    TRY
      LOOP
        c := Rd.GetChar(rd);
        IF ignoreCase THEN
          IF ASCII.Lower[c] = lowerPat THEN RETURN Rd.Index(rd)-1 END;
        ELSE
          IF c = pat THEN RETURN Rd.Index(rd)-1 END;
        END;
      END;
    EXCEPT Rd.EndOfFile =>
    END;
    RETURN -1
  END RdFindChar;

BEGIN
END MiscUtils.
