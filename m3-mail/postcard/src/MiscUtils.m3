(* Copyright 1989 Digital Equipment Corporation.  *) (* Distributed
only by permission.  *) (* OSUtils.mod *) (* Last modified on Tue Aug
11 14:56:45 PDT 1992 by birrell *)

MODULE MiscUtils;

IMPORT ASCII, Rd, Text, TextF (* REVEAL Text = BRANDED REF ARRAY OF CHAR *),
       Thread, Wr;

(* *)
(* Operations on TEXT, Rd.T and Wr.T *)
(* *)

PROCEDURE ToInt(t: TEXT): INTEGER RAISES { BadFormat } =
  VAR i := 0; pos := 0; len := Text.Length(t); neg := FALSE;
  CONST White = SET OF CHAR{' ', '\n', '\t'};
  BEGIN
    LOOP
      IF pos >= len THEN RAISE BadFormat END;
      IF NOT (t[pos] IN White) THEN EXIT END;
      INC(pos);
    END;
    IF t[pos] = '+' THEN INC(pos)
    ELSIF t[pos] = '-' THEN neg := TRUE; INC(pos)
    END;
    LOOP
      IF pos >= len THEN RAISE BadFormat END;
      IF NOT (t[pos] IN White) THEN EXIT END;
      INC(pos);
    END;
    LOOP
      IF t[pos] >= '0' AND t[pos] <= '9' THEN
        IF neg THEN (* get most-negative integer correct *)
          i := i * 10 - (ORD(t[pos]) - ORD('0'));
        ELSE
          i := i * 10 + (ORD(t[pos]) - ORD('0'));
        END;
        INC(pos);
      ELSIF t[pos] IN White THEN
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
  BEGIN
    Wr.PutString(wr, SUBARRAY(t^, start, MIN(length, NUMBER(t^)-start)));
  END PutTextSub;

PROCEDURE Equal(t, u: TEXT; ignoreCase: BOOLEAN := FALSE): BOOLEAN =
(* Return TRUE iff t and u have the same length and would have the same
   contents if all their upper case letters were mapped to lower case by
   indexing ASCII.Lower *)
  VAR len: CARDINAL;
  BEGIN
    IF ignoreCase THEN
      len := Text.Length(t);
      IF len # Text.Length(u) THEN
        RETURN FALSE
      ELSE
        FOR i := 0 TO len DO
          IF ASCII.Lower[t[i]] # ASCII.Lower[u[i]] THEN RETURN FALSE END;
        END;
        RETURN TRUE
      END
    ELSE
      RETURN Text.Equal(t, u)
    END;
  END Equal;

PROCEDURE Find(txt: TEXT; start: CARDINAL; pat: TEXT;
               ignoreCase: BOOLEAN := FALSE): INTEGER =
  VAR found: INTEGER;
  BEGIN
    found := FindInSub(SUBARRAY(txt^, start, MAX(0,Text.Length(txt)-start)),
                     pat, ignoreCase);
    IF found < 0 THEN RETURN found ELSE RETURN found + start END;
  END Find;

PROCEDURE FindInSub(READONLY sub: ARRAY OF CHAR; pat: TEXT;
                    ignoreCase: BOOLEAN := FALSE): INTEGER =
  VAR
    len := Text.Length(pat);
    txtlen := NUMBER(sub);
    tbl := ARRAY CHAR OF INTEGER{len, ..};
    p := len-1;
  BEGIN
    IF len = 0 THEN RETURN 0 END;
    FOR i := 0 TO len-1 DO
      IF ignoreCase THEN
        tbl[ASCII.Lower[pat[i]]] := len - i - 1;
        tbl[ASCII.Upper[pat[i]]] := len - i - 1;
      ELSE
        tbl[pat[i]] := len - i - 1;
      END;
    END;
    WHILE p < txtlen DO
      VAR m := tbl[sub[p]]; BEGIN
        IF m = 0 THEN
          (* test for match ending at sub[p] *)
          VAR j := 0; startOfMatch := p-(len-1); BEGIN
            IF ignoreCase THEN
              WHILE j # len AND
                  ASCII.Lower[pat[j]] = ASCII.Lower[sub[startOfMatch+j]] DO
                INC(j);
              END;
            ELSE
              WHILE j # len AND pat[j] = sub[startOfMatch+j] DO INC(j) END;
            END;
            IF j = len THEN RETURN startOfMatch ELSE INC(p) END
          END
        ELSE
          INC(p, m)
        END
      END
    END;
    RETURN -1
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
