MODULE TextOps;

IMPORT
  ASCII,
  Convert,
  File,
  Fmt,
  RefSeq,
  Text,
  TextClass;

IMPORT
  DateOps,
  TimeOps;

PROCEDURE CAP(ch: CHAR): CHAR =
  BEGIN
    IF ch IN ASCII.Lowers THEN
      ch := ASCII.Upper[ch];
    END;
    RETURN ch;
  END CAP;

<*INLINE*> PROCEDURE LOW(ch: CHAR): CHAR =
  BEGIN
    IF ch IN ASCII.Uppers THEN
      ch := ASCII.Lower[ch];
    END;
    RETURN ch;
  END LOW;

PROCEDURE Caps(t: TEXT): TEXT =
  VAR
    result: TEXT := "";
  BEGIN
    IF t = NIL THEN
      RETURN NIL;
    END;
    IF Text.Length(t) = 0 THEN
      RETURN "";
    END;

    FOR i := 0 TO Text.Length(t)-1 DO
      result := result & Text.FromChar(CAP(Text.GetChar(t, i)));
    END;

    RETURN result;
  END Caps;

PROCEDURE Lows(t: TEXT): TEXT =
  VAR
    result: TEXT := "";
  BEGIN
    IF t = NIL THEN
      RETURN NIL;
    END;
    IF Text.Length(t) = 0 THEN
      RETURN "";
    END;

    FOR i := 0 TO Text.Length(t)-1 DO
      result := result & Text.FromChar(LOW(Text.GetChar(t, i)));
    END;

    RETURN result;
  END Lows;

PROCEDURE FromChars(READONLY chars: ARRAY OF CHAR): TEXT =
  VAR
    r: TEXT := "";
    i: CARDINAL := 0;
  BEGIN
    WHILE (i < NUMBER(chars)) AND (chars[i] # '\000') DO
      r := r & Text.FromChar(chars[i]);
      INC(i);
    END;
    RETURN r;
  END FromChars;
  
PROCEDURE Escape(t: TEXT; s: SET OF CHAR; esc: CHAR := '\\'): TEXT =
  VAR
    res: TEXT := "";
    escT: TEXT;
  BEGIN
    IF Text.Length(t) = 0 THEN
      RETURN t;
    END;
    s := s + SET OF CHAR { esc };

    escT := Text.FromChar(esc);
    FOR i := 0 TO Text.Length(t) - 1 DO
      WITH ch = Text.GetChar(t, i) DO
        IF ch IN s THEN
          res := res & escT;
        END;
        res := res & Text.FromChar(ch);
      END;
    END;

    RETURN res;
  END Escape;

PROCEDURE EscapeS(t, s: TEXT; esc: CHAR := '\134'): TEXT =
  VAR
    chSet := SET OF CHAR {};
  BEGIN
    FOR i := 0 TO Text.Length(s)-1 DO
      chSet := chSet + SET OF CHAR { Text.GetChar(s, i) };
    END;
    RETURN Escape(t, chSet, esc);
  END EscapeS;

PROCEDURE UnEscape(t: TEXT; esc: CHAR := '\134'): TEXT =
  VAR
    res: TEXT := "";
  BEGIN
    IF Text.Length(t) = 0 THEN
      RETURN t;
    END;
    
    FOR i := 0 TO Text.Length(t) - 1 DO
      WITH ch = Text.GetChar(t, i) DO
        IF ch # esc THEN
          res := res & Text.FromChar(ch);
        END;
      END;
    END;

    RETURN res;
  END UnEscape;

PROCEDURE Pos(t, p: TEXT) : CARDINAL =
  BEGIN
    RETURN NextPos(t, p, 0);
  END Pos;

PROCEDURE NextPos(t, p: TEXT; from: CARDINAL) : CARDINAL =
  VAR
    len, curr: CARDINAL;
  BEGIN
    len := Text.Length(p);
    IF len = 0 THEN
      RETURN 0;
    END;
    curr := 0;

    FOR i := from TO Text.Length(t)-1 DO
      IF Text.GetChar(t, i) = Text.GetChar(p, curr) THEN
        IF curr+1=len THEN
          RETURN i-curr;
        END;
        INC(curr);
      ELSE
        curr := 0;
      END;
    END;

    RETURN LAST(CARDINAL);
  END NextPos;


PROCEDURE Item(t: TEXT; s: SET OF CHAR; n: CARDINAL; escape: CHAR := '\134'; skipSucc: BOOLEAN := TRUE): TEXT =
  VAR
    from: CARDINAL;
    len: CARDINAL := Text.Length(t);
    i: CARDINAL := 0;
    out: BOOLEAN := FALSE;
    result: TEXT := "";
  BEGIN
    WHILE skipSucc AND (i < len) AND (Text.GetChar(t, i) IN s) DO
      INC(i);
    END;

    from := i;
    INC(i);
    WHILE (i < len) AND NOT out DO
      IF Text.GetChar(t, i) IN s THEN
        IF NOT skipSucc OR (n = 0) THEN
          result := Text.Sub(t, from, i - from);
          out := TRUE;
        ELSE
          DEC(n);
          WHILE (i < len) AND (Text.GetChar(t, i) IN s) DO
            INC(i);
          END;
          from := i;
        END;
      ELSIF Text.GetChar(t, i) = escape THEN
        INC(i);
      END;
      INC(i);
    END;

    IF (n = 0) AND (from < len) AND NOT out THEN
      result := Text.Sub(t, from);
    END;

    RETURN result;
  END Item;

PROCEDURE ItemS(t: TEXT; s: TEXT; n: CARDINAL; escape: CHAR := '\134'; skipSucc: BOOLEAN := TRUE): TEXT =
  VAR
    chSet := SET OF CHAR {};
  BEGIN
    FOR i := 0 TO Text.Length(s)-1 DO
      chSet := chSet + SET OF CHAR { Text.GetChar(s, i) };
    END;
    RETURN Item(t, chSet, n, escape, skipSucc);
  END ItemS;

PROCEDURE Split(t: TEXT; s: SET OF CHAR; escape: CHAR := '\134'; skipSucc: BOOLEAN := TRUE): RefSeq.T =
  VAR
    res: RefSeq.T := NEW(RefSeq.T).init();
    from: CARDINAL;
    len: CARDINAL := Text.Length(t);
    i: INTEGER := 0;
    str: REF ARRAY OF CHAR := NEW (REF ARRAY OF CHAR, len);
  BEGIN
    t.get_chars(str^, 0);
    
    WHILE skipSucc AND i < len AND str[i] IN s DO
      INC(i);
    END;

    from := i;
    
    IF skipSucc THEN
      INC(i);
    END;

    WHILE (i < len) DO
      (* IF Text.GetChar(t, i) IN s THEN *)
      IF str[i] IN s THEN
        res.addhi(Text.Sub(t, from, i - from));
        IF skipSucc THEN
          WHILE (i < len) AND (str[i] IN s) DO
            INC(i);
          END;
          from := i;
        ELSE
          from := i+1;
        END;
      ELSIF str[i] = escape THEN
        INC(i);
      END;
      INC(i);
    END;

    IF (from < len) THEN
      res.addhi(Text.Sub(t, from));
    END;

    RETURN res;
  END Split;

PROCEDURE SplitS(t: TEXT; s: TEXT; escape: CHAR := '\134'; skipSucc: BOOLEAN := TRUE): RefSeq.T =
  VAR
    chSet := SET OF CHAR {};
  BEGIN
    FOR i := 0 TO Text.Length(s)-1 DO
      chSet := chSet + SET OF CHAR { Text.GetChar(s, i) };
    END;
    RETURN Split(t, chSet, escape, skipSucc);
  END SplitS;

PROCEDURE ToInt(t: TEXT; base: CARDINAL := 10; min: INTEGER := FIRST(INTEGER); max: INTEGER := LAST(INTEGER)): INTEGER =
  VAR
    used, res: INTEGER;
    buf: REF ARRAY OF CHAR;
  BEGIN
    IF t = NIL THEN
      RETURN 0;
    END;
    
    buf := NEW(REF ARRAY OF CHAR, Text.Length(t)+1);
    FOR i := 0 TO Text.Length(t) DO
      buf[i] := '\000';
    END;
    Text.SetChars(buf^, t);
    res := Convert.ToInt(buf^, used, base);
    IF res < min THEN
      res := min;
    ELSIF res > max THEN
      res := max;
    END;
    RETURN res;
  END ToInt;

PROCEDURE ToRReal(t: TEXT): REAL =
  (*
  VAR
    used: INTEGER;
    buf: REF ARRAY OF CHAR;
  BEGIN
    IF t = NIL THEN
      RETURN 0.0;
    END;

    buf := NEW(REF ARRAY OF CHAR, Text.Length(t)+1);
    FOR i := 0 TO Text.Length(t) DO
      buf[i] := '\000';
    END;
    Text.SetChars(buf^, t);
    
    RETURN Convert.ToFloat(buf^, used);
  *)
  BEGIN
    RETURN FLOAT(ToReal(t), REAL);
  END ToRReal;

PROCEDURE ToReal(t: TEXT): LONGREAL =
  (*
  VAR
    used: INTEGER;
    buf: REF ARRAY OF CHAR;
  BEGIN
    IF t = NIL THEN
      RETURN 0.0d0;
    END;

    buf := NEW(REF ARRAY OF CHAR, Text.Length(t)+1);
    FOR i := 0 TO Text.Length(t) DO
      buf[i] := '\000';
    END;
    Text.SetChars(buf^, t);

    RETURN Convert.ToLongFloat(buf^, used);
  *)

  PROCEDURE NumberString2Real(READONLY s: TEXT): LONGREAL =
    VAR
      i, j, k: CARDINAL;
      r: LONGREAL := 0.0d0;
    BEGIN
      i := Text.Length(s);
      j := i;
      WHILE j>8 DO
        DEC(j, 8);
      END;
      k := 0;
      WHILE k<i DO
        r := r*1.0d8+FLOAT(ToInt(Text.Sub(s, k, j)), LONGREAL);
        INC(k, j);
        j := 8;
      END;
      RETURN r;
    END NumberString2Real;
    
  VAR
    s1, s2: TEXT;
    r1, r2: LONGREAL;
    div: LONGREAL := 1.0d0;

  BEGIN
    s1 := ItemS(t, ".", 0);
    s2 := ItemS(t, ".", 1);
    IF Text.Empty(s1) THEN
      s1 := "0";
    END;
    IF Text.Empty(s2) THEN
      s2 := "0";
    END;
    r1 := NumberString2Real(s1);
    r2 := NumberString2Real(s2);
    FOR i := 1 TO Text.Length(s2) DO
      div := div*10.0d0;
    END;
    
    RETURN r1 + (r2 / div);
  END ToReal;

PROCEDURE ToBoolean(t: TEXT): BOOLEAN =
  BEGIN
    t := Caps(t);
    RETURN Text.Equal(t, "TRUE") OR Text.Equal(t, "DA") OR Text.Equal(t, "YES") OR Text.Equal(t, "1");
  END ToBoolean;

PROCEDURE RemoveSpaces(t: TEXT): TEXT =
  CONST
    Spaces = SET OF CHAR { ' ', ASCII.HT, ASCII.VT };
  BEGIN
    RETURN RemoveChars(t, Spaces);
  END RemoveSpaces;

PROCEDURE RemoveChars(t: TEXT; s: SET OF CHAR): TEXT =
  VAR
    i: CARDINAL := 0;
    j: CARDINAL;
  BEGIN
    IF (t = NIL) OR (Text.Length(t) = 0) THEN
      RETURN t;
    END;

    WHILE (i < Text.Length(t)) AND (Text.GetChar(t, i) IN s) DO
      INC(i);
    END;

    j := Text.Length(t)-1;
    WHILE (j > i) AND (Text.GetChar(t, j) IN s) DO
      DEC(j);
    END;

    IF j >= i THEN
      RETURN Text.Sub(t, i, j+1-i);
    ELSE
      RETURN "";
    END;
  END RemoveChars;

PROCEDURE CompareWithNIL(t1, t2: TEXT): [-1..+1] =
  BEGIN
    IF t1 = NIL AND t2 = NIL THEN
      RETURN 0;
    ELSIF t1 # NIL AND t2 = NIL THEN
      RETURN -1;
    ELSIF t1 = NIL AND t2 # NIL THEN
      RETURN 1;
    ELSE
      RETURN Text.Compare(t1, t2);
    END;
  END CompareWithNIL;

PROCEDURE ToText(ra: REFANY): TEXT =
  VAR
    value: TEXT;
  BEGIN
    TYPECASE ra OF
    | TEXT(a) =>
      value := a;

    | REF INTEGER(a) =>
      value := Fmt.Int(a^);

    | REF BOOLEAN(b) =>
      IF b^ THEN
        value := "TRUE";
      ELSE
        value := "FALSE";
      END;

    | REF REAL(a) =>
      value := Fmt.Real(a^);

    | REF LONGREAL(a) =>
      value := Fmt.LongReal(a^);

    | REF EXTENDED(a) =>
      value := Fmt.Extended(a^);

    | TimeOps.T(a) =>
      value := TimeOps.ToText(a);

    | DateOps.T(a) =>
      value := DateOps.ToText(a, DateOps.DefaultMask);
    ELSE
      value := "";
    END;

    RETURN value;
  END ToText;

PROCEDURE F(fmt: TEXT; t1, t2, t3, t4, t5, t6, t7, t8, t9, t10: REFANY := NIL): TEXT =
  BEGIN
    RETURN FN(fmt, ARRAY OF REFANY { t1, t2, t3, t4, t5, t6, t7, t8, t9, t10 });
  END F;

PROCEDURE FN(fmt: TEXT; READONLY t: ARRAY OF REFANY): TEXT =
  CONST
    StartFmt = '%';
    StartSubMask = '"';
    EndSubMask = '"';
    LeftAligned = '-';
    LeadingZero = '0';
    Precision = ':';
    Numbers = SET OF CHAR { '0' .. '9' };
    Dot = '.';
    EndOfText = '\000';
    
  VAR
    i, n: CARDINAL := 0;
    ch: CHAR;
    len: CARDINAL := Text.Length(fmt);

  <* INLINE *> PROCEDURE init() =
    BEGIN
      IF len > 0 THEN
        ch := Text.GetChar(fmt, 0);
      ELSE
        ch := EndOfText;
      END;
    END init;

  <* INLINE *> PROCEDURE nextArgument(): REFANY =
    BEGIN
      INC(n);
      IF n > NUMBER(t) THEN
        RETURN "<-missing-argument->";
      END;
      RETURN t[n-1];
    END nextArgument;

  <* INLINE *> PROCEDURE next() =
    BEGIN
      INC(i);
      IF i < len THEN
        ch := Text.GetChar(fmt, i);
      ELSE
        ch := EndOfText;
      END;
    END next;

  <* INLINE *> PROCEDURE nextNumber(): CARDINAL =
    VAR
      i: CARDINAL := 0;
    BEGIN
      WHILE ch IN Numbers DO
        i := i*10 + ORD(ch) - ORD('0');
        next();
      END;
      RETURN i;
    END nextNumber;

  VAR
    res: TEXT := "";
    leftAligned, leadingZero, hasN1, hasN2, hasPrec: BOOLEAN;
    n1, n2, prec, j: CARDINAL;
    subMask, value: TEXT;
    ra: REFANY;

  BEGIN
    init();
    WHILE NOT ch = EndOfText DO
      IF ch = StartFmt THEN
        next();
        IF ch # StartFmt THEN
          leftAligned := FALSE; leadingZero := FALSE; n1 := 0; n2 := 0; prec := 0;
          hasN1 := FALSE; hasN2 := FALSE; hasPrec := FALSE;

          IF ch = LeftAligned THEN
            leftAligned := TRUE;
            next();
          END;
          IF ch = LeadingZero THEN
            leadingZero := TRUE;
            next();
          END;
          IF ch IN Numbers THEN
            n1 := nextNumber();
            hasN1 := TRUE;
          END;
          IF ch = Dot THEN
            next();
            IF ch IN Numbers THEN
              hasN2 := TRUE;
              n2 := nextNumber();
            END;
          END;
          IF ch = Precision THEN
            next();
            IF ch IN Numbers THEN
              hasPrec := TRUE;
              prec := nextNumber();
            END;
          END;
          IF ch = StartSubMask THEN
            next();
            j := i;
            WHILE ch # EndSubMask DO
              next();
            END;
            subMask := Text.Sub(fmt, j, i-j);
            next();
          ELSE
            subMask := "";
          END;
          next();

          ra := nextArgument();
          IF ra = NIL THEN
            ra := "";
          END;
          
          TYPECASE ra OF
          | TEXT(a) =>
            value := a;
            
          | REF INTEGER(a) =>
            IF NOT hasPrec THEN
              prec := 10;
            END;
            value := Fmt.Int(a^, prec);

          | REF File.Byte(b) =>
            IF NOT hasPrec THEN
              prec := 10;
            END;
            value := Fmt.Int(b^, prec);
            
          | REF BOOLEAN(b) =>
            IF b^ THEN
              value := "TRUE";
            ELSE
              value := "FALSE";
            END;
            
          | REF REAL(a) =>
            IF hasPrec THEN
              value := Fmt.Real(a^, Fmt.Style.Fix, prec);
            ELSE
              value := Fmt.Real(a^);
            END;
            
          | REF LONGREAL(a) =>
            IF hasPrec THEN
              value := Fmt.LongReal(a^, Fmt.Style.Fix, prec);
            ELSE
              value := Fmt.LongReal(a^);
            END;
            
          | REF EXTENDED(a) =>
            IF hasPrec THEN
              value := Fmt.Extended(a^, Fmt.Style.Fix, prec);
            ELSE
              value := Fmt.Extended(a^);
            END;
            
          | TimeOps.T(a) =>
            value := TimeOps.ToText(a);
            
          | DateOps.T(a) =>
            IF Text.Empty(subMask) THEN
              subMask := DateOps.DefaultMask;
            END;
            value := DateOps.ToText(a, subMask);
          ELSE
            value := "";
          END;

          IF leadingZero AND hasN1 THEN
            FOR i := Text.Length(value)+1 TO n1 DO
              value := "0" & value;
            END;
          END;

          IF hasN1 THEN
            IF leftAligned THEN
              value := Fmt.Pad(value, n1, ' ', Fmt.Align.Left);
            ELSE
              value := Fmt.Pad(value, n1);
            END;
          END;

          IF hasN2 THEN
            value := Text.Sub(value, 0, n2);
          END;

          res := res & value;
        ELSE
          res := res & Text.FromChar(StartFmt);
          next();
        END;
      ELSE
        res := res & Text.FromChar(ch);
        next();
      END;
    END;

    RETURN res;
  END FN;

PROCEDURE Match(text, pattern: TEXT): BOOLEAN =
  VAR
    tP, pP,
    tLen, pLen: CARDINAL;

  PROCEDURE WildMat(tP, pP: CARDINAL): BOOLEAN =
    VAR
      last: CARDINAL;
      matched,
      reverse: BOOLEAN;
    BEGIN
      WHILE pP # pLen DO
        CASE Text.GetChar(pattern, pP) OF
        | '\134' =>
          (* Literal match with following character.
          *)
          INC (pP);
          IF (tP = tLen) OR (Text.GetChar(text, tP) # Text.GetChar(pattern, pP)) THEN
            RETURN FALSE;
          END;
        | '?' =>
          (* Match anything.
          *)
          IF (tP = tLen) THEN
            RETURN FALSE;
          END;
        | '*' =>
          IF pP+1 = pLen THEN
            (* Trailing star matches everything.
            *)
            RETURN TRUE;
          ELSE
            RETURN Star(tP, pP+1);
          END;
        | '[' =>
          (* [^....] means inverse character class.
          *)
          IF (tP = tLen) THEN
            RETURN FALSE;
          END;
          reverse := Text.GetChar(pattern, pP+1) = '^';
          IF reverse THEN
            INC (pP);
          END;
          last := 256;
          matched := FALSE;
          INC (pP);
          WHILE (pP # pLen) AND (Text.GetChar(pattern, pP) # ']') DO
            (* This next line requires a good C compiler. *)
            (* And an even better Pascal compiler. -- DRC *)
            IF (Text.GetChar(pattern, pP) = '-') THEN
              INC (pP);
              IF ((Text.GetChar(text, tP) <= Text.GetChar(pattern, pP)) AND (ORD(Text.GetChar(text, tP)) >= last)) THEN
                matched := TRUE;
              END;
            ELSE
              IF (Text.GetChar(text, tP) = Text.GetChar(pattern, pP)) THEN
                matched := TRUE;
              END;
            END;
            last := ORD(Text.GetChar(pattern, pP));
            INC (pP);
          END;
          IF (matched = reverse) THEN
            RETURN FALSE;
          END;
        ELSE
          IF (tP = tLen) OR (CAP(Text.GetChar(text, tP)) # CAP(Text.GetChar(pattern, pP))) THEN
            RETURN FALSE;
          END;
        END;
        INC (tP);
        INC (pP);
      END;
      RETURN tP=tLen;
    END WildMat;

    PROCEDURE Star(tP, pP: CARDINAL): BOOLEAN =
      BEGIN
        WHILE NOT WildMat(tP, pP) DO
          INC (tP);
          IF (tP = tLen) THEN
            RETURN FALSE;
          END;
        END;
        RETURN TRUE;
      END Star;

  BEGIN
    tP := 0;
    pP := 0;
    tLen := Text.Length(text);
    pLen := Text.Length(pattern);
    RETURN WildMat(tP, pP);
  END Match;

BEGIN
END TextOps.
