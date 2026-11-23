(* The part of RTDebug that formats values for debuggers. *)

UNSAFE MODULE RTDebugFormat EXPORTS RTDebug;

IMPORT RT0, RTType, Text, Word;

CONST
  BufLen  = 1024;
  MaxText = 200;   (* characters of a TEXT shown *)
  MaxDims = 8;     (* dimensions of an open array shown *)

VAR
  buf : ARRAY [0 .. BufLen - 1] OF CHAR;
  len : CARDINAL := 0;

PROCEDURE TypecodeOf (r: ADDRESS;  VAR tc: INTEGER): BOOLEAN =
  BEGIN
    IF (r = NIL)
      OR (Word.And (LOOPHOLE (r, Word.T), ADRSIZE (ADDRESS) - 1) # 0) THEN
      RETURN FALSE;
    END;
    tc := TYPECODE (LOOPHOLE (r, REFANY));
    RETURN (0 <= tc) AND (tc <= RTType.MaxTypecode ())
             AND (RTType.Get (tc) # NIL);
  END TypecodeOf;

PROCEDURE TypeName (r: ADDRESS): ADDRESS =
  VAR tc: INTEGER;
  BEGIN
    IF NOT TypecodeOf (r, tc) THEN RETURN NIL; END;
    RETURN RTType.Get (tc).name;
  END TypeName;

PROCEDURE Format (r: ADDRESS): ADDRESS =
  VAR tc: INTEGER;  def: RT0.TypeDefn;
  BEGIN
    len := 0;
    IF (r = NIL) THEN
      PutStr ("NIL");
    ELSIF Word.And (LOOPHOLE (r, Word.T), 1) # 0 THEN
      PutStr ("tagged ");  PutHex (LOOPHOLE (r, Word.T));
    ELSIF NOT TypecodeOf (r, tc) THEN
      RETURN NIL;
    ELSIF RTType.IsSubtype (tc, TYPECODE (TEXT)) THEN
      PutText (LOOPHOLE (r, TEXT));
    ELSE
      def := RTType.Get (tc);
      PutName (def.name);
      PutStr (" (typecode ");  PutInt (tc);  PutStr (")");
      IF (def.kind = ORD (RT0.TypeKind.Array)) THEN
        PutShape (r, LOOPHOLE (def, RT0.ArrayTypeDefn).nDimensions);
      ELSE
        PutStr (", ");  PutInt (def.dataSize);  PutStr (" bytes");
      END;
    END;
    buf[MIN (len, BufLen - 1)] := '\000';
    RETURN ADR (buf[0]);
  END Format;

PROCEDURE PutShape (r: ADDRESS;  n: INTEGER) =
  VAR p: UNTRACED REF INTEGER := r + ADRSIZE (ADDRESS);
  BEGIN
    PutStr (" [");
    FOR i := 0 TO MIN (n, MaxDims) - 1 DO
      IF (i > 0) THEN PutStr (", "); END;
      PutInt (p^);
      INC (p, ADRSIZE (INTEGER));
    END;
    IF (n > MaxDims) THEN PutStr (", ..."); END;
    PutStr ("]");
  END PutShape;

PROCEDURE PutText (t: TEXT) =
  VAR n := Text.Length (t);  c: WIDECHAR;
  BEGIN
    PutChar ('"');
    FOR i := 0 TO MIN (n, MaxText) - 1 DO
      c := Text.GetWideChar (t, i);
      IF (c = W'"') OR (c = W'\\') THEN
        PutChar ('\\');  PutChar (VAL (ORD (c), CHAR));
      ELSIF (W' ' <= c) AND (c <= W'~') THEN
        PutChar (VAL (ORD (c), CHAR));
      ELSIF (c = W'\n') THEN
        PutStr ("\\n");
      ELSIF (c = W'\t') THEN
        PutStr ("\\t");
      ELSE
        PutStr ("\\x");  PutHex (ORD (c));
      END;
    END;
    PutChar ('"');
    IF (n > MaxText) THEN
      PutStr ("... (");  PutInt (n);  PutStr (" characters)");
    END;
  END PutText;

PROCEDURE PutName (name: RT0.String) =
  BEGIN
    IF (name = NIL) OR (name^ = '\000') THEN
      PutStr ("<anonymous>");
    ELSE
      WHILE (name^ # '\000') DO
        PutChar (name^);  INC (name, ADRSIZE (name^));
      END;
    END;
  END PutName;

PROCEDURE PutStr (s: TEXT) =
  BEGIN
    FOR i := 0 TO Text.Length (s) - 1 DO PutChar (Text.GetChar (s, i)); END;
  END PutStr;

PROCEDURE PutChar (c: CHAR) =
  BEGIN
    IF (len < BufLen - 1) THEN buf[len] := c;  INC (len); END;
  END PutChar;

PROCEDURE PutInt (i: INTEGER) =
  VAR d: ARRAY [0 .. 20] OF CHAR;  n := 0;  w: Word.T := i;
  BEGIN
    IF (i < 0) THEN PutChar ('-');  w := Word.Minus (0, i); END;
    REPEAT
      d[n] := VAL (ORD ('0') + Word.Mod (w, 10), CHAR);
      w := Word.Divide (w, 10);  INC (n);
    UNTIL (w = 0);
    WHILE (n > 0) DO DEC (n);  PutChar (d[n]); END;
  END PutInt;

PROCEDURE PutHex (w: Word.T) =
  CONST Digits = ARRAY [0 .. 15] OF CHAR {'0', '1', '2', '3', '4', '5', '6',
                   '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};
  VAR d: ARRAY [0 .. 15] OF CHAR;  n := 0;
  BEGIN
    REPEAT
      d[n] := Digits[Word.And (w, 15)];  w := Word.RightShift (w, 4);  INC (n);
    UNTIL (w = 0);
    PutStr ("16_");
    WHILE (n > 0) DO DEC (n);  PutChar (d[n]); END;
  END PutHex;

BEGIN
END RTDebugFormat.
