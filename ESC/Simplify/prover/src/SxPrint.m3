(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
UNSAFE MODULE SxPrint EXPORTS SxPrint;
(* UNSAFE only because of loopholing a RTO.String to a
   CTypes.char_STAR *)

IMPORT Atom, Ctypes, Enode, Fmt, M3toC, MatchingRule,
       RefList,RTType, Sx, Text, Wr;

IMPORT Thread;
<* FATAL Wr.Failure, Thread.Alerted *>

CONST
  SLASH         = '\\';
  BAR           = '|';
  SQUOTE        = '\'';
  DQUOTE        = '"';
  DIGITS        = SET OF CHAR {'0'.. '9'};
  LETTERS       = SET OF CHAR {'a'.. 'z', 'A'.. 'Z'};
  ALPHANUMERICS = LETTERS + DIGITS;

CONST
  ATOM_CHARS = SET OF
                 CHAR {
                 '!', '#', '$', '%', '&', '*', '+', '-', '.', '/', ':', '<',
                 '=', '>', '?', '@', '[', ']', '^', '_', '{', '}', '~'};
  ID_CHARS = ALPHANUMERICS + SET OF CHAR {'_'};

PROCEDURE Print(wr       : Wr.T;
                 ra       : REFANY;
                 READONLY sub: MatchingRule.Substitution
                                 := MatchingRule.EmptySub;
                 maxDepth : CARDINAL := LAST(CARDINAL);
                 maxLength: CARDINAL := LAST(CARDINAL)  ) =
  CONST
    DEPTH_ELLIPSIS  = "...";
    LENGTH_ELLIPSIS = "...";
  VAR sx := LOOPHOLE(ra, Sx.T); BEGIN
    TRY
      TYPECASE sx OF
      | NULL => Wr.PutText(wr, "()")
      | REF INTEGER(r) => Wr.PutText(wr, Fmt.Int(r^))
      | REF CHAR(r) =>
          Wr.PutChar(wr, SQUOTE);
          PrintChar(wr, r^, SQUOTE);
          Wr.PutChar(wr, SQUOTE)
      | REF REAL(r) =>
          (* Wr.PutText(wr, Fmt.Real(r^, modula := TRUE)) *)
          Wr.PutText(wr, Fmt.Real(r^, Fmt.Style.Auto, literal := TRUE))
      | REF LONGREAL(r) =>
          (* Wr.PutText(wr, Fmt.LongReal(r^, modula := TRUE)) *)
          Wr.PutText(wr, Fmt.LongReal(r^, Fmt.Style.Auto, literal := TRUE))
      | REF EXTENDED(r) =>
          (* Wr.PutText(wr, Fmt.Extended(r^, modula := TRUE)) *)
          Wr.PutText(wr, Fmt.Extended(r^, Fmt.Style.Auto, literal := TRUE))
      | TEXT(t) =>
          Wr.PutChar(wr, DQUOTE);
          FOR i := 0 TO Text.Length(t) - 1 DO
            PrintChar(wr, Text.GetChar(t, i), DQUOTE)
          END;
          Wr.PutChar(wr, DQUOTE)
      | Atom.T(a) =>
          VAR name := Atom.ToText(a);
          BEGIN
            IF NeedsBars(name) THEN
              Wr.PutChar(wr, BAR);
              FOR i := 0 TO Text.Length(name) - 1 DO
                PrintChar(wr, Text.GetChar(name, i), BAR)
              END;
              Wr.PutChar(wr, BAR)
            ELSE
              Wr.PutText(wr, name)
            END
          END
      | RefList.T(list) =>
          IF maxDepth = 0 THEN
            Wr.PutText(wr, DEPTH_ELLIPSIS)
          ELSE
            VAR len := maxLength;
            BEGIN
              Wr.PutChar(wr, '(');
                DEC(maxDepth);
              LOOP
                Print(wr, list.head, sub, maxDepth, maxLength);
                list := list.tail;
                IF list = NIL THEN EXIT END;
                Wr.PutChar(wr, ' ');
                IF len = 0 THEN Wr.PutText(wr, LENGTH_ELLIPSIS); EXIT END;
                DEC(len)
              END; (* LOOP *)
              Wr.PutChar(wr, ')')
            END
          END
      | Enode.T(e) =>
          Print(wr, Enode.DbgToSx(e), sub, maxDepth, maxLength)
      | MatchingRule.PatVar(v) =>
          Wr.PutText(wr, "|$$" & Fmt.Int(v^));
          IF sub # MatchingRule.EmptySub THEN
            Wr.PutText(wr, ": ");
            IF v^ >= FIRST(sub) AND v^ <= LAST(sub) THEN
              Print(wr, Enode.DbgToSx(sub[v^]), sub, maxDepth, maxLength);
            ELSE
              Wr.PutText(wr, "<* out-of-bounds *>")
            END;
          END;
          Wr.PutText(wr, "|");
      ELSE
        Wr.PutText(wr, "<* unprintable '");
        Wr.PutText(wr,
          M3toC.StoT(
            LOOPHOLE(RTType.Get(TYPECODE(sx)).name, Ctypes.char_star)));
        Wr.PutText(wr, " *>");
      END
    EXCEPT ELSE
      Wr.PutText(wr, "<* ... unprintable '");
      Wr.PutText(wr,
        M3toC.StoT(
          LOOPHOLE(RTType.Get(TYPECODE(sx)).name, Ctypes.char_star)));
      Wr.PutText(wr, " ... *>");
    END
  END Print; 

PROCEDURE PrintChar(wr: Wr.T; ch: CHAR; delim: CHAR)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF ch = '\n' THEN
      Wr.PutText(wr, "\\n")
    ELSIF ch = '\t' THEN
      Wr.PutText(wr, "\\t")
    ELSIF ch = '\r' THEN
      Wr.PutText(wr, "\\r")
    ELSIF ch = '\f' THEN
      Wr.PutText(wr, "\\f")
    ELSIF ch = SLASH THEN
      Wr.PutText(wr, "\\\\")
    ELSIF ch = delim THEN
      Wr.PutChar(wr, SLASH);
      Wr.PutChar(wr, ch)
    ELSIF ISO_Latin_printing(ch) THEN
      Wr.PutText(wr, Text.FromChar(ch))
    ELSE
      Wr.PutText(wr, Fmt.F("\\%03s", Fmt.Int(ORD(ch), 8)))
    END
  END PrintChar;

PROCEDURE ISO_Latin_printing(ch: CHAR): BOOLEAN =
  BEGIN
    RETURN ' ' <= ch AND ch <= '~' OR '\241' <= ch AND ch <= '\377'
  END ISO_Latin_printing;

PROCEDURE NeedsBars(t: TEXT): BOOLEAN =
  VAR
    len       := Text.Length(t);
    c  : CHAR;
  BEGIN
    IF len = 0 THEN RETURN TRUE END;(* || *)
    c := Text.GetChar(t, 0);
    IF c IN LETTERS THEN
      FOR i := 1 TO len - 1 DO
        c := Text.GetChar(t, i);
        IF NOT c IN ID_CHARS THEN RETURN TRUE END
      END;
      RETURN FALSE
    ELSIF c IN ATOM_CHARS THEN
      FOR i := 1 TO len - 1 DO
        c := Text.GetChar(t, i);
        IF NOT c IN ATOM_CHARS THEN RETURN TRUE END
      END;
      RETURN FALSE
    ELSE
      RETURN TRUE
    END
  END NeedsBars;

BEGIN
END SxPrint.
