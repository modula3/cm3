(* Copyright © 1993 Digital Equipment Corporation.                      *)
(* Distributed only by permission.                                       *)
(* See the file COPYRIGHT for a full description.                        *)
(* Last modified on Thu Feb  2 13:54:05 PST 1995 by kalsow               *)
(*      modified on Mon Aug  2 16:21:04 PDT 1993 by mhb                  *)
(*      modified on Mon Jun 21 11:15:48 PDT 1993 by wobber               *)
(*      modified on Fri Jun  4 15:49:01 PDT 1993 by meehan               *)
(*      modified on Thu Feb 18 09:03:27 PST 1993 by mjordan              *)
<* PRAGMA LL                                                             *>
<* PRAGMA EXPORTED                                                       *>

MODULE Sx;

IMPORT Atom, FloatMode, Fmt, Lex, Rd, RdClass, RdUtils, RefList, Text, Thread,
       Wr;

CONST
  MinBoxedInt   = -100;
  MaxBoxedInt   = 100;
  SLASH         = '\\';
  BAR           = '|';
  SQUOTE        = '\'';
  DQUOTE        = '"';
  DIGITS        = SET OF CHAR {'0'.. '9'};
  LETTERS       = SET OF CHAR {'a'.. 'z', 'A'.. 'Z'};
  ALPHANUMERICS = LETTERS + DIGITS;
  BLANKS = SET OF CHAR {'\f', '\r', ' ', '\n', '\t', '\013'};
  NOREADMACROS = BLANKS + SET OF CHAR {';'};

EXCEPTION SetReadMacroError;
<* FATAL SetReadMacroError *>

VAR
  BoxedInts  := ARRAY [MinBoxedInt .. MaxBoxedInt] OF REF INTEGER {NIL, ..};
  BoxedChars := ARRAY CHAR OF REF CHAR {NIL, ..};
  BoxedReals := ARRAY [-1 .. 1] OF REF REAL {NIL, ..};
  BoxedLongReals := ARRAY [-1 .. 1] OF REF LONGREAL {NIL, ..};
  BoxedExtendeds := ARRAY [-1 .. 1] OF REF EXTENDED {NIL, ..};

<* EXPORTED *>
PROCEDURE FromInt (i: INTEGER): REF INTEGER =
  BEGIN
    IF MinBoxedInt <= i AND i <= MaxBoxedInt THEN
      RETURN BoxedInts [i]
    ELSE
      VAR r := NEW (REF INTEGER);
      BEGIN
        r^ := i;
        RETURN r
      END
    END
  END FromInt;

<* EXPORTED *>
PROCEDURE FromChar (c: CHAR): REF CHAR =
  BEGIN
    RETURN BoxedChars [c]
  END FromChar;

<* EXPORTED *>
PROCEDURE FromBool (b: BOOLEAN): Atom.T =
  BEGIN
    IF b THEN RETURN True ELSE RETURN False END
  END FromBool;

<* EXPORTED *>
PROCEDURE FromReal (x: REAL): REF REAL =
  BEGIN
    IF x = -1.0E0 THEN
      RETURN BoxedReals [-1]
    ELSIF x = 0.0E0 THEN
      RETURN BoxedReals [0]
    ELSIF x = 1.0E0 THEN
      RETURN BoxedReals [1]
    ELSE
      VAR r := NEW (REF REAL);
      BEGIN
        r^ := x;
        RETURN r
      END
    END
  END FromReal;

<* EXPORTED *>
PROCEDURE FromLongReal(x: LONGREAL): REF LONGREAL=
  BEGIN 
    IF x = -1.0D0 THEN
      RETURN BoxedLongReals [-1]
    ELSIF x = 0.0D0 THEN
      RETURN BoxedLongReals [0]
    ELSIF x = 1.0D0 THEN
      RETURN BoxedLongReals [1]
    ELSE
      VAR r := NEW (REF LONGREAL);
      BEGIN
        r^ := x;
        RETURN r
      END
    END
  END FromLongReal;

<* EXPORTED *>
PROCEDURE FromExtended (x: EXTENDED): REF EXTENDED =
  BEGIN
    IF x = -1.0X0 THEN
      RETURN BoxedExtendeds [-1]
    ELSIF x = 0.0X0 THEN
      RETURN BoxedExtendeds [0]
    ELSIF x = 1.0X0 THEN
      RETURN BoxedExtendeds [1]
    ELSE
      VAR r := NEW (REF EXTENDED);
      BEGIN
        r^ := x;
        RETURN r
      END
    END
  END FromExtended;

REVEAL
  Syntax = MUTEX BRANDED OBJECT
             <* LL = SELF *>
             map          := SET OF CHAR {};
             mlist: MList := NIL;
           END;

(* Invariant for Syntax:
   forall (c, c IN SELF.map iff
              exists (item in SELF.mlist, item.ch = c))
*)

TYPE
  MList = REF RECORD
                ch  : CHAR;
                m   : ReadMacro;
                next: MList       := NIL
              END;

VAR Standard := NEW (Syntax);

<* EXPORTED *>
PROCEDURE Read (rd: Rd.T; syntax: Syntax := NIL): T
  RAISES {ReadError, Rd.EndOfFile, Thread.Alerted} =
  VAR
    c: CHAR;
    s            := syntax;
    a: RefList.T;
    m: ReadMacro;
  BEGIN
    IF s = NIL THEN s := Standard END;
    TRY
      LOOP
        LOOP
          Lex.Skip (rd, BLANKS);
          c := Rd.GetChar (rd);
          IF c # ';' THEN EXIT END;
          REPEAT c := Rd.GetChar (rd) UNTIL c = '\n'
        END;
        m := NIL;
        LOCK s DO IF c IN s.map THEN m := Syn (s, c).m END END;
        IF m # NIL THEN
          a := m.read (rd, syntax);
          IF a = NIL THEN        (* skip *)
          ELSIF a.tail = NIL THEN
            RETURN a.head
          ELSE
            RAISE ReadError (
                    Fmt.F ("Read-macro for '%s' produced %s results",
                           Text.FromChar (c), Fmt.Int (RefList.Length (a))))
          END
        ELSE
          RETURN ReadToken (rd, c, syntax)
        END
      END
    EXCEPT
    | Rd.Failure (ref) => RAISE ReadError (RdUtils.FailureText (ref))
    END
  END Read;

<* EXPORTED *>
PROCEDURE ReadDelimitedList (rd: Rd.T; delim: CHAR; syntax: Syntax := NIL):
  RefList.T RAISES {ReadError, Thread.Alerted} =
  VAR
    c  : CHAR;
    s              := syntax;
    a  : RefList.T;
    m  : ReadMacro;
    res: RefList.T := NIL;
  BEGIN
    IF s = NIL THEN s := Standard END;
    TRY
      LOOP
        LOOP
          Lex.Skip (rd, BLANKS);
          c := Rd.GetChar (rd);
          IF c # ';' THEN EXIT END;
          REPEAT c := Rd.GetChar (rd) UNTIL c = '\n'
        END;
        m := NIL;
        LOCK s DO IF c IN s.map THEN m := Syn (s, c).m END END;
        IF m # NIL THEN
          a := m.read (rd, syntax);
          (* res := RefList.AppendD (res, RefList.Reverse (a)); *)
          WHILE a # NIL DO
            res := RefList.Cons (a.head, res);
            a := a.tail
          END
        ELSIF c = delim THEN
          RETURN RefList.ReverseD (res)
        ELSE
          res := RefList.Cons (ReadToken (rd, c, syntax), res)
        END
      END
    EXCEPT
    | Rd.EndOfFile => RAISE ReadError ("End-of-file in ReadDelimitedList")
    | Rd.Failure (ref) => RAISE ReadError (RdUtils.FailureText (ref))
    END
  END ReadDelimitedList;

CONST
  ATOM_CHARS = SET OF
                 CHAR {
                 '!', '#', '$', '%', '&', '*', '+', '-', '.', '/', ':', '<',
                 '=', '>', '?', '@', '[', ']', '^', '_', '{', '}', '~'};
  ID_CHARS = ALPHANUMERICS + SET OF CHAR {'_'};

PROCEDURE ReadToken (rd: Rd.T; c: CHAR; s: Syntax): T
  RAISES {Rd.EndOfFile, Rd.Failure, ReadError, Thread.Alerted} =
  BEGIN
    IF c = DQUOTE THEN
      RETURN ReadDelimitedText (rd, c)
    ELSIF c = SQUOTE THEN
      RETURN FromChar (ReadCharLiteral (rd))
    ELSIF c = '(' THEN
      RETURN ReadDelimitedList (rd, ')', s)
    ELSIF c = BAR THEN
      RETURN Atom.FromText (ReadDelimitedText (rd, BAR))
    ELSIF c IN LETTERS THEN
      RETURN ReadAtom (rd, c, ID_CHARS)
    ELSIF c = '+' OR c = '-' OR c = '.' THEN (* ambiguous *)
      IF Rd.EOF (rd) THEN
        RETURN Atom.FromText (Text.FromChar (c))
      ELSE
        VAR d := Rd.GetChar (rd);
        BEGIN
          IF d IN DIGITS THEN
            RETURN ReadNumber (rd, d, c)
          ELSIF d IN ATOM_CHARS THEN
            RETURN Atom.FromText (Text.FromChar (c) & Text.FromChar (d)
                                    & Lex.Scan (rd, ATOM_CHARS))
          ELSE
            Rd.UnGetChar (rd);
            RETURN Atom.FromText (Text.FromChar (c))
          END
        END
      END
    ELSIF c IN DIGITS THEN
      RETURN ReadNumber (rd, c, c)
    ELSIF c IN ATOM_CHARS THEN
      RETURN ReadAtom (rd, c, ATOM_CHARS)
    ELSE
      RAISE ReadError (Fmt.F ("Bad character '%s'", Text.FromChar (c)))
    END
  END ReadToken;

PROCEDURE ReadAtom (rd: Rd.T; c: CHAR; READONLY cs: SET OF CHAR): Atom.T
  RAISES {Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN Atom.FromText (Text.FromChar (c) & Lex.Scan (rd, cs))
  END ReadAtom;

TYPE
  RefArrayReader = Rd.T OBJECT
                     mu: MUTEX
                   METHODS
                     init    (): RefArrayReader            := Init;
                     putChar (ch: CHAR) RAISES {ReadError} := PutChar;
                   OVERRIDES
                     length := Length;
                     seek   := Seek;
                     close  := Close
                   END;

(* This is a simple class of reader with one "buff", pre-allocated.
   Invariants: rd.lo = 0 and rd.length() = rd.hi.
   We use this because we have to scan ahead to determine what type
   of number we've got. As we scan, we store the digits in the buffer.
   When we reach the end of the number, we pass the reader to the
   appropriate procedure in Lex (Int, Real, LongReal, or Extended).
*)


CONST MAXLEN = 4000;

VAR
  Reader := NEW (RefArrayReader, mu := NEW (MUTEX),
                 buff := NEW (REF ARRAY OF CHAR, MAXLEN), lo := 0, st := 0,
                 seekable := TRUE, intermittent := FALSE);

PROCEDURE Init (rd: RefArrayReader): RefArrayReader =
  BEGIN
    rd.cur := 0;
    rd.hi := 0;
    rd.closed := FALSE;
    RETURN rd
  END Init;

PROCEDURE Seek (rd: RefArrayReader; pos: CARDINAL;
                <* UNUSED *> dontBlock: BOOLEAN):
  RdClass.SeekResult =
  BEGIN
    rd.cur := pos;
    IF pos < rd.hi THEN
      RETURN RdClass.SeekResult.Ready
    ELSE
      RETURN RdClass.SeekResult.Eof
    END
  END Seek;

PROCEDURE Length (rd: RefArrayReader): INTEGER =
  BEGIN
    RETURN rd.hi
  END Length;

PROCEDURE Close (<* UNUSED *> rd: RefArrayReader) =
  BEGIN
  END Close;

PROCEDURE PutChar (rd: RefArrayReader; ch: CHAR)
  RAISES {ReadError} =
  BEGIN
    IF rd.hi >= MAXLEN THEN
      RAISE ReadError("Sx: Text literal or numeric too long")
    END;
    rd.buff[rd.hi] := ch;
    INC(rd.hi);
  END PutChar;

PROCEDURE ReadNumber (rd: Rd.T; c: CHAR; prev: CHAR): T
  RAISES {Rd.EndOfFile, Rd.Failure, ReadError, Thread.Alerted} =
  CONST HEX_DIGITS = SET OF CHAR {'0'.. '9', 'a'.. 'f', 'A'.. 'F'};
  VAR wr := Reader;
  PROCEDURE scanExp (): Rd.T
    RAISES {Rd.EndOfFile, Rd.Failure, ReadError, Thread.Alerted} =
    BEGIN
      wr.putChar (c);
      c := Rd.GetChar (rd);
      IF c = '+' OR c = '-' THEN wr.putChar (c); c := Rd.GetChar (rd) END;
      IF NOT c IN DIGITS THEN RAISE ReadError ("Illegal exponent") END;
      REPEAT wr.putChar (c); c := Rd.GetChar (rd) UNTIL NOT c IN DIGITS;
      Rd.UnGetChar (rd);
      RETURN wr
    END scanExp;
  PROCEDURE scanFloat (): T RAISES {Lex.Error, FloatMode.Trap, Rd.EndOfFile,
                                    Rd.Failure, ReadError, Thread.Alerted} =
    BEGIN
      IF c = 'e' OR c = 'E' THEN
        RETURN FromReal (Lex.Real (scanExp ()))
      ELSIF c = 'd' OR c = 'D' THEN
        RETURN FromLongReal (Lex.LongReal (scanExp ()))
      ELSIF c = 'x' OR c = 'X' THEN
        RETURN FromExtended (Lex.Extended (scanExp ()))
      ELSE
        Rd.UnGetChar (rd);
        RETURN FromReal (Lex.Real (wr))
      END
    END scanFloat;
  BEGIN
    TRY
      LOCK wr.mu DO
        EVAL wr.init ();
        IF NOT prev IN DIGITS THEN wr.putChar (prev) END; (* + - . *)
        REPEAT wr.putChar (c); c := Rd.GetChar (rd) UNTIL NOT c IN DIGITS;
        IF prev = '.' THEN
          RETURN scanFloat ()
        ELSIF c = '_' THEN
          wr.putChar (c);
          c := Rd.GetChar (rd);
          IF NOT c IN HEX_DIGITS THEN RAISE ReadError ("Illegal integer") END;
          REPEAT
            wr.putChar (c);
            c := Rd.GetChar (rd)
          UNTIL NOT c IN HEX_DIGITS;
          Rd.UnGetChar (rd);
          RETURN FromInt (Lex.Int (wr))
        ELSIF c = '.' THEN
          REPEAT wr.putChar (c); c := Rd.GetChar (rd) UNTIL NOT c IN DIGITS;
          RETURN scanFloat ()
        ELSE
          Rd.UnGetChar (rd);
          RETURN FromInt (Lex.Int (wr))
        END
      END
    EXCEPT
    | Lex.Error, FloatMode.Trap => RAISE ReadError ("Bad Format in number")
    END
  END ReadNumber;

PROCEDURE ReadDelimitedText (rd: Rd.T; delim: CHAR): TEXT
  RAISES {Rd.EndOfFile, Rd.Failure, ReadError, Thread.Alerted} =
  VAR
    wr       := Reader;
    c : CHAR;
  BEGIN
    LOCK wr.mu DO
      EVAL wr.init ();
      LOOP
        c := Rd.GetChar (rd);
        IF c = delim THEN
          RETURN Text.FromChars (SUBARRAY (wr.buff^, 0, wr.hi))
        ELSIF c = SLASH THEN
          wr.putChar (ReadEscapeSequence (rd, delim))
        ELSIF ISO_Latin_printing (c) THEN
          wr.putChar (c)
        ELSE
          RAISE ReadError ("Illegal character in Text literal")
        END
      END
    END
  END ReadDelimitedText;

PROCEDURE ISO_Latin_printing (ch: CHAR): BOOLEAN =
  BEGIN
    RETURN ' ' <= ch AND ch <= '~' OR '\241' <= ch AND ch <= '\377'
  END ISO_Latin_printing;

PROCEDURE ReadCharLiteral (rd: Rd.T): CHAR
  RAISES {Rd.EndOfFile, Rd.Failure, ReadError, Thread.Alerted} =
  VAR c := Rd.GetChar (rd);
  BEGIN
    IF c = SQUOTE OR NOT ISO_Latin_printing (c) THEN
      RAISE ReadError ("Illegal character literal")
    END;
    IF c = SLASH THEN c := ReadEscapeSequence (rd, SQUOTE) END;
    IF Rd.GetChar (rd) = SQUOTE THEN
      RETURN c
    ELSE
      RAISE ReadError ("Illegal character literal")
    END
  END ReadCharLiteral;

PROCEDURE ReadEscapeSequence (rd: Rd.T; delim: CHAR): CHAR
  RAISES {Rd.EndOfFile, Rd.Failure, ReadError, Thread.Alerted} =
  (* Note that for reading, \' is allowed but unnecessary in text literals,
     and \" is allowed but unnecessary in character literals. *)
  CONST OCTAL_CODES = ARRAY ['0' .. '7'] OF INTEGER {0, 1, 2, 3, 4, 5, 6, 7};
  VAR
    c             := Rd.GetChar (rd);
    sum: [0 .. 8_377];
  BEGIN
    IF c = 'n' THEN
      RETURN '\n'
    ELSIF c = 'r' THEN
      RETURN '\r'
    ELSIF c = 't' THEN
      RETURN '\t'
    ELSIF c = 'f' THEN
      RETURN '\f'
    ELSIF c = SLASH OR c = delim OR c = DQUOTE OR c = SQUOTE THEN
      RETURN c
    ELSIF '0' <= c AND c <= '3' THEN
      sum := OCTAL_CODES [c];
      c := Rd.GetChar (rd);
      IF '0' <= c AND c <= '7' THEN
        sum := 8 * sum + OCTAL_CODES [c];
        c := Rd.GetChar (rd);
        IF '0' <= c AND c <= '7' THEN
          sum := 8 * sum + OCTAL_CODES [c];
          RETURN VAL (sum, CHAR)
        END
      END
    END;
    RAISE ReadError ("Illegal escape sequence")
  END ReadEscapeSequence;

<* EXPORTED *>
PROCEDURE Print (wr       : Wr.T;
                 sx       : T;
                 maxDepth : CARDINAL := LAST (CARDINAL);
                 maxLength: CARDINAL := LAST (CARDINAL)  )
  RAISES {PrintError, Wr.Failure, Thread.Alerted} =
  CONST
    DEPTH_ELLIPSIS  = "...";
    LENGTH_ELLIPSIS = "...";
  BEGIN
    TYPECASE sx OF
    | NULL => Wr.PutText (wr, "()")
    | REF INTEGER (r) => Wr.PutText (wr, Fmt.Int (r^))
    | REF CHAR (r) =>
        Wr.PutChar (wr, SQUOTE);
        PrintChar (wr, r^, SQUOTE);
        Wr.PutChar (wr, SQUOTE)
    | REF REAL (r) =>
        (* Wr.PutText (wr, Fmt.Real (r^, modula := TRUE)) *)
        Wr.PutText (wr, Fmt.Real (r^, Fmt.Style.Auto, literal := TRUE))
    | REF LONGREAL (r) =>
        (* Wr.PutText (wr, Fmt.LongReal (r^, modula := TRUE)) *)
        Wr.PutText (wr, Fmt.LongReal (r^, Fmt.Style.Auto, literal := TRUE))
    | REF EXTENDED (r) =>
        (* Wr.PutText (wr, Fmt.Extended (r^, modula := TRUE)) *)
        Wr.PutText (wr, Fmt.Extended (r^, Fmt.Style.Auto, literal := TRUE))
    | TEXT (t) =>
        Wr.PutChar (wr, DQUOTE);
        FOR i := 0 TO Text.Length (t) - 1 DO
          PrintChar (wr, Text.GetChar (t, i), DQUOTE)
        END;
        Wr.PutChar (wr, DQUOTE)
    | Atom.T (a) =>
        VAR name := Atom.ToText (a);
        BEGIN
          IF NeedsBars (name) THEN
            Wr.PutChar (wr, BAR);
            FOR i := 0 TO Text.Length (name) - 1 DO
              PrintChar (wr, Text.GetChar (name, i), BAR)
            END;
            Wr.PutChar (wr, BAR)
          ELSE
            Wr.PutText (wr, name)
          END
        END
    | RefList.T (list) =>
        IF maxDepth = 0 THEN
          Wr.PutText (wr, DEPTH_ELLIPSIS)
        ELSE
          VAR len := maxLength;
          BEGIN
            Wr.PutChar (wr, '(');
            DEC (maxDepth);
            LOOP
              Print (wr, list.head, maxDepth, maxLength);
              list := list.tail;
              IF list = NIL THEN EXIT END;
              Wr.PutChar (wr, ' ');
              IF len = 0 THEN Wr.PutText (wr, LENGTH_ELLIPSIS); EXIT END;
              DEC (len)
            END;
            Wr.PutChar (wr, ')')
          END
        END
    ELSE
      RAISE PrintError ("Unprintable S-expression")
    END
  END Print;

PROCEDURE PrintChar (wr: Wr.T; ch: CHAR; delim: CHAR)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF ch = '\n' THEN
      Wr.PutText (wr, "\\n")
    ELSIF ch = '\t' THEN
      Wr.PutText (wr, "\\t")
    ELSIF ch = '\r' THEN
      Wr.PutText (wr, "\\r")
    ELSIF ch = '\f' THEN
      Wr.PutText (wr, "\\f")
    ELSIF ch = SLASH THEN
      Wr.PutText (wr, "\\\\")
    ELSIF ch = delim THEN
      Wr.PutChar (wr, SLASH);
      Wr.PutChar (wr, ch)
    ELSIF ISO_Latin_printing (ch) THEN
      Wr.PutText (wr, Text.FromChar (ch))
    ELSE
      Wr.PutText (wr, Fmt.F ("\\%03s", Fmt.Int (ORD (ch), 8)))
    END
  END PrintChar;

PROCEDURE NeedsBars (t: TEXT): BOOLEAN =
  VAR
    len       := Text.Length (t);
    c  : CHAR;
  BEGIN
    IF len = 0 THEN RETURN TRUE END; (* || *)
    c := Text.GetChar (t, 0);
    IF c IN LETTERS THEN
      FOR i := 1 TO len - 1 DO
        c := Text.GetChar (t, i);
        IF NOT c IN ID_CHARS THEN RETURN TRUE END
      END;
      RETURN FALSE
    ELSIF c IN ATOM_CHARS THEN
      FOR i := 1 TO len - 1 DO
        c := Text.GetChar (t, i);
        IF NOT c IN ATOM_CHARS THEN RETURN TRUE END
      END;
      RETURN FALSE
    ELSE
      RETURN TRUE
    END
  END NeedsBars;


<* EXPORTED *>
PROCEDURE CopySyntax (s: Syntax := NIL): Syntax =
  VAR
    new          := NEW (Syntax);
    oldml: MList;
  BEGIN
    IF s # NIL THEN
      new.map := s.map;
      LOCK s DO
        oldml := s.mlist;
        WHILE oldml # NIL DO
          new.mlist :=
            NEW (MList, ch := oldml.ch, m := oldml.m, next := new.mlist);
          oldml := oldml.next
        END
      END
    END;
    RETURN new
  END CopySyntax;

PROCEDURE Syn (s: Syntax; ch: CHAR): MList = <* LL = s *>
  VAR ml := s.mlist;
  BEGIN
    LOOP IF ml.ch = ch THEN RETURN ml ELSE ml := ml.next END END
  END Syn;

<* EXPORTED *>
PROCEDURE SetReadMacro (s: Syntax; ch: CHAR; m: ReadMacro) =
  PROCEDURE remove (VAR ml: MList) = (* My favorite use of
                                        VAR *)
    BEGIN
      IF ml.ch = ch THEN
        ml := ml.next
      ELSE
        remove (ml.next)
      END
    END remove;
  BEGIN
    IF s = NIL THEN RAISE SetReadMacroError END;
    LOCK s DO
      IF ch IN NOREADMACROS THEN RAISE SetReadMacroError END;
      IF ch IN s.map THEN
        IF m = NIL THEN
          s.map := s.map - SET OF CHAR {ch};
          remove (s.mlist)
        ELSE
          Syn (s, ch).m := m
        END
      ELSIF m # NIL THEN
        s.map := s.map + SET OF CHAR {ch};
        s.mlist :=
          NEW (MList, ch := ch, m := m, next := s.mlist)
      END
    END
  END SetReadMacro;

BEGIN
  True  := Atom.FromText ("TRUE");
  False := Atom.FromText ("FALSE");
  FOR i := MinBoxedInt TO MaxBoxedInt DO
    BoxedInts [i] := NEW (REF INTEGER);
    BoxedInts [i]^ := i
  END;
  FOR c := FIRST (CHAR) TO LAST (CHAR) DO
    BoxedChars [c] := NEW (REF CHAR);
    BoxedChars [c]^ := c
  END;
  FOR i := -1 TO +1 DO
    BoxedReals [i] := NEW (REF REAL);
    BoxedReals [i]^ := FLOAT (i, REAL);
    BoxedLongReals [i] := NEW (REF LONGREAL);
    BoxedLongReals [i]^ := FLOAT (i, LONGREAL);
    BoxedExtendeds [i] := NEW (REF EXTENDED);
    BoxedExtendeds [i]^ := FLOAT (i, EXTENDED)
  END
END Sx.
