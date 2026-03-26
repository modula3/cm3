(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE SchemeInputPort;
IMPORT AL, Rd, UnsafeRd, RdClass, Math;
FROM SchemeUtils IMPORT Error, Warn, Cons, List2, ListToVector, Stringify;
FROM Scheme IMPORT Object, E;
IMPORT SchemeLongReal;
IMPORT SchemeBoolean, SchemeSymbol;
IMPORT CharSeq;
IMPORT Text;
IMPORT Scan, FloatMode, Lex, CitTextUtils AS TextUtils;
FROM SchemeChar IMPORT IChr, Character, Delims, White, NumberChars;
IMPORT Thread;
IMPORT SchemeString;
IMPORT SchemePair, SchemeUtils;
IMPORT SchemeInputPortClass;
IMPORT Fmt;
IMPORT BigInt;
IMPORT SchemeInt, Mpz;
(*IMPORT Debug;*)

REVEAL RdClass.Private <: MUTEX; (* see cryptic SRC comments *)

TYPE Boolean = SchemeBoolean.T;
     String  = SchemeString.T;

<* FATAL Thread.Alerted *>

REVEAL
  T = SchemeInputPortClass.Private BRANDED Brand OBJECT
    rd : Rd.T;
    isPushedToken, isPushedChar := FALSE;
    pushedToken : Object := NIL;
    pushedChar : INTEGER := -1;
    lNo := 1;
    helpfulText : TEXT := NIL;
  METHODS

    nextToken(bigInt     : BOOLEAN;
              bigIntBase : BigInt.PrintBase;
              wx         : Wx                := NIL) : Object
      RAISES { E } := NextToken;

    readTail(bigInt     : BOOLEAN;
             bigIntBase : BigInt.PrintBase;
             wx         : Wx) : Object RAISES { E } := ReadTail2;

    warn(msg : TEXT) RAISES { E } := MyWarn;

    doRead(bigInt : BOOLEAN; bigIntBase : BigInt.PrintBase) : Object RAISES { E } := DoRead;
  OVERRIDES
    fastGetCh := FastGetCh;
    lock      := Lock;
    unlock    := Unlock;

    getCh    :=  GetCh; 
    init     :=  Init;
    readChar :=  ReadChar;
    peekChar :=  PeekChar;
    pushChar :=  PushChar;
    popChar  :=  PopChar;
    peekCh   :=  PeekCh;
    read     :=  Read;
    readBigInt := ReadBigInt;
    close      :=  Close;
    charReady  :=  CharReady;
  END;

PROCEDURE UnNil( txt : TEXT) : TEXT = 
  BEGIN IF txt = NIL THEN RETURN "<NIL>" ELSE RETURN txt END END UnNil;

PROCEDURE MyWarn(t : T; msg : TEXT) RAISES { E } =
  BEGIN
    EVAL Warn(msg & ", " & UnNil(t.helpfulText) & ", line " & Fmt.Int(t.lNo))
  END MyWarn;

PROCEDURE Lock(t : T) = BEGIN RdClass.Lock(t.rd) END Lock;

PROCEDURE Unlock(t : T) = BEGIN RdClass.Unlock(t.rd) END Unlock;

PROCEDURE Init(t : T; rd : Rd.T; warnText : TEXT) : T = 
  BEGIN t.helpfulText := warnText; t.rd := rd; RETURN t END Init;

PROCEDURE GetCh(t : T) : INTEGER RAISES { E } =
  BEGIN
    LOCK t.rd DO
      RETURN t.fastGetCh()
    END
  END GetCh;

PROCEDURE FastGetCh(t : T) : INTEGER RAISES { E } =
  BEGIN
    TRY
      WITH chr = UnsafeRd.FastGetChar(t.rd) DO
        IF chr = '\n' THEN INC(t.lNo) END;

        RETURN ORD(chr)
      END
    EXCEPT
      Rd.EndOfFile => RETURN ChEOF 
    |
      Rd.Failure(err) =>
      t.warn("Rd.Failure : " & AL.Format(err));
      RETURN ChEOF
    END
  END FastGetCh;

PROCEDURE ReadChar(t : T) : Object RAISES { E } =
  BEGIN
(*
    TRY
*)
      IF t.isPushedChar THEN
        t.isPushedChar := FALSE;
        IF t.pushedChar = ChEOF THEN 
          RETURN EOF
        ELSE
          RETURN IChr(t.pushedChar)
        END
      ELSE
        WITH ch = t.getCh() DO
          IF ch = ChEOF THEN RETURN EOF ELSE RETURN IChr(ch) END
        END
      END
(*
    EXCEPT
      Rd.Failure(err) =>
        t.warn("On input, exception: " & AL.Format(err));
        RETURN EOF
    END
*)
  END ReadChar;

PROCEDURE PeekChar(t : T) : Object RAISES { E } =
  BEGIN
    WITH p = t.peekCh() DO
      IF p = ChEOF THEN RETURN EOF ELSE RETURN IChr(p) END
    END
  END PeekChar;

PROCEDURE CharReady(t : T) : BOOLEAN RAISES { E } =
  BEGIN
    IF t.isPushedChar THEN RETURN TRUE END;
    TRY
      RETURN Rd.CharsReady(t.rd) > 0
    EXCEPT
      Rd.Failure(err) => RAISE E("char-ready?: Rd.Failure: " & AL.Format(err))
    END
  END CharReady;

PROCEDURE PushChar(t : T; ch : INTEGER) : INTEGER =
  BEGIN
    t.isPushedChar := TRUE;
    t.pushedChar := ch;
    RETURN ch 
  END PushChar;

PROCEDURE PopChar(t : T) : INTEGER =
  BEGIN
    t.isPushedChar := FALSE;
    RETURN t.pushedChar
  END PopChar;

PROCEDURE PeekCh(t : T) : INTEGER RAISES { E } =
  BEGIN
(*
    TRY
*)
      IF t.isPushedChar THEN
        RETURN t.pushedChar
      ELSE
        RETURN t.pushChar(t.getCh())
      END
(*
    EXCEPT
      Rd.Failure(err) => 
        t.warn("On input, exception: " & AL.Format(err));
        RETURN ChEOF
    END
*)
  END PeekCh;

PROCEDURE DoRead(t : T; bigInt : BOOLEAN; bigIntBase : BigInt.PrintBase := 10) : Object RAISES { E } =

  CONST Symbol = SchemeSymbol.Symbol;

  VAR wx := WxReset(NIL);
  BEGIN
(*
    TRY
*)
      WITH token = t.nextToken(bigInt, bigIntBase, wx) DO
        IF    token = LP THEN
          RETURN t.readTail(bigInt, bigIntBase, wx)
        ELSIF token = RP THEN
          t.warn("Extra ) ignored"); RETURN t.doRead(bigInt, bigIntBase)
        ELSIF token = DOT THEN
          t.warn("Extra . ignored"); RETURN t.doRead(bigInt, bigIntBase)
        ELSIF token = SQ THEN
          RETURN List2(Symbol("quote"), t.doRead(bigInt, bigIntBase))
        ELSIF token = BQ THEN
          RETURN List2(Symbol("quasiquote"), t.doRead(bigInt, bigIntBase))
        ELSIF token = COM THEN
          RETURN List2(Symbol("unquote"), t.doRead(bigInt, bigIntBase))
        ELSIF token = COMAT THEN
          RETURN List2(Symbol("unquote-splicing"), t.doRead(bigInt, bigIntBase))
        ELSE
          RETURN token
        END
      END
(*
    EXCEPT
      Rd.Failure(err) => 
        t.warn("On input, exception: " & AL.Format(err));
        RETURN EOF
    END
*)
  END DoRead;

PROCEDURE Close(t : T) : Boolean RAISES { E } =
  BEGIN
    TRY
      Rd.Close(t.rd);
      RETURN SchemeBoolean.True();
    EXCEPT
      Rd.Failure(err) => RETURN Error("IOException: " & AL.Format(err))
    END
  END Close;

PROCEDURE Read(t : T) : Object RAISES { E } =
  BEGIN RETURN DoRead(t, FALSE) END Read;

PROCEDURE ReadBigInt(t : T) : Object RAISES { E } =
  BEGIN RETURN DoRead(t, TRUE, 10) END ReadBigInt;

PROCEDURE IsEOF(x : Object) : BOOLEAN = BEGIN RETURN x = EOF END IsEOF;

PROCEDURE ReadTail2(t          : T; 
                    bigInt     : BOOLEAN;
                    bigIntBase : BigInt.PrintBase;
                    wx         : Wx) : Object
  RAISES { E } =
  VAR token := t.nextToken(bigInt, bigIntBase, wx);
      res : SchemePair.T := NIL;
      ret : Object;
  BEGIN
    LOOP
    IF    token = EOF THEN
      RETURN Error("EOF during read.")
    ELSIF token = RP THEN
      ret := NIL ; EXIT
    ELSIF token = DOT THEN
      WITH result = t.doRead(bigInt, bigIntBase) DO
        token := t.nextToken(bigInt, bigIntBase);
        IF token # RP THEN
          t.warn("Where's the ')'?  Got " & Stringify(token) &
            " after .")
        END;
        ret :=  result ; EXIT
      END
    ELSE
      t.isPushedToken := TRUE;
      t.pushedToken := token;
      res:= Cons(t.doRead(bigInt, bigIntBase), res) ; token := t.nextToken(bigInt, bigIntBase, wx)
    END
    END;

    VAR p := res; BEGIN
     res := ReverseD(res);
     IF p # NIL THEN
       p.rest := ret;
       RETURN res
     ELSE
       RETURN ret
     END;
    END
  END ReadTail2;

PROCEDURE ReverseD(p : SchemePair.T) : SchemePair.T =
  VAR q : SchemePair.T := NIL; t : SchemePair.T;
  BEGIN
    WHILE p # NIL DO
      t := p.rest;
      p.rest := q;
      q := p;
      p := t
    END;
    RETURN q
  END ReverseD;

<*UNUSED*>
PROCEDURE ReadTail(t          : T;
                   bigInt     : BOOLEAN;
                   bigIntBase : BigInt.PrintBase;
                   wx         : Wx) : Object RAISES { E } =
  VAR
    token := t.nextToken(bigInt, bigIntBase, wx);
  BEGIN
    IF    token = EOF THEN
      RETURN Error("EOF during read.")
    ELSIF token = RP THEN
      RETURN NIL
    ELSIF token = DOT THEN
      WITH result = t.doRead(bigInt, bigIntBase) DO
        token := t.nextToken(bigInt, bigIntBase);
        IF token # RP THEN
          t.warn("Where's the ')'?  Got " & Stringify(token) &
            " after . ")
        END;
        RETURN result
      END
    ELSE
      t.isPushedToken := TRUE;
      t.pushedToken := token;
      RETURN Cons(t.doRead(bigInt, bigIntBase), t.readTail(bigInt, bigIntBase, wx))
    END
  END ReadTail;

PROCEDURE NextToken(t          : T;
                    bigInt     : BOOLEAN;
                    bigIntBase : BigInt.PrintBase;
                    wx         : Wx) : Object RAISES { E } =

  CONST Symbol = SchemeSymbol.Symbol;

  VAR ch : INTEGER;

  BEGIN
    TRY
      t.lock();
      IF t.isPushedToken THEN
        t.isPushedToken := FALSE;
        RETURN t.pushedToken
      ELSIF t.isPushedChar THEN
        ch := t.popChar()
      ELSE
        ch := t.fastGetCh()
      END;

      WHILE ch # ChEOF AND VAL(ch,CHAR) IN White DO ch := t.fastGetCh() END;
    FINALLY
      t.unlock()
    END;

    CASE ch OF
      ChEOF => RETURN EOF;
    |
      ORD('(') => RETURN LP
    |
      ORD(')') => RETURN RP 
    | 
      ORD('\''), ORD('`') => 
        RETURN Symbol(Text.FromChar(VAL(ch,CHAR)))
    |
      ORD(',') =>
        ch := t.getCh();
        IF ch = ORD('@') THEN
          RETURN COMAT
        ELSE
          EVAL t.pushChar(ch); RETURN COM
        END
    |
      ORD(';') =>
        TRY
          t.lock();
          WHILE ch # ChEOF AND ch # ORD('\n') AND ch # ORD('\r') DO
            ch := t.fastGetCh()
          END
        FINALLY
          t.unlock()
        END;
        RETURN t.nextToken(bigInt, bigIntBase)
    |
      ORD(QMC) =>
        WITH buff = NEW(CharSeq.T).init() DO
          TRY
            t.lock();
            LOOP
              ch := t.fastGetCh();
              IF ch = ORD(QMC) OR ch = ChEOF THEN EXIT END;
              IF ch = ORD(BSC) THEN
                buff.addhi(VAL(t.fastGetCh(),CHAR))
              ELSE
                buff.addhi(VAL(ch,CHAR))
              END
            END;
            IF ch = ChEOF THEN t.warn("EOF inside a string") END;
            RETURN CharSeqToArray(buff)
          FINALLY
            t.unlock()
          END
        END
    |
      ORD('#') =>
        ch := t.getCh();
        CASE ch OF
          ORD('t'), ORD('T') => RETURN SchemeBoolean.True()
        |
          ORD('f'), ORD('F') => RETURN SchemeBoolean.False()
        |
          ORD('(') =>
            EVAL t.pushChar(ch);
            RETURN ListToVector(t.doRead(bigInt, bigIntBase))
        |
          ORD(BSC) =>
            ch := t.getCh();
            IF VAL(ch,CHAR) IN SET OF CHAR { 's', 'S', 'n', 'N' } THEN
              EVAL t.pushChar(ch);
              WITH token = t.nextToken(bigInt, bigIntBase) DO
                IF    token = SPACE THEN RETURN Character(' ')
                ELSIF token = NEWLINE THEN RETURN Character('\n')
                ELSE
                  (* this isn't right.. if we're parsing "#\n", for
                     instance, we'd be pushing the token "n"... *)
                  (*
                  t.isPushedToken := TRUE;
                  t.pushedToken := token;
                  *)

                  (* lop off start of token *)
                  WITH txt = SchemeSymbol.ToText(token) DO
                    IF Text.Length(txt) > 1 THEN
                      t.isPushedToken := TRUE;
                      t.pushedToken := SchemeSymbol.Symbol(Text.Sub(txt,1))
                    END
                  END;

                  RETURN IChr(ch)
                END
              END
            ELSE
              RETURN IChr(ch)
            END
        |
          ORD('e'), ORD('i'), ORD('d'),
          ORD('b'), ORD('o'), ORD('x') =>
            (* R4RS number prefixes: #e #i #d #b #o #x
               Combined prefixes allowed in any order, e.g. #e#x1F *)
            VAR base := bigIntBase;
                exactness := ORD('d'); (* default *)
            BEGIN
              LOOP
                CASE ch OF
                  ORD('e'), ORD('i') => exactness := ch
                |
                  ORD('d') => (* default radix, keep current base *)
                |
                  ORD('b') => base := 2
                |
                  ORD('o') => base := 8
                |
                  ORD('x') => base := 16
                ELSE
                  EXIT
                END;
                ch := t.getCh();
                IF ch # ORD('#') THEN EXIT END;
                ch := t.getCh()
              END;
              (* ch is the first char after the prefix(es); push it back *)
              EVAL t.pushChar(ch);
              WITH result = t.nextToken(bigInt, base) DO
                IF exactness = ORD('e') THEN
                  (* force exact *)
                  IF SchemeInt.IsExactInt(result) THEN
                    RETURN result
                  ELSIF ISTYPE(result, SchemeLongReal.T) THEN
                    WITH lr = NARROW(result, SchemeLongReal.T)^ DO
                      IF Math.floor(lr) = lr AND
                         lr >= FLOAT(FIRST(INTEGER), LONGREAL) AND
                         lr <= FLOAT(LAST(INTEGER), LONGREAL) THEN
                        RETURN SchemeInt.FromI(ROUND(lr))
                      ELSE
                        WITH m = Mpz.New() DO
                          Mpz.set_d(m, lr);
                          RETURN SchemeInt.MpzToScheme(m)
                        END
                      END
                    END
                  ELSE
                    RETURN result
                  END
                ELSIF exactness = ORD('i') THEN
                  (* force inexact *)
                  IF SchemeInt.IsExactInt(result) THEN
                    RETURN SchemeLongReal.FromLR(SchemeLongReal.FromO(result))
                  ELSE
                    RETURN result
                  END
                ELSE
                  RETURN result
                END
              END
            END
        ELSE
          t.warn("#" & Text.FromChar(VAL(ch,CHAR)) &
                    " not recognized, ignored");
          RETURN t.nextToken(bigInt, bigIntBase)
        END
    ELSE
      WITH c  = VAL(ch,CHAR) DO

        wx := WxReset(wx);

        
        TRY
          t.lock();
          REPEAT
            WxPutChar(wx, VAL(ch, CHAR));
            
            ch := t.fastGetCh()
          UNTIL
            ch = ChEOF OR VAL(ch,CHAR) IN White OR VAL(ch,CHAR) IN Delims;
        FINALLY
          t.unlock()
        END;

        EVAL t.pushChar(ch);

        IF c IN NumberChars OR bigIntBase # 10 THEN
          WITH txt = WxToText(wx) DO
            IF bigIntBase # 10 THEN
              (* non-decimal radix from #b/#o/#x prefix: parse as exact integer *)
              EVAL WxReset(wx);
              TRY
                WITH i = Scan.Int(txt, bigIntBase) DO
                  RETURN SchemeInt.FromI(i)
                END
              EXCEPT
                Lex.Error, FloatMode.Trap =>
                  (* overflow or parse error, try Mpz *)
                  VAR m: Object; BEGIN
                    TRY m := Mpz.InitScan(txt, bigIntBase)
                    EXCEPT ELSE m := NIL END;
                    IF m # NIL THEN RETURN m END;
                    WxPutText(wx, txt) (* restore *)
                  END
              END
            ELSIF bigInt THEN
              (* read-big-int path: replaces BigInt.ScanBased.
                 Handles base_value notation (e.g. "10_65536", "16_FF")
                 and plain decimal integers.  Returns native exact
                 integers (SchemeInt or Mpz.T for overflow).
                 Raises Lex.Error for non-numeric tokens like "-", "+". *)
              EVAL WxReset(wx);
              TRY
                RETURN ScanBased(txt, 10)
              EXCEPT
                Lex.Error, FloatMode.Trap =>
                  WxPutText(wx, txt) (* restore *)
              END
            ELSIF NOT HaveAlphasOtherThane(txt) THEN
              (* decimal number: check if pure integer or float *)
              EVAL WxReset(wx);
              IF IsPureInteger(txt) THEN
                (* no decimal point, no exponent: parse as exact integer *)
                TRY
                  WITH i = Scan.Int(txt) DO
                    RETURN SchemeInt.FromI(i)
                  END
                EXCEPT
                  Lex.Error, FloatMode.Trap =>
                    (* overflow, try Mpz *)
                    VAR m: Object; BEGIN
                      TRY m := Mpz.InitScan(txt, 10)
                      EXCEPT ELSE m := NIL END;
                      IF m # NIL THEN RETURN m END;
                      WxPutText(wx, txt) (* restore *)
                    END
                END
              ELSE
                (* has decimal point or exponent: inexact *)
                TRY
                  WITH lr = Scan.LongReal(txt),
                       lrp = NEW(SchemeLongReal.T) DO
                    lrp^ := lr;
                    RETURN lrp
                  END
                EXCEPT
                  Lex.Error, FloatMode.Trap =>
                    WxPutText(wx, txt) (* restore *);
                END
              END
            END
          END
        END;

        IF CaseInsensitive THEN
          RETURN Symbol(TextUtils.ToLower(WxToText(wx)))
        ELSE
          RETURN Symbol(WxToText(wx))
        END
      END
    END
  END NextToken;

PROCEDURE HaveAlphasOtherThane(txt : TEXT) : BOOLEAN =
  BEGIN
    FOR i := 0 TO Text.Length(txt)-1 DO
      WITH c = Text.GetChar(txt, i) DO
        IF c >= 'a' AND c <= 'z' OR c >= 'A' AND c <= 'Z' THEN
          IF c # 'e' THEN RETURN TRUE END
        END
      END
    END;
    RETURN FALSE
  END HaveAlphasOtherThane;

PROCEDURE IsPureInteger(txt : TEXT) : BOOLEAN =
  (* TRUE if txt looks like an integer: optional sign, then all digits.
     No decimal point, no exponent. *)
  VAR
    start := 0;
    len := Text.Length(txt);
  BEGIN
    IF len = 0 THEN RETURN FALSE END;
    IF Text.GetChar(txt, 0) = '+' OR Text.GetChar(txt, 0) = '-' THEN
      start := 1
    END;
    IF start >= len THEN RETURN FALSE END;
    FOR i := start TO len-1 DO
      WITH c = Text.GetChar(txt, i) DO
        IF c < '0' OR c > '9' THEN RETURN FALSE END
      END
    END;
    RETURN TRUE
  END IsPureInteger;

PROCEDURE ScanBased(txt : TEXT; defaultBase : CARDINAL) : Object
  RAISES { Lex.Error, FloatMode.Trap } =
  (* Replaces BigInt.ScanBased: parses base_value notation
     (e.g. "10_65536" = 65536, "16_FF" = 255) and plain decimals.
     Returns SchemeInt or Mpz.T.  Raises Lex.Error for non-numbers. *)
  VAR
    neg := FALSE;
    start : TEXT := txt;
  BEGIN
    IF Text.Length(txt) = 0 THEN RAISE Lex.Error END;
    IF Text.GetChar(txt, 0) = '-' THEN
      neg := TRUE;
      start := Text.Sub(txt, 1)
    ELSIF Text.GetChar(txt, 0) = '+' THEN
      start := Text.Sub(txt, 1)
    END;
    IF Text.Length(start) = 0 THEN RAISE Lex.Error END;

    VAR upos := Text.FindChar(start, '_');
        base := defaultBase;
        valueTxt := start;
    BEGIN
      IF upos >= 1 AND upos < Text.Length(start) - 1 THEN
        base := Scan.Int(Text.Sub(start, 0, upos));
        valueTxt := Text.Sub(start, upos + 1)
      END;

      IF base < 2 OR base > 36 THEN RAISE Lex.Error END;

      IF base <= 16 THEN
        (* Scan.Int handles bases 2..16 *)
        TRY
          VAR i := Scan.Int(valueTxt, base); BEGIN
            IF neg THEN i := -i END;
            RETURN SchemeInt.FromI(i)
          END
        EXCEPT
          Lex.Error, FloatMode.Trap =>
            (* overflow or parse error: fall through to Mpz *)
        END
      END;

      (* bases 17..36 or INTEGER overflow: use Mpz.
         Mpz.init_set_str returns 0 on success, -1 on failure. *)
      VAR m := Mpz.New(); BEGIN
        IF Mpz.set_str(m, valueTxt, base) # 0 THEN
          RAISE Lex.Error
        END;
        IF neg THEN Mpz.neg(m, m) END;
        RETURN SchemeInt.MpzToScheme(m)
      END
    END
  END ScanBased;

PROCEDURE CharSeqToArray(seq : CharSeq.T) : String =
  BEGIN
    WITH res = NEW(String, seq.size()) DO
      FOR i := FIRST(res^) TO LAST(res^) DO
        res[i] := seq.get(i)
      END;
      RETURN res
    END
  END CharSeqToArray;

(**********************************************************************)

TYPE
  Wx = REF RECORD
    d : REF ARRAY OF CHAR;
    s : CARDINAL;
  END;

PROCEDURE WxReset(old : Wx) : Wx =
  BEGIN
    IF old = NIL THEN
      RETURN NEW(Wx, d := NEW(REF ARRAY OF CHAR, 1), s := 0)
    ELSE
      old.s := 0;
      RETURN old
    END
  END WxReset;

PROCEDURE WxPutChar(wx : Wx; c : CHAR) =
  BEGIN
    IF wx.s = LAST(wx.d^) + 1 THEN
      VAR 
        new := NEW(REF ARRAY OF CHAR, NUMBER(wx.d^) * 2);
      BEGIN
        SUBARRAY(new^,0,NUMBER(wx.d^)) := wx.d^;
        wx.d := new
      END
    END;
    wx.d^[wx.s] := c; 
    INC(wx.s)
  END WxPutChar;

PROCEDURE WxPutText(wx : Wx; t : TEXT) =
  BEGIN
    FOR i := 0 TO Text.Length(t)-1 DO
      WxPutChar(wx,Text.GetChar(t,i))
    END
  END WxPutText;

PROCEDURE WxToText(wx : Wx) : TEXT =
  BEGIN
    RETURN Text.FromChars(SUBARRAY(wx.d^,0,wx.s))
  END WxToText;

(**********************************************************************)

CONST BSC = '\\'; QMC = '"';

VAR 
  LP := SchemeSymbol.Symbol("(");
  RP := SchemeSymbol.Symbol(")");
  DOT := SchemeSymbol.Symbol(".");
  SQ := SchemeSymbol.Symbol("'");
  BQ := SchemeSymbol.Symbol("`");
  COM := SchemeSymbol.Symbol(",");
  COMAT := SchemeSymbol.Symbol(",@");
  SPACE := SchemeSymbol.Symbol("space");
  NEWLINE := SchemeSymbol.Symbol("newline");
BEGIN
  EOF := SchemeSymbol.Symbol("#!EOF");
END SchemeInputPort.
