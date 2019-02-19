(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE SchemeInputPort;
IMPORT AL, Rd, UnsafeRd, RdClass;
FROM SchemeUtils IMPORT Error, Warn, Cons, List2, ListToVector, Stringify;
FROM Scheme IMPORT Object, E;
IMPORT SchemeLongReal;
IMPORT SchemeBoolean, SchemeSymbol;
IMPORT CharSeq;
IMPORT Text;
IMPORT Scan, FloatMode, Lex, TextUtils;
FROM SchemeChar IMPORT IChr, Character, Delims, White, NumberChars;
IMPORT Thread;
IMPORT SchemeString;
IMPORT SchemePair, SchemeUtils;
IMPORT SchemeInputPortClass;
IMPORT Fmt;
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

    nextToken(wx : Wx := NIL) : Object RAISES { E } := NextToken;

    readTail(wx : Wx) : Object RAISES { E } := ReadTail2;

    warn(msg : TEXT) RAISES { E } := MyWarn;

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
    close    :=  Close;
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

PROCEDURE Read(t : T) : Object RAISES { E } =

  CONST Symbol = SchemeSymbol.Symbol;

  VAR wx := WxReset(NIL);
  BEGIN
(*
    TRY
*)
      WITH token = t.nextToken(wx) DO
        IF    token = LP THEN
          RETURN t.readTail(wx)
        ELSIF token = RP THEN
          t.warn("Extra ) ignored"); RETURN t.read()
        ELSIF token = DOT THEN
          t.warn("Extra . ignored"); RETURN t.read()
        ELSIF token = SQ THEN
          RETURN List2(Symbol("quote"), t.read())
        ELSIF token = BQ THEN
          RETURN List2(Symbol("quasiquote"), t.read())
        ELSIF token = COM THEN
          RETURN List2(Symbol("unquote"), t.read())
        ELSIF token = COMAT THEN
          RETURN List2(Symbol("unquote-splicing"), t.read())
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
  END Read;

PROCEDURE Close(t : T) : Boolean RAISES { E } =
  BEGIN
    TRY
      Rd.Close(t.rd);
      RETURN SchemeBoolean.True();
    EXCEPT
      Rd.Failure(err) => RETURN Error("IOException: " & AL.Format(err))
    END
  END Close;

PROCEDURE IsEOF(x : Object) : BOOLEAN = BEGIN RETURN x = EOF END IsEOF;

PROCEDURE ReadTail2(t : T; 
                   wx : Wx) : Object RAISES { E } =
  VAR token := t.nextToken(wx);
      res : SchemePair.T := NIL;
      ret : Object;
  BEGIN
    LOOP
    IF    token = EOF THEN
      RETURN Error("EOF during read.")
    ELSIF token = RP THEN
      ret := NIL ; EXIT
    ELSIF token = DOT THEN
      WITH result = t.read() DO
        token := t.nextToken();
        IF token # RP THEN
          t.warn("Where's the ')'?  Got " & Stringify(token) &
            " after .")
        END;
        ret :=  result ; EXIT
      END
    ELSE
      t.isPushedToken := TRUE;
      t.pushedToken := token;
      res:= Cons(t.read(), res) ; token := t.nextToken(wx)
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

PROCEDURE ReadTail(t : T; 
                   wx : Wx) : Object RAISES { E } =
  VAR token := t.nextToken(wx);
  BEGIN
    IF    token = EOF THEN
      RETURN Error("EOF during read.")
    ELSIF token = RP THEN
      RETURN NIL
    ELSIF token = DOT THEN
      WITH result = t.read() DO
        token := t.nextToken();
        IF token # RP THEN
          t.warn("Where's the ')'?  Got " & Stringify(token) &
            " after . ")
        END;
        RETURN result
      END
    ELSE
      t.isPushedToken := TRUE;
      t.pushedToken := token;
      RETURN Cons(t.read(), t.readTail(wx))
    END
  END ReadTail;

PROCEDURE NextToken(t : T; wx : Wx) : Object RAISES { E } =

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
        RETURN t.nextToken()
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
            RETURN ListToVector(t.read())
        | 
          ORD(BSC) =>
            ch := t.getCh();
            IF VAL(ch,CHAR) IN SET OF CHAR { 's', 'S', 'n', 'N' } THEN
              EVAL t.pushChar(ch);
              WITH token = t.nextToken() DO
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
          ORD('e'), ORD('i'), ORD('d') => RETURN t.nextToken()
        |
          ORD('b'), ORD('o'), ORD('x') =>
            t.warn("#" & Text.FromChar(VAL(ch,CHAR)) & 
                      " not implemented, ignored");
            RETURN t.nextToken()
        ELSE
          t.warn("#" & Text.FromChar(VAL(ch,CHAR)) & 
                    " not recognized, ignored");
          RETURN t.nextToken()
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

        IF c IN NumberChars THEN
          WITH txt = WxToText(wx) DO
            (* note we are stricter than Modula-3 here.
               we allow only "e" as the exponent marker.  Not E, d, D, x, or X. *)

            IF HaveAlphasOtherThane(txt) THEN
            ELSE
              EVAL WxReset(wx);
              TRY
                WITH lr = Scan.LongReal(txt), 
                     lrp = NEW(SchemeLongReal.T) DO
                  lrp^ := lr;
                  RETURN lrp
                END
              EXCEPT
                Lex.Error, FloatMode.Trap => 
                  WxPutText(wx, txt) (* restore END *);
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
