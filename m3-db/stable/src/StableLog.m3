(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Created by Carsten Weich                                    *)
(*                                                             *)
(* Last modified on Thu Jan 19 13:13:18 PST 1995 by kalsow     *)
(*      modified on Tue Sep 27 19:18:12 PDT 1994 by weich      *)

UNSAFE MODULE StableLog;

IMPORT Rd, Wr, WrClass, RdClass, Text, Text8,
       Thread, Pickle, 
       StableError, RdUtils, Word;

REVEAL
  WrClass.Private <: MUTEX;
  RdClass.Private <: MUTEX;

<*FATAL Thread.Alerted*>

CONST
  CallMark     = 27353;  (* this is Gregs birthday... *)
  EndCallMark  = 30965;  (* ...Carstens birthday...*)

  Mtext        = ORD('t');
  Mpickle      = ORD('P');

PROCEDURE OutCall (log: Wr.T; procId: CARDINAL) =
  BEGIN
    OutInteger(log, CallMark);
    OutInteger(log, procId)
  END OutCall;

PROCEDURE OutCallEndMark (log: Wr.T) =
  BEGIN
    OutInteger(log, EndCallMark)
  END OutCallEndMark;

PROCEDURE InCall (log: Rd.T; max: CARDINAL): CARDINAL RAISES {Error} =
  BEGIN
    IF InInteger(log) # CallMark THEN RAISE Error END;
    VAR res:= InInteger(log); BEGIN
      IF res > max OR res < 0 THEN RAISE Error END;
      RETURN res
    END
  END InCall;

PROCEDURE CheckCallEndMark (log: Rd.T): BOOLEAN =
  BEGIN
    TRY
      RETURN EndCallMark = InInteger(log)
    EXCEPT
      Error => RETURN FALSE
    END
  END CheckCallEndMark;

PROCEDURE OutRef(log: Wr.T; r: REFANY) =
  BEGIN
    TYPECASE r OF
    | TEXT(x) => OutInteger(log, Mtext); OutText(log, x);
    ELSE
      TRY
        OutInteger(log, Mpickle);
        Pickle.Write(log, r)
      EXCEPT
        Wr.Failure (err) =>
        StableError.Halt(
            "Cannot write to logfile: " & RdUtils.FailureText(err))
      | Pickle.Error (msg) =>
        StableError.Halt("Cannot write to logfile: Pickle error: " & msg)
      END
    END
  END OutRef;

PROCEDURE InRef(log: Rd.T): REFANY RAISES {Error} =
  VAR r: REFANY;
      code:= InInteger(log);
  BEGIN
    IF code = Mpickle THEN
      TRY
        r:= Pickle.Read(log)
      EXCEPT
      | Pickle.Error, Rd.EndOfFile => RAISE Error
      | Rd.Failure (err) =>
        StableError.Halt(
            "InRef: Can not read log file: " & RdUtils.FailureText(err))
      END
    ELSIF code = Mtext THEN
      r:= InText(log)
    ELSE
      RAISE Error
    END;
    RETURN r
  END InRef;


(* Procedures for generic logging of procedure parameters *)

PROCEDURE OutChar (log: Wr.T; c: CHAR) =
  BEGIN
    TRY
      Wr.PutChar(log, c)
    EXCEPT
      Wr.Failure (err) =>
        StableError.Halt(
          "Cannot write to logfile: " & RdUtils.FailureText(err))
    END
  END OutChar;

PROCEDURE OutChars (log: Wr.T; READONLY chars: ARRAY OF CHAR) =
  VAR n:= NUMBER(chars) - NUMBER(chars) MOD BYTESIZE(Word.T);
  BEGIN
    TRY
      Wr.PutString(log, SUBARRAY(chars, 0, n));
      FOR i:= n TO LAST(chars) DO
        Wr.PutChar(log, chars[i])
      END
    EXCEPT
      Wr.Failure (err) =>
        StableError.Halt(
          "Cannot write to logfile: " & RdUtils.FailureText(err))
    END
  END OutChars;


PROCEDURE OutInteger (log: Wr.T; i: INTEGER) =
  BEGIN
    TRY
      Wr.PutString(log, LOOPHOLE(i, ARRAY [0..BYTESIZE(INTEGER)-1] OF CHAR));
    EXCEPT
      Wr.Failure (err) =>
        StableError.Halt(
          "Cannot write to logfile: " & RdUtils.FailureText(err))
    END
  END OutInteger;

PROCEDURE OutCardinal (log: Wr.T; card: CARDINAL) =
  BEGIN
    OutInteger(log, card)
  END OutCardinal;

PROCEDURE OutBoolean (log: Wr.T; bool: BOOLEAN) =
  BEGIN
    TRY
      Wr.PutChar(log, VAL(ORD(bool), CHAR))
    EXCEPT
      Wr.Failure (err) =>
        StableError.Halt(
          "Cannot write to logfile: " & RdUtils.FailureText(err))
    END
  END OutBoolean;

PROCEDURE OutReal (log: Wr.T; r: REAL) =
  BEGIN
    TRY
      Wr.PutString(log, LOOPHOLE(r, ARRAY [0..BYTESIZE(REAL)-1] OF CHAR));
    EXCEPT
      Wr.Failure (err) =>
        StableError.Halt(
          "Cannot write to logfile: " & RdUtils.FailureText(err))
    END
  END OutReal;

PROCEDURE OutLongreal (log: Wr.T; r: LONGREAL) =
  BEGIN
    TRY
      Wr.PutString(log, LOOPHOLE(r, ARRAY [0..BYTESIZE(LONGREAL)-1] OF CHAR));
    EXCEPT
      Wr.Failure (err) =>
        StableError.Halt(
          "Cannot write to logfile: " & RdUtils.FailureText(err))
    END
  END OutLongreal;

PROCEDURE OutExtended (log: Wr.T; r: EXTENDED) =
  BEGIN
    TRY
      Wr.PutString(log, LOOPHOLE(r, ARRAY [0..BYTESIZE(EXTENDED)-1] OF CHAR));
    EXCEPT
      Wr.Failure (err) =>
        StableError.Halt(
          "Cannot write to logfile: " & RdUtils.FailureText(err))
    END
  END OutExtended;

PROCEDURE OutText(log: Wr.T; text: TEXT) =
  VAR len, start: INTEGER;  buf: ARRAY [0..255] OF CHAR;
  BEGIN
    IF text # NIL THEN
      len := Text.Length(text)
    ELSE
      len := -1
    END;
    OutInteger(log, len);
    start := 0;
    WHILE start < len DO
      Text.SetChars(buf, text, start);
      OutChars(log, SUBARRAY(buf, 0, MIN(NUMBER(buf), len - start)));
      INC (start, NUMBER(buf));
    END;
  END OutText;


(* The following procedures are provided in support of generic reading the
   log. *)

PROCEDURE InChar (log: Rd.T): CHAR RAISES {Error} =
  BEGIN
    TRY
      RETURN Rd.GetChar(log)
    EXCEPT
      Rd.EndOfFile => RAISE Error
    | Rd.Failure (err) =>
        <*NOWARN*> StableError.Halt(
          "InChar: Can not read log file: " & RdUtils.FailureText(err))
    END
  END InChar;

PROCEDURE InCharsLen (log: Rd.T): CARDINAL RAISES {Error} =
  BEGIN
    RETURN InInteger(log)
  END InCharsLen;

PROCEDURE InChars (    log  : Rd.T;
                   VAR chars: ARRAY OF CHAR)
  RAISES {Error} =
  VAR n:= NUMBER(chars) - NUMBER(chars) MOD BYTESIZE(Word.T);
  BEGIN
    TRY
      IF Rd.GetSub(log, SUBARRAY(chars, 0, n)) # n THEN
        RAISE Error
      END;
      FOR i:= n TO LAST(chars) DO
        chars[i]:= Rd.GetChar(log)
      END
    EXCEPT
      Rd.EndOfFile => RAISE Error
    | Rd.Failure (err) =>
        StableError.Halt("InChars: Can not read log file: "
                           & RdUtils.FailureText(err))
    END
  END InChars;

PROCEDURE InInteger (log: Rd.T;
                     min         := FIRST(INTEGER);
                     max         := LAST(INTEGER)   ):
    INTEGER RAISES {Error} =
  VAR 
    i: INTEGER;
  BEGIN
    TRY
      IF Rd.GetSub(log, LOOPHOLE(i, ARRAY [0..BYTESIZE(INTEGER)-1] OF CHAR))
          # BYTESIZE(INTEGER) THEN
        RAISE Error
      END;
    EXCEPT
    | Rd.Failure (err) =>
        StableError.Halt("InInteger: Can not read log file: "
                           & RdUtils.FailureText(err))
    END;

    IF min <= i AND i <= max THEN
      RETURN i
    ELSE
      RAISE Error
    END (*IF*)
  END InInteger;

PROCEDURE InCardinal (log: Rd.T;
                      lim: CARDINAL := LAST(CARDINAL)):
  CARDINAL RAISES {Error} =
  BEGIN
    RETURN InInteger(log, 0, lim)
  END InCardinal;

PROCEDURE InBoolean (log: Rd.T): BOOLEAN RAISES {Error} =
  BEGIN
    TRY
      RETURN Rd.GetChar(log) = VAL(ORD(TRUE), CHAR)
    EXCEPT
      Rd.EndOfFile => RAISE Error
    | Rd.Failure (err) =>
        <*NOWARN*> StableError.Halt(
          "InBoolean: Can not read log file: " & RdUtils.FailureText(err))
    END
  END InBoolean;

PROCEDURE InReal (log: Rd.T): REAL RAISES {Error} =
  VAR 
    r: REAL;
  BEGIN
    TRY
      IF Rd.GetSub(log, LOOPHOLE(r, ARRAY [0..BYTESIZE(REAL)-1] OF CHAR))
          # BYTESIZE(REAL) THEN
        RAISE Error
      END;
    EXCEPT
    | Rd.Failure (err) =>
        StableError.Halt("InReal: Can not read log file: "
                           & RdUtils.FailureText(err))
    END;
    RETURN r;
  END InReal;

PROCEDURE InLongreal (log: Rd.T): LONGREAL RAISES {Error} =
  VAR 
    r: LONGREAL;
  BEGIN
    TRY
      IF Rd.GetSub(log, LOOPHOLE(r, ARRAY [0..BYTESIZE(LONGREAL)-1] OF CHAR))
          # BYTESIZE(LONGREAL) THEN
        RAISE Error
      END;
    EXCEPT
    | Rd.Failure (err) =>
        StableError.Halt("InLongreal: Can not read log file: "
                           & RdUtils.FailureText(err))
    END;
    RETURN r;
  END InLongreal;

PROCEDURE InExtended (log: Rd.T): EXTENDED RAISES {Error} =
  VAR 
    r: EXTENDED;
  BEGIN
    TRY
      IF Rd.GetSub(log, LOOPHOLE(r, ARRAY [0..BYTESIZE(EXTENDED)-1] OF CHAR))
          # BYTESIZE(EXTENDED) THEN
        RAISE Error
      END;
    EXCEPT
    | Rd.Failure (err) =>
        StableError.Halt("InExtended: Can not read log file: "
                           & RdUtils.FailureText(err))
    END;
    RETURN r;
  END InExtended;

PROCEDURE InText(log: Rd.T) : TEXT
   RAISES {Error} =
  VAR len: INTEGER;
      txt: Text8.T;
      buf: ARRAY [0..255] OF CHAR;
  BEGIN
    len := InInteger(log);
    IF len = -1 THEN
      RETURN NIL
    ELSIF len < 0 THEN
      RAISE Error
    ELSIF len = 0 THEN
      RETURN "";
    ELSIF len <= NUMBER(buf) THEN
      InChars(log, SUBARRAY(buf, 0, len));
      RETURN Text.FromChars(SUBARRAY(buf, 0, len));
    ELSE
      txt := Text8.Create(len);
      InChars(log, SUBARRAY(txt.contents^, 0, len));
      RETURN txt
    END
  END InText;

BEGIN
END StableLog.
