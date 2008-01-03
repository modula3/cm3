(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Fri Jun  3 11:38:39 1994 by luca                   *)

MODULE SynWr; 
IMPORT Stdio, Wr, Formatter, Thread; 

VAR setupDone := FALSE;

PROCEDURE Setup() =
  BEGIN
    IF NOT setupDone THEN
      setupDone := TRUE;
      out := New(Stdio.stdout, 75);
      err := New(Stdio.stderr, 75);
    END;
  END Setup;

REVEAL T =
  BRANDED OBJECT
    mu: Thread.Mutex;
    nesting: INTEGER;
    fmt: Formatter.T;
    silent: INTEGER;
    open: BOOLEAN;
  END;

PROCEDURE New(wr: Wr.T; width: CARDINAL:=75): T =
  BEGIN
    RETURN NEW(T, mu:=NEW(Thread.Mutex), nesting := 0, 
               fmt:=Formatter.New(wr, width), silent:=0, open:=TRUE);
  END New;

PROCEDURE UnderlyingWr (swr: T): Wr.T =
  BEGIN
    LOCK swr.mu DO
      RETURN Formatter.UnderlyingWr(swr.fmt);
    END;
  END UnderlyingWr;

PROCEDURE Beg(swr: T; indent: INTEGER:=0; loud:=FALSE) =
BEGIN 
  LOCK swr.mu DO
    IF swr.open AND ((swr.silent = 0) OR loud) THEN
      TRY
        Formatter.Begin(swr.fmt, indent);
        INC(swr.nesting);
      EXCEPT Wr.Failure => END;
    END;
  END;
END Beg;

PROCEDURE Break(swr: T; loud:=FALSE) =
BEGIN 
  LOCK swr.mu DO
    IF swr.open AND ((swr.silent = 0) OR loud) THEN
      TRY
        Formatter.UnitedBreak(swr.fmt); 
      EXCEPT Wr.Failure => END;
    END;
  END;
END Break;

PROCEDURE FlatBreak(swr: T; loud:=FALSE) =
BEGIN 
  LOCK swr.mu DO
    IF swr.open AND ((swr.silent = 0) OR loud) THEN
      TRY
        Formatter.Break(swr.fmt); 
      EXCEPT Wr.Failure => END;
    END;
  END;
END FlatBreak;

PROCEDURE End(swr: T; loud:=FALSE) =
BEGIN 
  LOCK swr.mu DO
    IF swr.open AND ((swr.silent = 0) OR loud) THEN
      IF swr.nesting > 0 THEN
        TRY
          DEC(swr.nesting);
          Formatter.End(swr.fmt);
        EXCEPT Wr.Failure => END;
      END;
    END;
  END;
END End;

PROCEDURE Char(swr: T; c: CHAR; loud:=FALSE) =
BEGIN
  LOCK swr.mu DO
    IF swr.open AND ((swr.silent = 0) OR loud) THEN
      TRY
        Formatter.PutChar(swr.fmt, c); 
      EXCEPT Wr.Failure => END;
   END;
  END;
END Char;

PROCEDURE Text(swr: T; t: TEXT; loud:=FALSE) =
BEGIN 
  LOCK swr.mu DO
    IF swr.open AND ((swr.silent = 0) OR loud) THEN
      TRY
        Formatter.PutText(swr.fmt, t); 
      EXCEPT Wr.Failure => END;
    END;
  END;
END Text;

PROCEDURE NewLine(swr: T; loud:=FALSE) =
BEGIN 
  LOCK swr.mu DO
    IF swr.open AND ((swr.silent = 0) OR loud) THEN
      TRY
        Formatter.NewLine(swr.fmt); 
      EXCEPT Wr.Failure => END;
    END;
  END;
END NewLine;

PROCEDURE Flush(swr: T; loud:=FALSE) =
BEGIN 
  LOCK swr.mu DO
    IF swr.open AND ((swr.silent = 0) OR loud) THEN
      TRY
        Formatter.Flush(swr.fmt);
        swr.nesting := 0;
      EXCEPT Wr.Failure => END;
    END;
  END;
END Flush;

PROCEDURE Close(swr: T) =
BEGIN
  LOCK swr.mu DO
    swr.open := FALSE;
    swr.nesting := 0;
    TRY
      Formatter.Close(swr.fmt);
    EXCEPT Wr.Failure => END;
  END;
END Close;

PROCEDURE PushSilence(swr: T) =
  BEGIN
    LOCK swr.mu DO
      INC(swr.silent);
    END;
  END PushSilence;

PROCEDURE PopSilence(swr: T) =
  BEGIN
    LOCK swr.mu DO
      swr.silent := MAX(swr.silent-1, 0);
    END;
  END PopSilence;

BEGIN
END SynWr.
