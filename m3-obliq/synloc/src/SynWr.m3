(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Created by luca                   *)
(*                                                                           *)
(* Parts Copyright (C) 1997, Columbia University                             *)
(* All rights reserved.                                                      *)
(*
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 14:53:30 1997
 *)

MODULE SynWr; 
IMPORT Stdio, Wr, Formatter, Thread(*, NetObj, NullWr*); 

VAR setupDone := FALSE;

PROCEDURE Setup() =
  BEGIN
    IF NOT setupDone THEN
      setupDone := TRUE;
      out := New(Stdio.stdout, 75);
      err := New(Stdio.stderr, 75);
    END;
  END Setup;

TYPE
  Private = T BRANDED "SynWr.Private" OBJECT
    mu: Thread.Mutex;
    nesting: INTEGER;
    fmt: Formatter.T;
    silent: INTEGER;
    open: BOOLEAN;
  OVERRIDES
    underlyingWr := UnderlyingWrP;
    beg := BegP;
    break := BreakP;
    flatBreak := FlatBreakP;
    end := EndP;
    char := CharP;
    text := TextP;
    newLine := NewLineP;
    flush := FlushP;
    close := CloseP;
    pushSilence := PushSilenceP;
    popSilence := PopSilenceP;
  END;

PROCEDURE New(wr: Wr.T; width: CARDINAL:=75): T =
  BEGIN
    RETURN NEW(Private, mu:=NEW(Thread.Mutex), nesting := 0, 
               fmt:=Formatter.New(wr, width), silent:=0, open:=TRUE);
  END New;

PROCEDURE UnderlyingWrP (swr: Private): Wr.T =
  BEGIN
    LOCK swr.mu DO
      RETURN Formatter.UnderlyingWr(swr.fmt);
    END;
  END UnderlyingWrP;

PROCEDURE BegP(swr: Private; indent: INTEGER:=0; loud:=FALSE) =
BEGIN 
  LOCK swr.mu DO
    IF swr.open AND ((swr.silent = 0) OR loud) THEN
      TRY
        Formatter.Begin(swr.fmt, indent);
        INC(swr.nesting);
      EXCEPT Wr.Failure => END;
    END;
  END;
END BegP;

PROCEDURE BreakP(swr: Private; loud:=FALSE) =
BEGIN 
  LOCK swr.mu DO
    IF swr.open AND ((swr.silent = 0) OR loud) THEN
      TRY
        Formatter.UnitedBreak(swr.fmt); 
      EXCEPT Wr.Failure => END;
    END;
  END;
END BreakP;

PROCEDURE FlatBreakP(swr: Private; loud:=FALSE) =
BEGIN 
  LOCK swr.mu DO
    IF swr.open AND ((swr.silent = 0) OR loud) THEN
      TRY
        Formatter.Break(swr.fmt); 
      EXCEPT Wr.Failure => END;
    END;
  END;
END FlatBreakP;

PROCEDURE EndP(swr: Private; loud:=FALSE) =
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
END EndP;

PROCEDURE CharP(swr: Private; c: CHAR; loud:=FALSE) =
BEGIN
  LOCK swr.mu DO
    IF swr.open AND ((swr.silent = 0) OR loud) THEN
      TRY
        Formatter.PutChar(swr.fmt, c); 
      EXCEPT Wr.Failure => END;
   END;
  END;
END CharP;

PROCEDURE TextP(swr: Private; t: TEXT; loud:=FALSE) =
BEGIN 
  LOCK swr.mu DO
    IF swr.open AND ((swr.silent = 0) OR loud) THEN
      TRY
        Formatter.PutText(swr.fmt, t); 
      EXCEPT Wr.Failure => END;
    END;
  END;
END TextP;

PROCEDURE NewLineP(swr: Private; loud:=FALSE) =
BEGIN 
  LOCK swr.mu DO
    IF swr.open AND ((swr.silent = 0) OR loud) THEN
      TRY
        Formatter.NewLine(swr.fmt); 
      EXCEPT Wr.Failure => END;
    END;
  END;
END NewLineP;

PROCEDURE FlushP(swr: Private; loud:=FALSE) =
BEGIN 
  LOCK swr.mu DO
    IF swr.open AND ((swr.silent = 0) OR loud) THEN
      TRY
        Formatter.Flush(swr.fmt);
        swr.nesting := 0;
      EXCEPT Wr.Failure => END;
    END;
  END;
END FlushP;

PROCEDURE CloseP(swr: Private) =
BEGIN
  LOCK swr.mu DO
    swr.open := FALSE;
    swr.nesting := 0;
    TRY
      Formatter.Close(swr.fmt);
    EXCEPT Wr.Failure => END;
  END;
END CloseP;

PROCEDURE PushSilenceP(swr: Private) =
  BEGIN
    LOCK swr.mu DO
      INC(swr.silent);
    END;
  END PushSilenceP;

PROCEDURE PopSilenceP(swr: Private) =
  BEGIN
    LOCK swr.mu DO
      swr.silent := MAX(swr.silent-1, 0);
    END;
  END PopSilenceP;

(*******************************************************************)
(* convenience routines                                            *)
(*******************************************************************)
PROCEDURE UnderlyingWr (swr: T): Wr.T =
  BEGIN
    (*TRY*)
      RETURN swr.underlyingWr();
    (*EXCEPT NetObj.Error, Thread.Alerted => 
      (* silently suck them up *)
      RETURN NEW(NullWr.T).init();
    END;
    *)
  END UnderlyingWr;

PROCEDURE Beg(swr: T; indent: INTEGER:=0; loud:=FALSE) =
  BEGIN 
    (*TRY*)
      swr.beg(indent,loud);
    (*EXCEPT NetObj.Error, Thread.Alerted => (* silently suck them up *)
    END;*)
  END Beg;

PROCEDURE Break(swr: T; loud:=FALSE) =
  BEGIN 
    (*TRY*)
      swr.break(loud);
    (*EXCEPT NetObj.Error, Thread.Alerted => (* silently suck them up *)
    END;*)
  END Break;

PROCEDURE FlatBreak(swr: T; loud:=FALSE) =
  BEGIN 
    (*TRY*)
      swr.flatBreak(loud);
    (*EXCEPT NetObj.Error, Thread.Alerted => (* silently suck them up *)
    END;*)
  END FlatBreak;

PROCEDURE End(swr: T; loud:=FALSE) =
  BEGIN 
    (*TRY*)
      swr.end(loud);
    (*EXCEPT NetObj.Error, Thread.Alerted => (* silently suck them up *)
    END;*)
  END End;

PROCEDURE Char(swr: T; c: CHAR; loud:=FALSE) =
  BEGIN
    (*TRY*)
      swr.char(c,loud);
    (*EXCEPT NetObj.Error, Thread.Alerted => (* silently suck them up *)
    END;*)
  END Char;

PROCEDURE Text(swr: T; t: TEXT; loud:=FALSE) =
  BEGIN 
    (*TRY*)
      swr.text(t,loud);
    (*EXCEPT NetObj.Error, Thread.Alerted => (* silently suck them up *)
    END;*)
  END Text;

PROCEDURE NewLine(swr: T; loud:=FALSE) =
  BEGIN 
    (*TRY*)
      swr.newLine(loud);
    (*EXCEPT NetObj.Error, Thread.Alerted => (* silently suck them up *)
    END;*)
  END NewLine;

PROCEDURE Flush(swr: T; loud:=FALSE) =
  BEGIN 
    (*TRY*)
      swr.flush(loud);
    (*EXCEPT NetObj.Error, Thread.Alerted => (* silently suck them up *)
    END;*)
  END Flush;

PROCEDURE Close(swr: T) =
  BEGIN
    (*TRY*)
      swr.close();
    (*EXCEPT NetObj.Error, Thread.Alerted => (* silently suck them up *)
    END;*)
  END Close;

PROCEDURE PushSilence(swr: T) =
  BEGIN
    (*TRY*)
      swr.pushSilence();
    (*EXCEPT NetObj.Error, Thread.Alerted => (* silently suck them up *)
    END;*)
  END PushSilence;

PROCEDURE PopSilence(swr: T) =
  BEGIN
    (*TRY*)
      swr.popSilence();
    (*EXCEPT NetObj.Error, Thread.Alerted => (* silently suck them up *)
    END;*)
  END PopSilence;

BEGIN
END SynWr.
