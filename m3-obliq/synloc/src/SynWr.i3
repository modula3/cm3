(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Fri Jun  3 11:39:06 1994 by luca                   *)

INTERFACE SynWr;
IMPORT Wr (*, NetObj, Thread *);

TYPE 
  T = (* NetObj.T *) OBJECT METHODS
    underlyingWr(): Wr.T (* RAISES {NetObj.Error, Thread.Alerted} *);
    beg(indent: INTEGER:=0; loud:=FALSE) 
       (* RAISES {NetObj.Error, Thread.Alerted} *);
    break(loud:=FALSE) (* RAISES {NetObj.Error, Thread.Alerted} *);
    flatBreak(loud:=FALSE) (* RAISES {NetObj.Error, Thread.Alerted} *);
    end(loud:=FALSE) (* RAISES {NetObj.Error, Thread.Alerted} *);
    char(c: CHAR; loud:=FALSE) (* RAISES {NetObj.Error, Thread.Alerted} *);
    text(t: TEXT; loud:=FALSE) (* RAISES {NetObj.Error, Thread.Alerted} *);
    newLine(loud:=FALSE) (* RAISES {NetObj.Error, Thread.Alerted} *);
    flush(loud:=FALSE) (* RAISES {NetObj.Error, Thread.Alerted} *);
    close() (* RAISES {NetObj.Error, Thread.Alerted} *);
    pushSilence() (* RAISES {NetObj.Error, Thread.Alerted} *);
    popSilence() (* RAISES {NetObj.Error, Thread.Alerted} *);
  END;

PROCEDURE Setup();
(* To be called before any other use of this module. *)

VAR (*READONLY*) out, err: T;
(* Two Formatter.T's based on stdout and stderr *)

PROCEDURE New(wr: Wr.T; width: CARDINAL:=75): T;
(* A new Formatter.T based on wr. *)

PROCEDURE UnderlyingWr (swr: T): Wr.T;
(* Returns the writer that is connected to the output of swr. *)

PROCEDURE Beg(swr: T; indent: INTEGER:=0; loud:=FALSE);
(* Formatter.Begin(out, indent) *)

PROCEDURE Break(swr: T; loud:=FALSE);
(* Formatter.UnitedBreak(swr.wr) *)

PROCEDURE FlatBreak(swt: T; loud:=FALSE);
(* Formatter.Break(swr.wr) *)

PROCEDURE End(swr: T; loud:=FALSE);
(* Formatter.End(swr.wr) *)

PROCEDURE Char(swr: T; c: CHAR; loud:=FALSE);
(* Formatter.PutChar(swr.wr, c) *)

PROCEDURE Text(swr: T; t: TEXT; loud:=FALSE);
(* Formatter.PutText(swr.wr, t) *)

PROCEDURE NewLine(swr: T; loud:=FALSE);
(* Formatter.NewLine(swr.wr) *)

PROCEDURE Flush(swt: T; loud:=FALSE);
(* Formatter.Flush(swr.wr) *)

PROCEDURE Close(swt: T);
(* Formatter.Close(swr.wr) *)

PROCEDURE PushSilence(swr: T);
(* Pushes one level of silence on a stack. When the silence stack is 
   non-empty, the output operations have no effect on swr, 
   unless they are "loud". *)

PROCEDURE PopSilence(swr: T);
(* Pops one level of silence from the silence stack of swr. *)

END SynWr.
