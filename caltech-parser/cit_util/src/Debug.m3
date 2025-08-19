(*                                                                           *)
(*  Debug.m3                                                                 *)
(*                                                                           *)
(*  Debugging output and aborting the program.                               *)
(*                                                                           *)
(*  Copyright (c) 2000 California Institute of Technology                    *)
(*  All rights reserved.                                                     *)
(*  Department of Computer Science                                           *)
(*  Pasadena, CA 91125.                                                      *)
(*                                                                           *)
(*  Author: Mika Nystrom <mika@cs.caltech.edu>                               *)
(*                                                                           *)
(*  Permission to use, copy, modify, and distribute this software            *)
(*  and its documentation for any purpose and without fee is hereby          *)
(*  granted, provided that the above copyright notice appear in all          *)
(*  copies. The California Institute of Technology makes no representations  *)
(*  about the suitability of this software for any purpose. It is            *)
(*  provided "as is" without express or implied warranty. Export of this     *)
(*  software outside of the United States of America may require an          *)
(*  export license.                                                          *)
(*                                                                           *)
(* $Id: Debug.m3,v 1.33 2009/09/09 10:36:22 mika Exp $ *)

MODULE Debug;
FROM DebugClass IMPORT level;
IMPORT TextSet;
IMPORT TextSetDef;
IMPORT FileRd;
IMPORT Rd;
IMPORT BreakHere;
IMPORT Thread;
IMPORT OSError;
IMPORT Wr, TextWr, Text;
FROM Stdio IMPORT stderr;
IMPORT Env, Scan;
IMPORT FloatMode, Lex;
IMPORT Fmt;
IMPORT Process;
IMPORT ThreadF;
IMPORT Pathname;
IMPORT LockedTextBooleanTbl;
IMPORT RdWrPipe;
IMPORT TZ, XTime AS Time;
IMPORT DebugStream, DebugStreamList;
IMPORT RTParams, TextReader, FileWr, AL;
IMPORT TextTextTbl;
IMPORT RefList;

VAR options := SET OF Options {};

PROCEDURE SetOptions(newOptions : SET OF Options) =
  BEGIN options := newOptions END SetOptions;

PROCEDURE GetOptions() : SET OF Options = BEGIN RETURN options END GetOptions;

VAR envOverrides := NEW(LockedTextBooleanTbl.Default).init();
    valOverrides := NEW(TextTextTbl.Default).init();

PROCEDURE EnableOptions(q : SET OF Options) =
  BEGIN options := options + q END EnableOptions;
  

PROCEDURE DisableOptions(q : SET OF Options) =
  BEGIN options := options - q END DisableOptions;

PROCEDURE SetEnv(var : TEXT) =
  BEGIN EVAL envOverrides.put(var,TRUE) END SetEnv;

PROCEDURE ClearEnv(var : TEXT) =
  BEGIN EVAL envOverrides.put(var,FALSE) END ClearEnv;

PROCEDURE GetVal(var : TEXT) : TEXT =
  VAR
    val : TEXT;
  BEGIN
    IF valOverrides.get(var,val) THEN
      RETURN val
    ELSE
      RETURN Env.Get(var)
    END
  END GetVal;

PROCEDURE HaveEnv(var : TEXT) : BOOLEAN =
  VAR b : BOOLEAN; BEGIN
    IF envOverrides.get(var, b) THEN
      RETURN b
    ELSE
      RETURN GetVal(var) # NIL
    END
  END HaveEnv;

PROCEDURE DebugThis(this : TEXT; def : BOOLEAN) : BOOLEAN =

  VAR
    env := "DEBUG" & this;
    res := (def OR HaveEnv("DEBUGEVERYTHING")) AND NOT HaveEnv("NO" & env) OR 
           HaveEnv(env) AND NOT HaveEnv("DEBUGNOTHING");
  BEGIN
    IF level > 100 THEN
      Out("DebugThis: " & env & " = " & Fmt.Bool(res),0)
    END;
    RETURN res
  END DebugThis;

VAR pidText := "(" & Fmt.Int(Process.GetMyID()) & ") ";
 
PROCEDURE OutFilePos(file     : Pathname.T;
                     pos      : CARDINAL;
                     t        : TEXT;
                     minLevel : CARDINAL;
                     cr   :=TRUE ) = 
  BEGIN
    (* for now... *)
    Out(file & ":" & Fmt.Int(pos) & ": " & t,minLevel,cr)
  END OutFilePos;

(* 
   The code below here is very inefficient.
   
   Project for future: change code so that text can either be 
   written left to right or else make a small virtual machine that
   can figure out exactly how to assemble the text left to right
   at the end. 
*)

PROCEDURE PrependText(pre, t : TEXT) : TEXT =

  PROCEDURE Find() : [-1..LAST(CARDINAL) ] =
    BEGIN
      found := Text.FindChar(t, '\n');
      RETURN found
    END Find; 

  VAR
    res := pre;
    found : [-1..LAST(CARDINAL) ];
  BEGIN
    WHILE Find() # -1 DO
      res := res & Text.Sub(t, 0, found+1);
      t := pre & Text.Sub(t, found+1)
    END;
    res := res & t;
    RETURN res
  END PrependText;

VAR tMu := NEW(MUTEX);
    tz : TZ.T;
    lastTime := FIRST(Time.T);

PROCEDURE Out(t: TEXT; minLevel : CARDINAL; cr:=TRUE; this : TEXT := NIL) =
  VAR timeText : TEXT := NIL;
  BEGIN
    IF this # NIL AND NOT DebugThis(this,FALSE) THEN 
      RETURN 
    END;

    IF minLevel > level THEN RETURN END;
    IF debugFilter # NIL THEN
      IF triggers.member(t) THEN
        BreakHere.Please();
      END;
    END;

    IF Options.PrintTime IN options THEN
      WITH now = Time.Now() DO
        LOCK tMu DO
          (* ok this is a bit messy, remember that for modularity 
             reasons, we will allow the routines here to call Debug.Out! *)

          TRY
            IF tz = NIL THEN 
              VAR 
                newTz : TZ.T;
              BEGIN
                
                Thread.Release(tMu);
                TRY
                  newTz := TZ.New(DebugTimeZone)
                FINALLY
                  Thread.Acquire(tMu)
                END;
                  
                tz := newTz
              END
            END;
            

            IF TRUNC(now) # TRUNC(lastTime) THEN
              (* 
                 lastTime assignment must go FIRST in case debug level
                 is so high that we are called re-entrantly from 
                 TZ.FormatSubsecond !
              *)
              lastTime := now;

              Thread.Release(tMu);
              TRY
                timeText := "****** " & 
                                TZ.FormatSubsecond(tz,now,printMillis := FALSE) &
                                                  " " & DebugTimeZone;
              FINALLY
                Thread.Acquire(tMu)
              END
            END
          EXCEPT
            OSError.E => timeText := "****** TIMEZONE ERROR " & DebugTimeZone
          END
        END
      END
    END;

    IF Options.PrintThreadID IN options THEN
      WITH threadText = "((" & Fmt.Int(ThreadF.MyId()) & ")) " DO
        t := PrependText(threadText,t)
      END
    END;

    IF Options.PrintPID IN options THEN
      t := PrependText(pidText,t)
    END;

    t:=UnNil(t);
    IF cr THEN
      t:=t&"\n";
    END;

    IF timeText # NIL THEN
      outHook(timeText & "\n" & t)
    ELSE
      outHook(t)
    END
  END Out;

PROCEDURE HexOut(t        : TEXT;
                 minLevel : CARDINAL;
                 cr       : BOOLEAN; 
                 toHex    : PROCEDURE (t : TEXT) : TEXT) =
  BEGIN
    Out(toHex(t), minLevel, cr)
  END HexOut;

PROCEDURE ToHex(t : TEXT) : TEXT =
  <* FATAL Thread.Alerted, Wr.Failure *>
  <*UNUSED*> CONST 
    BackSlash = VAL(8_134, CHAR); 
  CONST
    brax = TRUE;
  VAR
    wr := NEW(TextWr.T).init();
  CONST
    OK = SET OF CHAR { ' '..'~' }; (* the whole standard printable set *)
  BEGIN
    FOR i := 0 TO Text.Length(t)-1 DO
      WITH c = Text.GetChar(t,i) DO
        IF c IN OK THEN 
          Wr.PutChar(wr,c) 
        ELSE 
          IF brax THEN
            Wr.PutText(wr, Fmt.F("[\\%03s]",Fmt.Int(ORD(c))))
          ELSE
            Wr.PutText(wr, Fmt.F("\\%03s",Fmt.Int(ORD(c))))
          END
        END
      END
    END;
    RETURN TextWr.ToText(wr)
  END ToHex;  

PROCEDURE S(t:TEXT;minLevel:CARDINAL;cr:=TRUE)=BEGIN Out(t,minLevel,cr);END S;

PROCEDURE Warning(txt : TEXT) =
  VAR
    p := warnStreams;
    t := "WARNING: " & UnNil(txt);
  BEGIN
    WHILE p # NIL DO
      TRY
        Wr.PutText(p.head.wr, t); 
        Wr.PutChar(p.head.wr, '\n'); 
        
        IF p.head.flushAlways THEN
          Wr.Flush(p.head.wr)
        END
      EXCEPT ELSE END;
      p := p.tail
    END;
    
    S(t, 0);
  END Warning;

PROCEDURE Error(t: TEXT; exit : BOOLEAN; exitCode : Process.ExitCode) =
  BEGIN
    IF exit THEN
      errCode := exitCode;
      errHook(t)
    ELSE
      S("ERROR: " & UnNil(t), 0)
    END
  END Error;

PROCEDURE Check(b : BOOLEAN; msg : TEXT; exit : BOOLEAN; exitCode : Process.ExitCode) =
  BEGIN IF NOT b THEN Error(msg,exit,exitCode) END END Check;

PROCEDURE UnNil(text : TEXT) : TEXT =
  BEGIN IF text = NIL THEN RETURN "(NIL)" ELSE RETURN text END END UnNil;

PROCEDURE RaiseLevel(newLevel : CARDINAL) = 
  BEGIN
    IF newLevel > level THEN level := newLevel END
  END RaiseLevel;

PROCEDURE LowerLevel(newLevel : CARDINAL) = 
  BEGIN
    IF newLevel < level THEN level := newLevel END
  END LowerLevel;

PROCEDURE SetLevel(newLevel : CARDINAL) = BEGIN level := newLevel END SetLevel;
  
PROCEDURE GetLevel() : CARDINAL = BEGIN RETURN level END GetLevel;

(* outHook *)
VAR
  errHook := DefaultError;
  errCode : Process.ExitCode := DefaultErrorExitCode;
  outHook := DefaultOut;
  outHookLevel:=-1;

PROCEDURE DefaultOut(t: TEXT) =
  VAR
    reset := FALSE;
  BEGIN
    LOCK mu DO
      INC(calls);
      IF calls >= ResetInterval THEN
        reset := TRUE; calls := 0
      END;

      VAR
        p := streams;
      BEGIN
        WHILE p # NIL DO
          IF reset = TRUE THEN RdWrPipe.ResetWrCounter(p.head.wr) END;

          TRY
            Wr.PutText(p.head.wr, t); 

            IF p.head.flushAlways THEN
              Wr.Flush(p.head.wr)
            END
          EXCEPT ELSE END;
          p := p.tail
        END
      END
    END
  END DefaultOut;

PROCEDURE AddStream(wr : Wr.T) = 
  BEGIN 
    LOCK mu DO 
      streams := DebugStreamList.Cons(DebugStream.T { wr }, streams) 
    END 
  END AddStream;

PROCEDURE AddWarnStream(wr : Wr.T) = 
  BEGIN 
    LOCK mu DO
      <*ASSERT wr # NIL*>
      warnStreams := DebugStreamList.Cons(DebugStream.T { wr          := wr,
                                                          flushAlways := TRUE},
                                          warnStreams) 
    END 
  END AddWarnStream;

PROCEDURE RemStream(wr : Wr.T) =
  VAR new, p : DebugStreamList.T := NIL; BEGIN
    LOCK mu DO
      p := streams;
      WHILE p # NIL DO
        IF p.head.wr # wr THEN new := DebugStreamList.Cons(p.head,new) END;
        p := p.tail
      END;
      streams := new
    END
  END RemStream;

PROCEDURE DefaultError(t: TEXT) =
  BEGIN
    S("ERROR: " & UnNil(t), 0);
    Process.Exit(errCode);
  END DefaultError; 

PROCEDURE RegisterHook(out: OutHook; level:=0) =
  BEGIN
    <* ASSERT level#outHookLevel *>
    IF level>outHookLevel THEN
      outHookLevel:=level;
      outHook := out;
    END;
  END RegisterHook;

PROCEDURE RegisterErrorHook(err: OutHook) =
  BEGIN
    errHook := err;
  END RegisterErrorHook;

(* debugFilter *)

VAR
  debugFilter := GetVal("DEBUGFILTER");
  triggers: TextSet.T;
  streams     := DebugStreamList.List1(DebugStream.T { stderr }); 

  warnStreams : DebugStreamList.T := streams;
  (* ADDITIONAL warn Streams *)
  
  (* protected by mu *)
  mu := NEW(MUTEX);
  calls := 0;

CONST
  ResetInterval = 1000;

  DefaultDebugTimeZone = "GMT";

VAR
  DebugTimeZone := DefaultDebugTimeZone;

PROCEDURE SetDebugTimeZone( tzName : TEXT) RAISES { OSError.E } =
  BEGIN
    LOCK tMu DO
      DebugTimeZone := tzName;
      tz := TZ.New(tzName)
    END
  END SetDebugTimeZone;

PROCEDURE SetTimedDebugFlush(wr : Wr.T) =
  BEGIN
    StartFlusher();
    LOCK mu DO
      VAR p := streams; BEGIN
        WHILE p # NIL DO
          IF p.head.wr = wr THEN p.head.flushAlways := FALSE END;
          p := p.tail
        END
      END
    END
  END SetTimedDebugFlush;

VAR flusher : Thread.T := NIL;

PROCEDURE StartFlusher() =
  BEGIN
    IF flusher # NIL THEN RETURN END;

    flusher := Thread.Fork(NEW(Thread.Closure, apply := FlushApply));
  END StartFlusher;

PROCEDURE FlushApply(<*UNUSED*>cl : Thread.Closure) : REFANY =
  <*FATAL Thread.Alerted, Wr.Failure*>
  BEGIN
    LOOP
      Thread.Pause(0.1d0);
      LOCK mu DO
        VAR p := streams; BEGIN
          WHILE p # NIL DO
            IF NOT p.head.flushAlways THEN
              Wr.Flush(p.head.wr)
            END;
            p := p.tail
          END
        END
      END
    END
  END FlushApply;

TYPE
  OverrideClosure = Thread.Closure OBJECT
    fn : Pathname.T;
  OVERRIDES
    apply := OCApply;
  END;

PROCEDURE OCApply(cl : OverrideClosure) : REFANY =
  BEGIN
    LOOP
      Thread.Pause(1.0d0);
      OCIter(cl)
    END
  END OCApply;

PROCEDURE OCIter(cl : OverrideClosure) =
  VAR
    val : TEXT;
    newTab := NEW(TextTextTbl.Default).init();

  BEGIN
    TRY
      WITH rd     = FileRd.Open(cl.fn) DO
        LOOP
          WITH line = Rd.GetLine(rd),
               reader = NEW(TextReader.T).init(line),
               key    = reader.nextE(" ", skipNulls := TRUE) DO
            IF NOT reader.next(" ", val, skipNulls := TRUE) THEN
              val := ""
            END;
            EVAL newTab.put(key, val)
          END
        END
      END
    EXCEPT
      OSError.E => (* file not found *)
    |
      Rd.EndOfFile => (* done parsing, ok *)
    |
      Rd.Failure => (* who knows *)
    |
      Thread.Alerted => <*ASSERT FALSE*>
    |
      TextReader.NoMore => (* syntax error in file *)
    END;
    
    IF Different(newTab, valOverrides) THEN
      valOverrides := newTab;
      Reevaluate()
    END
  END OCIter;

PROCEDURE Different(t1, t2 : TextTextTbl.Default) : BOOLEAN =
  VAR
    var, val1, val2 : TEXT;
  BEGIN
    IF t1.size() # t2.size() THEN RETURN TRUE END;

    VAR
      iter := t1.iterate();
    BEGIN
      WHILE iter.next(var, val1) DO
        IF NOT t2.get(var, val2) THEN
          RETURN TRUE
        ELSIF (val1 = NIL OR val2 = NIL) THEN
          IF val1 # val2 THEN RETURN TRUE END
        ELSIF NOT Text.Equal(val1, val2) THEN
          RETURN TRUE
        END
      END
    END;
    RETURN FALSE
  END Different;

PROCEDURE Reevaluate() =
  BEGIN
    WITH str = GetVal("DEBUGTIMEZONE") DO
      IF str # NIL THEN
        DebugTimeZone := str;
        LOCK tMu DO
          tz := NIL
        END
      END
    END;
    
    VAR
      debugStr := GetVal("DEBUGLEVEL");
    BEGIN
      TRY
        IF debugStr # NIL THEN level := Scan.Int(debugStr) END
      EXCEPT
        Lex.Error, FloatMode.Trap => 
        Error("DEBUGLEVEL set to nonsense! \"" & debugStr & "\"",TRUE,DefaultErrorExitCode)
      END
    END;

    CallCallbacks()
  END Reevaluate;

VAR
  callbacks : RefList.T := NIL;
  cbMu := NEW(MUTEX);

TYPE CbRec = REF RECORD cb : SimpleCallback END;

PROCEDURE RegisterCallback(cb : SimpleCallback) =
  BEGIN
    LOCK cbMu DO
      callbacks := RefList.Cons(NEW(CbRec, cb := cb), callbacks)
    END
  END RegisterCallback;

PROCEDURE CallCallbacks() =
  BEGIN
    LOCK cbMu DO
      VAR p := callbacks;
      BEGIN
        WHILE p # NIL DO
          WITH cbRec = NARROW(p.head, CbRec) DO
            cbRec.cb()
          END;
          p := p.tail
        END
      END
    END
  END CallCallbacks;
  
BEGIN

  FOR i := FIRST(Options) TO LAST(Options) DO
    WITH optionEnvName = "DEBUGOPT_" & OptionNames[i] DO
      IF HaveEnv(optionEnvName) THEN
        options := options + SET OF Options { i }
      END
    END
  END;
  
  WITH str = RTParams.Value("debugoverrides") DO
    IF str # NIL THEN
      WITH cl = NEW(OverrideClosure, fn := str) DO
        OCIter(cl); (* get the table set up before proceeding with fork *)
        EVAL Thread.Fork(cl)
      END
    END
  END;

  WITH targetstring = RTParams.Value("debugtrace") DO
    IF targetstring # NIL THEN
      VAR 
        reader := NEW(TextReader.T).init(targetstring); 
        p := reader.shatter(",", endDelims := "");
      BEGIN
        WHILE p # NIL DO
          TRY
            AddStream(FileWr.Open(p.head))
          EXCEPT
            OSError.E(x) =>
            Error("Couldn't add file \"" & p.head & "\" to debug streams: OSError.E: " & AL.Format(x), exit := FALSE, exitCode := DefaultErrorExitCode)
          END;
          p := p.tail
        END
      END(*VAR BEGIN*)
    END(*IF*)
  END(*WITH*);

  Reevaluate();
  
  IF debugFilter # NIL THEN
    triggers := NEW(TextSetDef.T).init();
    VAR
      in: Rd.T;
      line: TEXT;
      <* FATAL Rd.Failure, Thread.Alerted *>
    BEGIN
      TRY
        in := FileRd.Open(debugFilter);
        LOOP
          line := Rd.GetLine(in);
          EVAL triggers.insert(line);
        END;
      EXCEPT
      | Rd.EndOfFile =>
      | OSError.E => Error("Can't find file \"" & debugFilter & "\".",TRUE,DefaultErrorExitCode);
      END
    END
  END

END Debug.
