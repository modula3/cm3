(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* File: ServerLog.mod,  server logging utilities *)
(* Last modified on Thu Feb  2 08:51:27 PST 1995 by kalsow *)
(*      modified on Fri Apr 29 12:55:34 PDT 1994 by wobber *)
(*      modified on Wed Dec 19 12:40:12 PST 1990 by swart *)
(*      modified on Tue Jun 19  8:13:53 PDT 1990 by mcjones *)
(*      modified on Thu Jan 21 14:26:28 1988 by mann *)

MODULE ServerLog;

IMPORT Fmt, Date, FmtTime, TextWr, Thread, Time, Wr;

TYPE
  Statistic = RECORD
    name:             TEXT;
    current, maximum: INTEGER;
    timedCount:       CARDINAL;
    elapsedTime:      Time.T;
  END;

  StatTable = REF ARRAY OF Statistic;

REVEAL
  T = Thread.Closure BRANDED OBJECT
    showMS:          BOOLEAN;
    startTime:       Time.T;
    lazyFlushThread: Thread.T := NIL;
    flusherPause:    Time.T;
    maxQueueLength: CARDINAL;

    mutex: MUTEX;
    cond:  Thread.Condition;
    wakeFlusher: Thread.Condition;
    wrBusy: BOOLEAN := FALSE;
    wr: Wr.T;       (* Can only be accessed with wrBusy TRUE *)
    lastTime: Date.T;
    stats: Stats := NIL;    (* a list of statistics objects *)

    (* protected by  *)
    qHead, qTail: QElem := NIL;
    queueLength:  CARDINAL := 0;
  OVERRIDES
    apply := LazyFlush;
  END;

REVEAL
  Stats = StatsPublic BRANDED OBJECT
    mu: MUTEX;
    next: Stats := NIL;
    tag: TEXT;
    tbl: StatTable;
  OVERRIDES
    incr := StatIncr;
    decr := StatDecr;
    time := StatTime;
  END;

TYPE
  QElem = REF RECORD
    time: Time.T;
    text: TEXT;
    (* The following entries are used when sl is on a queue. *)
    next: QElem := NIL;
    lostData: BOOLEAN := FALSE;
  END;

  TimerContext = Thread.Closure OBJECT
    t: T;
  OVERRIDES
    apply := Timer;
  END;
  
PROCEDURE Init(
  logWr:            Wr.T;
  lazyWriter:       BOOLEAN := FALSE;
  flusherPause:     Time.T := 0.0D0;
  showMilliseconds: BOOLEAN := FALSE;
  maxQueueLength:  CARDINAL := 0
  ): T =
  CONST
    DefaultFlusherPause    = 1.0D1;
    DefaultMaxQueueLength = 100;
  VAR
    t:      T;
  BEGIN
      (* note that logWr can be NIL *)
    IF flusherPause = 0.0D0 THEN flusherPause := DefaultFlusherPause; END;
    IF maxQueueLength = 0 THEN
      maxQueueLength := DefaultMaxQueueLength;
    END;
    t := NEW(T,
        mutex           := NEW(MUTEX),
        cond            := NEW(Thread.Condition),
        wakeFlusher     := NEW(Thread.Condition),
        startTime       := Time.Now(),
        showMS          := showMilliseconds,
        wr              := logWr);
    t.lastTime.year := 0;
      (* horrible hack !!!! *)
    IF logWr # NIL THEN
      (*
      TRY t.wrFile := FileStream.FileFromWr(logWr); EXCEPT
      | System.NarrowFault:
        t.wrFile := NIL;
      END;
      *)
      IF lazyWriter THEN
        t.maxQueueLength := maxQueueLength;
        t.flusherPause    := flusherPause;
        t.lazyFlushThread := Thread.Fork(t);
      END;
    END;
    RETURN t;
  END Init;

PROCEDURE ProcessText(t: T; text: TEXT; time: Time.T; flush: BOOLEAN) =
  VAR
    cal:  Date.T;
    fsync: BOOLEAN;
    <* FATAL Thread.Alerted *>
  BEGIN
    fsync := FALSE;
    (* WHILE t.parent # NIL DO t := t.parent; END; *)
    LOCK t.mutex DO
      WHILE t.wrBusy DO Thread.Wait(t.mutex, t.cond); END;
      t.wrBusy := TRUE;
    END;
    TRY
      TRY
        cal := Date.FromTime(time);
        IF (cal.year # t.lastTime.year) OR
           (cal.month # t.lastTime.month) OR (cal.day # t.lastTime.day)
        THEN
          Wr.PutText(t.wr, "## Date is " & FmtTime.DateLong(cal) & "\n");
          t.lastTime := cal;
        END;
        Wr.PutText(t.wr, Fmt.F("%02s:%02s:%02s",
              Fmt.Int(cal.hour), Fmt.Int(cal.minute),
              Fmt.Int(cal.second MOD 100)));
        IF t.showMS THEN
          Wr.PutText(t.wr, Fmt.F(".%03s",
            Fmt.Int(ROUND((time - FLOAT(TRUNC(time), LONGREAL)) * 1.0D3))));
        END;
        Wr.PutChar(t.wr, ' ');
        Wr.PutText(t.wr, text);
        IF flush THEN Wr.Flush(t.wr); END;
      EXCEPT
      | Wr.Failure =>
      END;
    FINALLY
      LOCK t.mutex DO t.wrBusy := FALSE; END;
      Thread.Signal(t.cond);
    END;
  END ProcessText;

PROCEDURE WriteText(t: T; text: TEXT) =
  VAR qElem: QElem;
  BEGIN
    (* WHILE t.parent # NIL DO t := t.parent; END; *)
    IF t.wr # NIL THEN
      IF t.lazyFlushThread = NIL THEN
        ProcessText(t, text, Time.Now(), TRUE);
      ELSE
          (* Queue the writer *)
        qElem := NEW(QElem, text := text, time := Time.Now());
        LOCK t.mutex DO
          IF t.queueLength > t.maxQueueLength THEN
            t.qTail.lostData := TRUE;
            RETURN;
          END;
          IF t.qHead = NIL THEN
            t.qHead  := qElem;
          ELSE
            t.qTail.next := qElem;
          END;
          t.qTail := qElem;
          INC(t.queueLength);
          IF (t.queueLength >= t.maxQueueLength DIV 2) OR
             ((Time.Now() - t.qHead.time) > t.flusherPause)
          THEN
            Thread.Signal(t.wakeFlusher);
          END;
        END;
      END;
    END;
  END WriteText;

PROCEDURE Timer(tc: TimerContext): REFANY =
  BEGIN
    LOOP
      Thread.Pause(tc.t.flusherPause);
      Thread.Signal(tc.t.wakeFlusher);
    END;
    <*NOWARN*> RETURN NIL;
  END Timer;

PROCEDURE LazyFlush(t: T) : REFANY =
  VAR tc := NEW(TimerContext, t := t);
  VAR qElem: QElem;
  BEGIN
    EVAL Thread.Fork(tc);
    LOOP
      LOCK t.mutex DO
        LOOP
          IF (t.qHead # NIL) AND
             ((t.queueLength >= t.maxQueueLength DIV 2) OR
              (Time.Now() - t.qHead.time > t.flusherPause))
          THEN
            EXIT;
          END;
          Thread.Wait(t.mutex, t.wakeFlusher);
        END;
        qElem := t.qHead;
        t.qHead  := NIL;
        t.qTail   := NIL;
        t.queueLength := 0;
      END;

      WHILE qElem # NIL DO
        ProcessText(t, qElem.text, qElem.time, qElem.next = NIL);
        IF qElem.lostData THEN
          ProcessText(t, "ServerLog: Lost writes!!!\n", Time.Now(), TRUE);
        END;
        qElem := qElem.next;
      END;
    END;
    <* NOWARN *> RETURN NIL;
  END LazyFlush;


(* procs for statistic collection *)

PROCEDURE NewStats(t: T; tag: TEXT; VAR statNames: ARRAY OF TEXT): Stats =
  VAR s := NEW(Stats, tag := tag, mu := NEW(MUTEX),
                          tbl := NEW(StatTable, NUMBER(statNames)));
  BEGIN
    FOR i := 0 TO NUMBER(statNames)-1 DO
      s.tbl[i] := Statistic{statNames[i], 0, 0, 0, 0.0D0};
    END;
    LOCK t.mutex DO s.next := t.stats; t.stats := s; END;
    RETURN s;
  END NewStats;

PROCEDURE DumpStats(t: T; resetTimes: BOOLEAN := TRUE) : TEXT =
  VAR
    wr: Wr.T;
    lastLineIndex, startIndex: CARDINAL;
    seconds, minutes, hours, days: CARDINAL;
    out: TEXT;
  CONST
    EndLineLimit = 60;
    minSec = 60; hrSec = minSec * 60; daySec = hrSec * 24;
    <* FATAL Wr.Failure *>
    <* FATAL Thread.Alerted *>
  BEGIN
    wr := TextWr.New();
    LOCK t.mutex DO
      VAR s := t.stats; BEGIN
        WHILE s # NIL DO
          LOCK s.mu DO
            seconds := ROUND(Time.Now() - t.startTime);
            days := seconds DIV daySec;
            DEC(seconds, (days * daySec));
            hours := seconds DIV hrSec;
            DEC(seconds, (hours * hrSec));
            minutes := seconds DIV minSec;
            Wr.PutText(wr,
              Fmt.F("  %s, up %s days, %s:%02s ...\n",
                  s.tag, Fmt.Int(days), Fmt.Int(hours), Fmt.Int(minutes)));
            lastLineIndex := Wr.Index(wr);
            Wr.PutText(wr, "    ");
            FOR i := 0 TO LAST(s.tbl^) DO
              startIndex := Wr.Index(wr);
              IF (startIndex - lastLineIndex) > EndLineLimit THEN
                Wr.PutText(wr, "\n    ");
                lastLineIndex := startIndex + 1;
              END;
              Wr.PutText(wr, " " & s.tbl[i].name & ": " &
                                 Fmt.Int(s.tbl[i].current));
              IF s.tbl[i].current # s.tbl[i].maximum THEN
                Wr.PutText(wr, "/" & Fmt.Int(s.tbl[i].maximum));
              END;
              IF s.tbl[i].timedCount # 0 THEN
                Wr.PutText(wr, ", " &
                     Fmt.Int(ROUND(s.tbl[i].elapsedTime * 1.0D3)
                                           DIV s.tbl[i].timedCount)
                     & "(" & Fmt.Int(s.tbl[i].timedCount) & ")");
                IF resetTimes THEN
                  s.tbl[i].timedCount := 0;
                  s.tbl[i].elapsedTime := 0.0D0;
                END;
              END;
              Wr.PutChar(wr, ';');
            END;
            Wr.PutChar(wr, '\n');
          END;
          s := s.next;
        END;
      END;
      out := TextWr.ToText(wr);
    END;
    RETURN out;
  END DumpStats;

PROCEDURE StatIncr(s: Stats; statNum: CARDINAL; n: CARDINAL := 1) =
  BEGIN
    LOCK s.mu DO
      INC(s.tbl[statNum].current, n);
      IF s.tbl[statNum].current > s.tbl[statNum].maximum THEN
        s.tbl[statNum].maximum := s.tbl[statNum].current;
      END;
    END;
  END StatIncr;

PROCEDURE StatDecr(s: Stats; statNum: CARDINAL; n: CARDINAL := 1) =
  BEGIN
    LOCK s.mu DO
      DEC(s.tbl[statNum].current, n);
    END;
  END StatDecr;

PROCEDURE StatTime(s: Stats; statNum: CARDINAL; start: Time.T) =
  VAR
    delta := Time.Now() - start;
  BEGIN
    IF delta < 0.0D0 THEN RETURN END;
    LOCK s.mu DO
      INC(s.tbl[statNum].timedCount);
      s.tbl[statNum].elapsedTime := s.tbl[statNum].elapsedTime + delta;
    END;
  END StatTime;

BEGIN
END ServerLog.


