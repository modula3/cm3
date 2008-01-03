(* Copyright (C) 1989, 1993 Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Tue Mar 21 09:45:17 PST 1995 by msm      *)
(*      modified on Tue Jan 31 11:02:28 PST 1995 by kalsow   *)
(*      modified on Mon Oct 25 12:33:51 PDT 1993 by sfreeman *)

UNSAFE MODULE JVDecomp;

IMPORT AtomList, Atom, JVBuffer, JVConverter, JVConverterF, JVFromSource,
       JVFromDecomp, Jvs, OSError, Point, Thread, Tick, Word;

REVEAL
  T = Public BRANDED OBJECT
        in        : JVConverter.T;
        maxBuffers: CARDINAL;
        server    : Jvs.T;
        dparams                        := Jvs.DefaultDecompress;
        cmap      : Jvs.ColormapInfo;
        thread    : Thread.T           := NIL;
        closure   : Closure            := NIL;
      OVERRIDES
        init := Init;

        start := Start;
        stop  := Stop;

        outSize  := OutSize;
        getInput := GetInput;

        startStats := StartStats;
        close      := Close;
      END;

(* {{{ -- methods -- *)

PROCEDURE Init (         t         : T;
                         in        : JVConverter.T;
                READONLY dparams   : Jvs.DcmpParams;
                READONLY cmap      : Jvs.ColormapInfo;
                         maxBuffers: CARDINAL           := 2;
                         factory   : JVBuffer.Factory   := NIL;
                         server    : Jvs.T              := NIL  ): T
  RAISES {OSError.E, Thread.Alerted} =
  VAR newFac := factory;
  BEGIN
    TRY
      LOCK t DO
        EVAL JVConverter.T.init(t);
        t.maxBuffers := maxBuffers;
        t.in := in;
        t.dparams := dparams;
        t.cmap := cmap;
        t.closure := NEW(Closure, t := t);

        (* create new output buffer pool.  It will be cleaned out when
           collected *)
        IF server = NIL THEN
          t.server := NEW(Jvs.T).init();
        ELSE
          t.server := server;
        END;
        IF newFac = NIL THEN
          newFac := NEW(JVFromDecomp.Factory).init(t.server);
        END;
        t.output := NEW(JVBuffer.Pool).init(newFac, maxBuffers);

        (* get real width and height for image *)
        EVAL t.server.colormap(t.cmap);
        EVAL t.server.setDecompress(t.dparams);

      END;
    EXCEPT
    | OSError.E (e) => RAISE OSError.E(AtomList.Cons(decompError, e));
    END;
    RETURN t;
  END Init;

PROCEDURE Start (t: T) RAISES {JVConverter.Error, Thread.Alerted} =
  BEGIN
    <* ASSERT t.output # NIL *>
    TRY
      t.output.setSize(t.maxBuffers);
      t.in.join();
      t.thread := Thread.Fork(t.closure);
    EXCEPT
    | OSError.E (e) =>
        RAISE JVConverter.Error(
                AtomList.Cons(Atom.FromText("JVDecomp.Start"), e));
    END;
  END Start;

PROCEDURE Stop (t: T) RAISES {JVConverter.Error, Thread.Alerted} =
  BEGIN
    TRY
      t.output.setSize(0);
    EXCEPT
    | OSError.E (al) =>
        RAISE JVConverter.Error(
                AtomList.Cons(Atom.FromText("JVDecomp.Stop"), al));
    END;
    InnerClose(t);
  END Stop;

PROCEDURE OutSize (t: T): Point.T =
  BEGIN
    RETURN Point.T{t.dparams.outX, t.dparams.outY};
  END OutSize;

PROCEDURE GetInput (t: T): JVConverter.T =
  BEGIN
    RETURN t.in;
  END GetInput;

PROCEDURE Close (t: T) =
  BEGIN
    LOCK t DO
      TRY
        InnerClose(t);
      EXCEPT
        JVConverter.Error, Thread.Alerted => (*skip *)
      END;
      t.output.signalClosed();
      t.output := NIL;
      t.server.close();
    END;
  END Close;

PROCEDURE InnerClose (t: T) RAISES {JVConverter.Error, Thread.Alerted} =
  (* LL >= t *)
  VAR
    res   : REFANY;
    thread         := t.thread;
  BEGIN
    IF thread # NIL THEN
      Thread.Alert(thread);

      Thread.Release(t);
      res := Thread.Join(thread);
      Thread.Acquire(t);
      t.thread := NIL;

      t.in.leave();
      (* t.in := NIL; *)

      IF res # NIL AND ISTYPE(res, AtomList.T) THEN
        RAISE
          JVConverter.Error(AtomList.Cons(Atom.FromText("JVDecomp.Stop"),
                                          NARROW(res, AtomList.T)));
      END;
    END;
  END InnerClose;

PROCEDURE StartStats (t: T) =
  BEGIN
    LOCK t DO
      IF t.statistics = NIL THEN t.statistics := NEW(Statistics); END;
      WITH s = NARROW(t.statistics, Statistics) DO
        s.framesStarted := 0;
        s.framesProcessed := 0;
        s.timesBlocked := 0;
        s.cumLatency := 0;
      END;
    END;
  END StartStats;

(* }}} *)
(* {{{ -- decompress thread -- *)

TYPE Closure = Thread.Closure OBJECT t: T;  OVERRIDES apply := Apply; END;

PROCEDURE Apply (cl: Closure): REFANY =
  VAR
    inbuf : JVFromSource.T := NIL;
    outbuf: JVFromDecomp.T := NIL;
    t                      := cl.t;
    source: JVBuffer.Pool  := NIL;
    errors: AtomList.T     := NIL;
    paused                 := FALSE;
    tick  : Tick.T;

  PROCEDURE ProcessFrame () RAISES {OSError.E, Thread.Alerted} =
    (* decompress the frame, update the statistics, and put the frame on
       the output queue *)
    VAR
      w1: UNTRACED REF INTEGER := outbuf.addr + 3 * ADRSIZE(INTEGER);
      w2: UNTRACED REF INTEGER := outbuf.addr + 4 * ADRSIZE(INTEGER);
    BEGIN
      TRY
        w1^ := 16_DEAD;
        w2^ := 16_BEEF;
	TRY
          t.server.decompress(inbuf.shmid, outbuf.shmid, inbuf.frameLength);
          IF w1^ = 16_DEAD AND w2^ = 16_BEEF THEN RETURN END
	EXCEPT
	  OSError.E(e) => 
	    IF DecompressError(e) THEN RETURN END;
	    RAISE OSError.E (e)
	END;
        outbuf.params := t.dparams;
        outbuf.cmap := t.cmap;
        outbuf.serial := inbuf.serial;
        outbuf.timestamp := inbuf.timestamp;
        outbuf.ready := inbuf.ready; (* pass ready closure on to clients of
                                        decompressed buffer *)
        outbuf.localTime := Tick.Now();
        inbuf.ready := NIL;

        tick := inbuf.localTime;
        inbuf.free();
        inbuf := NIL;
        t.output.insert(outbuf);
        IF t.statistics # NIL THEN
          WITH s = NARROW(t.statistics, Statistics) DO
            INC(s.framesProcessed);
            s.cumLatency :=
              Word.Plus(s.cumLatency, Word.Minus(outbuf.localTime, tick));
          END;
        END;

        outbuf := NIL;
      FINALLY
        IF inbuf # NIL THEN inbuf.free(); inbuf := NIL END;
        IF outbuf # NIL THEN outbuf.free(); outbuf := NIL END;
      END;
    END ProcessFrame;

  BEGIN
    TRY
      source := t.in.getOutput();
      <* ASSERT source # NIL *>
      source.join();
      LOOP
        IF inbuf # NIL THEN inbuf.free(); inbuf := NIL END;
        IF t.paused >= t.clients THEN
          t.in.setPaused(TRUE);
          paused := TRUE;

          LOCK t DO
            WHILE t.paused >= t.clients DO
              Thread.AlertWait(t, t.pauseEvent);
            END;
          END;
          t.in.setPaused(FALSE);
          paused := FALSE;
        END;

        inbuf := NARROW(source.waitForChange(), JVFromSource.T);
        outbuf := NARROW(t.output.getFreeBuffer(FALSE), JVFromDecomp.T);
        IF outbuf = NIL THEN
          LOCK t DO
            IF t.statistics # NIL THEN
              INC(NARROW(t.statistics, Statistics).timesBlocked);
            END;
          END;
          paused := TRUE;
          t.in.setPaused(TRUE);
          outbuf := NARROW(t.output.getFreeBuffer(TRUE), JVFromDecomp.T);
          t.in.setPaused(FALSE);
          paused := FALSE;
        END;


        LOCK t DO
          IF t.statistics # NIL THEN
            INC(NARROW(t.statistics, Statistics).framesStarted);
          END;

          IF inbuf.info.width > 0 AND inbuf.info.height > 0 THEN
            (* jvserver complains if width, height <= 0 *)
            t.dparams.qfactor := inbuf.info.qfactor;
            t.dparams.inX := inbuf.info.width;
            t.dparams.inY := inbuf.info.height;

            EVAL
              t.server.setDecompress(t.dparams); (* no-op if no change *)
            ProcessFrame();
            IF outbuf # NIL THEN outbuf.free() END
          END;
        END;
      END;                       (* loop *)
    EXCEPT
    | OSError.E (e) => errors := e;
    | JVBuffer.Closed, Thread.Alerted => (* skip *)
    END;
    IF paused THEN t.in.setPaused(FALSE); END;
    IF inbuf # NIL THEN inbuf.free(); END;
    IF outbuf # NIL THEN outbuf.free(); END;
    IF source # NIL THEN source.leave(); END;

    RETURN errors;
  END Apply;

PROCEDURE DecompressError (e: AtomList.T): BOOLEAN =
  (* return TRUE if "e" contains Jvs.DecompressFailure *)
  VAR curr := e;
  BEGIN
    WHILE curr # NIL DO
      IF curr.head = Jvs.DecompressFailure THEN RETURN TRUE; END;
      curr := curr.tail;
    END;
    RETURN FALSE;
  END DecompressError;

(* }}} *)


BEGIN
  decompError := Atom.FromText("JVDecomp: ");

END JVDecomp.
