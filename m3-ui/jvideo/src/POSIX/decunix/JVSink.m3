(* Copyright (C) 1989, 1993 Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Thu Mar 23 10:17:46 PST 1995 by msm      *)
(*      modified on Sun Oct 30 14:33:04 PST 1994 by kalsow   *)
(*      modified on Wed Oct 27 14:51:41 PDT 1993 by sfreeman *)

UNSAFE MODULE JVSink;

IMPORT Atom, AtomList, Ctypes, ConnFD, IP, JVBuffer, JvsBuffer, JVConverter,
       JVConverterF, JVFromSource, jvprotocol, OSError, OSErrorPosix,
       Process, Rd, RdUtils, TCP, TCPPosix, Thread, Tick, Uerror,
       Uin (*, Unix *), Usocket, WeakRef, Wr;

(**IMPORT Stdio;**)

FROM Ctypes IMPORT int;
FROM Utypes IMPORT u_int;

CONST RetrySeconds = 30.0d0;

REVEAL
  T = Public BRANDED OBJECT
        sourceHost           : TEXT;
        info                 : JVFromSource.StreamInfo;
        quality              : jvprotocol.ClientQualityVal;
        tcp                  : TCP.T                         := NIL;
        maxBuffers           : CARDINAL                      := 0;
        readThread, prodThread: Thread.T                     := NIL;
        readClosure          : Closure                       := NIL;
        prodClosure          : ProdClosure                   := NIL;
        readSerial           : JVBuffer.Serial;
        sync                 : CARDINAL                      := 0;
        delay                : CARDINAL                      := 0;

        retryThread : Thread.T := NIL;
        retryClosure: Closure  := NIL;
        retrying               := FALSE;
      OVERRIDES
        init  := Init;
        leave := Leave;

        start := Start;
        stop  := Stop;

        getInfo := GetInfo;
        close   := Close;

        startStats := StartStats;
      END;

(* {{{ -- methods -- *)

PROCEDURE signed_ntohl(x: int): int =
  BEGIN
    RETURN LOOPHOLE(Uin.ntohl(LOOPHOLE(x, u_int)), int)
  END signed_ntohl;

PROCEDURE signed_htonl(x: int): int =
  BEGIN
    RETURN LOOPHOLE(Uin.htonl(LOOPHOLE(x, u_int)), int)
  END signed_htonl;

PROCEDURE Init (t         : T;
                hostname  : TEXT;
                quality   : Quality            := DefaultQuality;
                maxBuffers: CARDINAL           := 2;
                factory   : JVBuffer.Factory;
                delay     : CARDINAL                              ): T
  RAISES {OSError.E, Thread.Alerted} =
  BEGIN
    TRY
      EVAL JVConverter.T.init(t);
      t.sourceHost := hostname;
      t.maxBuffers := maxBuffers;
      t.quality := quality + FIRST(jvprotocol.ClientQualityVal);
      t.output := NEW(JVBuffer.Pool).init(factory, t.maxBuffers);
      t.delay := delay;
      OpenRemoteConnection(t);
    EXCEPT
    | OSError.E (e) => RAISE OSError.E(AtomList.Cons(sinkError, e));
    END;
    RETURN t;
  END Init;

PROCEDURE Leave (t:T) RAISES {Thread.Alerted, JVConverter.Error} =
  VAR th := t.readThread;
  BEGIN
  (* Let's not do this, and see if anyone notices *)
    IF th # NIL AND FALSE THEN
      Thread.Alert(th)
    END;
    Public.leave(t)
  END Leave;

PROCEDURE Start (t: T) RAISES {JVConverter.Error, Thread.Alerted} =
  BEGIN
    <* ASSERT t.output # NIL *>
    TRY
      t.output.setSize(t.maxBuffers);
      Open(t);
    EXCEPT
    | OSError.E (e) =>
        RAISE JVConverter.Error(
                AtomList.Cons(Atom.FromText("JVSink.Start"), e));
    END;
  END Start;

PROCEDURE Stop (t: T) RAISES {JVConverter.Error, Thread.Alerted} =
  BEGIN
    TRY
      t.output.setSize(0);
    EXCEPT
    | OSError.E (al) =>
        RAISE JVConverter.Error(
                AtomList.Cons(Atom.FromText("JVSink.Stop"), al));
    END;
    InnerClose(t, TRUE);
  END Stop;

PROCEDURE GetInfo (t: T; VAR info: JVFromSource.StreamInfo): BOOLEAN =
  BEGIN
    LOCK t DO
      WITH res = info.serial < t.info.serial DO
        info := t.info;
        RETURN res;
      END;
    END;
  END GetInfo;

PROCEDURE Close (t: T) =
  BEGIN
    LOCK t DO
      InnerClose(t, TRUE);
      t.output.signalClosed();
      t.output := NIL;
    END;
  END Close;

(* {{{ -- internal procedures *)

PROCEDURE InnerClose (t: T; noRetry := FALSE) =
  (* unlocked implementation of Close() *)
  BEGIN
    IF NOT t.retrying OR noRetry THEN
      VAR thread := t.retryThread; BEGIN
        IF thread # NIL THEN
          Thread.Alert(thread);
          Thread.Release(t);
          EVAL Thread.Join(thread); (* don't do anything until the thread
                                       has finished *)
          Thread.Acquire(t)
        END;
      END;
    END;

    VAR thread := t.readThread; BEGIN
      IF thread # NIL THEN
        Thread.Alert(thread);
        Thread.Release(t);
        EVAL Thread.Join(thread); (* ditto *)
        Thread.Acquire(t);
      END;
    END;

    IF t.tcp # NIL THEN t.tcp.close() END;
    t.tcp := NIL;
  END InnerClose;

PROCEDURE StartStats (t: T) =
  BEGIN
    LOCK t DO
      IF t.statistics = NIL THEN
        t.statistics := NEW(JVConverter.Statistics);
      END;
      t.statistics.framesStarted := 0;
      t.statistics.framesProcessed := 0;
      t.statistics.timesBlocked := 0;
    END;
  END StartStats;

(* }}} *)

(* }}} *)
(* {{{ -- read thread -- *)

TYPE
  Closure = Thread.Closure OBJECT t: T;  OVERRIDES apply := ReadApply; END;
  ProdClosure = Thread.Closure OBJECT t: T;  OVERRIDES apply := ProdApply; END;

CONST AnySize = BYTESIZE(jvprotocol.AnyHeader);

PROCEDURE ProdApply (cl: ProdClosure): REFANY =
  VAR t := cl.t;
  BEGIN
    TRY
      LOOP
        Thread.Pause(3.0D0);
        LOCK t DO
          IF t.readThread = NIL THEN EXIT END;
          SendToSource(t.tcp, FIRST(jvprotocol.ClientNumCredits));
        END
      END
    EXCEPT
      Wr.Failure, Thread.Alerted =>
    END;
    LOCK t DO t.prodThread := NIL END;
    RETURN NIL
  END ProdApply;

PROCEDURE ReadApply (cl: Closure): REFANY =

  (* control data from the server currently is thrown away *)
  PROCEDURE GetControl (netlength: Ctypes.int)
    RAISES {Rd.Failure, Thread.Alerted} =
    VAR length := signed_ntohl(netlength);
    BEGIN
      IF length > jvprotocol.MaxControlMsgSize THEN
        RAISE Rd.Failure(NIL);
      END;
      GetMinBytes(t, t.tcp, SUBARRAY(control, 0, length));
    END GetControl;

  VAR
    t                         := cl.t;
    buffer: jvprotocol.Header;
    any := LOOPHOLE(ADR(buffer[0]), jvprotocol.AnyHeaderPtr);
    hdr := LOOPHOLE(ADR(buffer[0]), jvprotocol.VideoFramePtr);
    control: jvprotocol.ControlBuffer;
    jvbuff : JVFromSource.T           := NIL;
    length : CARDINAL;
    retryp : BOOLEAN;
    failure: AtomList.T;
  BEGIN
    TRY
      retryp := FALSE;
      failure := NIL;
      LOOP

        IF t.paused >= t.clients THEN
          (* don't start reading until we have a buffer in which to put the
             frame data *)
          LOCK t DO
            SendToSource(t.tcp, jvprotocol.ClientSuspendStream);
            WHILE t.paused >= t.clients DO
              Thread.AlertWait(t, t.pauseEvent);
            END;
            SendToSource(t.tcp, jvprotocol.ClientResumeStream);
          END;
        END;

        LOCK t DO GetHeader(t, t.tcp, buffer); END;

        IF any.type = jvprotocol.JVP_VIDEO THEN
          VAR subtype := JvsBuffer.Subtype2(signed_ntohl(hdr.length));
          BEGIN
            IF jvbuff # NIL AND jvbuff.subtype # subtype THEN
              jvbuff.free();
              jvbuff := NIL
            END;
            IF jvbuff = NIL THEN
              jvbuff := NARROW(t.output.getFreeBuffer(FALSE, subtype),
                               JVFromSource.T)
            END;
            IF jvbuff = NIL THEN
              LOCK t DO
                IF t.statistics # NIL THEN
                  INC(t.statistics.timesBlocked);
                END;
                SendToSource(t.tcp, jvprotocol.ClientSuspendStream);
              END;
              jvbuff := NARROW(t.output.getFreeBuffer(TRUE, subtype),
                               JVFromSource.T);
              LOCK t DO
                IF t.statistics # NIL THEN
                  INC(t.statistics.timesBlocked);
                END;
                SendToSource(t.tcp, jvprotocol.ClientResumeStream);
              END
            END
          END
        END;

        LOCK t DO
          (* {{{ -- dispatch header *)

          CASE any.type OF
          | jvprotocol.JVP_VIDEO =>
              IF t.statistics # NIL THEN
                INC(t.statistics.framesStarted);
              END;
              GetFrame(t, buffer, jvbuff);

              jvbuff.info := t.info;
              IF jvbuff.info.serial MOD 8 = 0 THEN
                jvbuff.ready.endToEnd := TRUE
              END;
              jvbuff.ready := NewReady(t.tcp);
              t.output.insert(jvbuff);
              jvbuff := NIL;
              IF t.statistics # NIL THEN
                INC(t.statistics.framesProcessed);
              END;

          | jvprotocol.JVP_RESOLUTION => (* skip *)
          | jvprotocol.JVP_TYPE =>
              WITH type = LOOPHOLE(any, jvprotocol.VideoTypePtr),
                   si   = t.info                                  DO
                INC(si.serial);
                si.qfactor := signed_ntohl(type.qfactor);
                si.width := signed_ntohl(type.width);
                si.height := signed_ntohl(type.height);
              END;
          | jvprotocol.JVP_SYNC =>
              WITH sync = LOOPHOLE(any, jvprotocol.SyncFramePtr) DO
                (* if this is the reply we're waiting for, send credits to
                   restart stream *)
                IF t.sync = signed_ntohl(sync.code) THEN t.sync := 0; END;
              END;
          | jvprotocol.JVP_QUALITIES =>
              length := LOOPHOLE(any, jvprotocol.QualitiesFramePtr).length;
              GetControl(signed_ntohl(length));
          | jvprotocol.JVP_ERROR =>
              length := LOOPHOLE(any, jvprotocol.ErrorFramePtr).length;
              GetControl(signed_ntohl(length));
          | jvprotocol.JVP_INFO =>
              length := LOOPHOLE(any, jvprotocol.InfoFramePtr).length;
              GetControl(signed_ntohl(length));
          | jvprotocol.JVP_VIDEO_UDP_REQUEST,
              jvprotocol.JVP_VIDEO_UDP_RESPONSE =>
          | jvprotocol.JVP_AUDIO, jvprotocol.JVP_STATUS,
              jvprotocol.JVP_ENDMARK, jvprotocol.JVP_AUDIO_SILENCE,
              jvprotocol.JVP_AUDIO_MULTICAST,
              jvprotocol.JVP_VIDEO_UDP_DATA =>
              RAISE Rd.Failure(NIL);
          ELSE
            RAISE Rd.Failure(NIL);
          END;

          (* }}} *)
        END;
      END;
    EXCEPT
    | Thread.Alerted =>          (* skip *)
    | Rd.Failure (f) => failure := f; retryp := TRUE;
    | Wr.Failure (f) => failure := f; retryp := TRUE;
    | OSError.E (f) => failure := f; retryp := TRUE;
    END;

    IF jvbuff # NIL THEN jvbuff.free(); END;
    IF failure # NIL THEN
      WITH txt = RdUtils.FailureText(failure) DO
        IF txt # NIL THEN
          JVConverter.ReportError(
            "JVSink, read from " & t.sourceHost & txt);
        END;
      END;
    END;
    LOCK t DO t.readThread := NIL; IF retryp THEN Retry(t); END; END;
    RETURN NIL;
  END ReadApply;

(* }}} *)
(* {{{ -- procedures to read from server -- *)

PROCEDURE GetMinBytes (o: T; t: TCP.T; VAR buff: ARRAY OF CHAR)
  RAISES {Rd.Failure, Thread.Alerted} =
  (* loop until "buff" has been filled *)
  VAR
    len       := NUMBER(buff);
    charsRead := 0;
  BEGIN
    TRY
      Thread.Release(o);
      TRY
        WHILE len > 0 DO
          WITH res = t.get(SUBARRAY(buff, charsRead, len)) DO
            IF res = 0 THEN RAISE ConnFD.TimedOut END;
            INC(charsRead, res);
            DEC(len, res);
          END;
        END;
      EXCEPT
      | ConnFD.TimedOut => RAISE Rd.Failure(NIL);
      END;
    FINALLY
      Thread.Acquire(o)
    END
  END GetMinBytes;

PROCEDURE GetHeader (o: T; t: TCP.T; VAR buffer: jvprotocol.Header)
  RAISES {Rd.Failure, Thread.Alerted} =
  VAR
    any := LOOPHOLE(ADR(buffer[0]), jvprotocol.AnyHeaderPtr);
    bytesLeft: CARDINAL;
  BEGIN
    GetMinBytes(o, t, SUBARRAY(buffer, 0, AnySize));
    any.type := signed_ntohl(any.type);
    CASE any.type OF
    | jvprotocol.JVP_FirstEvent .. jvprotocol.JVP_LastEvent =>
        bytesLeft := jvprotocol.HdrSizes[any.type] - AnySize;
        IF bytesLeft <= 0 THEN RETURN; END;
        GetMinBytes(o, t, SUBARRAY(buffer, AnySize, bytesLeft));
    ELSE
      RAISE Rd.Failure(AtomList.List1(Atom.FromText("BadHeaderType")));
    END;
  END GetHeader;

TYPE
  OpenArray = RECORD
                address: ADDRESS;
                count  : INTEGER;
              END;
  OpenArrayPtr = UNTRACED REF ARRAY OF CHAR;
(* this horrible hack allows us to build an open array from an address and
   length.  We can't use the trick with the very long fixed array because
   the NIL check in the subarray fails for shared memory segments *)

(***
PROCEDURE Bpt(s: TEXT := "") =
  <*FATAL ANY*>
  BEGIN
    Wr.PutText(Stdio.stderr, "Bpt: " & s & "\n");
  END Bpt;
***)

PROCEDURE GetFrame (         t     : T;
                    READONLY buffer: jvprotocol.Header;
                             jvb   : JVBuffer.T         )
  RAISES {Rd.Failure, Thread.Alerted} =
  VAR
    hdr            := LOOPHOLE(ADR(buffer[0]), jvprotocol.VideoFramePtr);
    oa : OpenArray;
    oap: OpenArrayPtr := ADR(oa);
  BEGIN
    <* ASSERT jvb # NIL *>
    jvb.serial := t.readSerial;
    INC(t.readSerial);
    jvb.timestamp.tv_sec := signed_ntohl(hdr.timestamp.tv_sec);
    jvb.timestamp.tv_usec := signed_ntohl(hdr.timestamp.tv_usec);
    jvb.frameLength := signed_ntohl(hdr.length);
    jvb.localTime := Tick.Now();

    <* ASSERT jvb.frameLength <= jvb.length *>
    oa.address := jvb.addr;
    oa.count := jvb.frameLength;
    GetMinBytes(t, t.tcp, oap^);
  END GetFrame;

(* }}} *)
(* {{{ -- retry thread -- *)

(* LL >= {t} *)
PROCEDURE Retry (t: T) =
  BEGIN
    IF t.retryThread # NIL THEN Thread.Alert(t.retryThread); END;
    IF t.retryClosure = NIL THEN
      t.retryClosure := NEW(Closure, t := t, apply := RetryApply);
    END;
    t.retrying := TRUE;
    t.retryThread := Thread.Fork(t.retryClosure);
  END Retry;

PROCEDURE RetryApply (cl: Closure): REFANY =
  VAR t := cl.t;
  BEGIN
    TRY
      LOOP
        TRY
          JVConverter.ReportError("JVSink, retrying " & t.sourceHost);
          LOCK t DO InnerClose(t); Open(t); END;
          EXIT;                  (* open succeeded *)
        EXCEPT
        | OSError.E =>           (* skip and loop again *)
        END;
        Thread.AlertPause(RetrySeconds);
      END;
    EXCEPT
    | Thread.Alerted =>          (*skip*)
    END;
    LOCK t DO t.retryThread := NIL; t.retrying := FALSE; END;
    RETURN NIL;
  END RetryApply;

(* }}} *)
(* {{{ -- internal procedures -- *)

PROCEDURE Open (t: T) RAISES {OSError.E, Thread.Alerted} =
  (* establish a connection with the server and start a thread to get
     frames and other info from it *)
  BEGIN
    TRY
      IF NOT t.retrying THEN
        IF t.retryThread # NIL THEN Thread.Alert(t.retryThread); END;
      END;

      OpenRemoteConnection(t);
      (* ask for correct quality *)
      SendToSource(t.tcp, t.quality);

      (* ask for frame rate *)
      IF t.delay # 0 THEN
        SendToSource(t.tcp, jvprotocol.ExtendedCommand);
        SendFrameRateToSource(t.tcp, jvprotocol.FrameRateHintRec{
                                       jvprotocol.FrameRateHint, t.delay})
      END;

      SendToSource(t.tcp, FIRST(jvprotocol.ClientNumCredits));
      SendToSource(t.tcp, FIRST(jvprotocol.ClientNumCredits));

      t.readSerial := 0;
      t.info.serial := 0;
      IF t.readThread = NIL THEN
        IF t.readClosure = NIL THEN
          t.readClosure := NEW(Closure, t := t);
        END;
        t.readThread := Thread.Fork(t.readClosure);
      END;
      IF FALSE AND t.prodThread = NIL THEN
        IF t.prodClosure = NIL THEN
          t.prodClosure := NEW(ProdClosure, t := t);
        END;
        t.prodThread := Thread.Fork(t.prodClosure);
      END;
    EXCEPT
    | Wr.Failure (f) =>
        RAISE OSError.E(AtomList.Cons(Atom.FromText("JVSink.Open"), f));
    END;
  END Open;


CONST TCP_NODELAY = 1;

PROCEDURE OpenRemoteConnection (t: T) RAISES {OSError.E, Thread.Alerted} =
  VAR
    endpoint: IP.Endpoint;
    arg     : Ctypes.int := 1;
  BEGIN
    IF t.tcp # NIL THEN RETURN; (*already open *) END;
    TRY
      IF NOT IP.GetHostByName(t.sourceHost, endpoint.addr) THEN
        RAISE OSError.E(AtomList.Cons(hostNotFound, NIL));
      END;
      (* endpoint.port := jvprotocol.PORT_VIDEO; the proper version *)
      endpoint.port := Uin.ntohs(jvprotocol.PORT_VIDEO);
      (* BOTCH, do this conversion for now because argo specifies its port
         in host byte order *)
      t.tcp := TCP.Connect(endpoint);

      WITH tcp = t.tcp DO
        LOCK tcp DO
          IF tcp.closed THEN
            RAISE IP.Error(AtomList.Cons(TCP.Closed, NIL));
          END;
          IF (* Unix.ioctl(tcp.fd, Unix.FIONBIO, ADR(arg)) < 0
               OR *) Usocket.setsockopt(tcp.fd, Uin.IPPROTO_TCP, TCP_NODELAY,
                                     ADR(arg), BYTESIZE(arg)) < 0
               OR Usocket.setsockopt(
                    tcp.fd, Usocket.SOL_SOCKET, Usocket.SO_KEEPALIVE,
                    ADR(arg), BYTESIZE(arg)) < 0 THEN
            Crash(Uerror.errno);
          END;

          (* set rcv buffer as large as possible *)
          arg := 64 * 1024 - 1;
          WHILE Usocket.setsockopt(
                  tcp.fd, Usocket.SOL_SOCKET, Usocket.SO_RCVBUF, ADR(arg),
                  BYTESIZE(arg)) < 0 DO
            IF Uerror.errno = Uerror.ENOBUFS THEN
              DEC(arg, 1024);
            ELSE
              Crash(Uerror.errno);
            END;
          END;
        END;
      END;

    EXCEPT
    | IP.Error (ec) => RAISE OSError.E(AtomList.Cons(sinkError, ec));
    | OSError.E (e) => RAISE OSError.E(AtomList.Cons(sinkError, e));
    END;
  END OpenRemoteConnection;

PROCEDURE SendToSource (t: TCP.T; msg: jvprotocol.ClientRequest)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR data := LOOPHOLE(ADR(msg), UNTRACED REF ARRAY [0 .. 0] OF CHAR);
  BEGIN
    t.put(data^);
  END SendToSource;

PROCEDURE SendFrameRateToSource (         t  : TCP.T;
                                 READONLY msg: jvprotocol.FrameRateHintRec)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    nmsg := msg;
    data := LOOPHOLE(ADR(nmsg), UNTRACED REF ARRAY [0 .. 7] OF CHAR);
  BEGIN
    nmsg.type := signed_htonl(nmsg.type);
    nmsg.period := signed_htonl(nmsg.period);
    t.put(data^);
  END SendFrameRateToSource;

PROCEDURE Crash (e: INTEGER) =
  BEGIN
    Process.Crash(
      "Emergency stop: " & Atom.ToText(OSErrorPosix.ErrnoAtom(e)));
  END Crash;

(* }}} *)
(* {{{ -- ready closure -- *)

(* this ready closure tries to send a credit back to the source if this is
   the first time the "apply" method has been called.  It is supposed to be
   called just after the "Picture.Paint" call for this buffer. *)

TYPE
  ReadyClosure = JVBuffer.ReadyClosure OBJECT
                   next  : ReadyClosure;  (* for free list *)
                   tcp   : TCP.T;
                   called: BOOLEAN;       (* has apply been called yet? *)
                   tcpOpen: BOOLEAN;  (* false if a call to this tcp has
                                         failed *)
                 OVERRIDES
                   apply := ReadyApply;
                 END;

PROCEDURE ReadyApply (cl: ReadyClosure) RAISES {Thread.Alerted} =
  BEGIN
    IF cl.tcpOpen AND NOT cl.called THEN (* unlocked quick check first *)
      LOCK cl DO
        IF cl.tcp # NIL AND cl.tcpOpen AND cl.endToEnd AND NOT cl.called THEN
          cl.called := TRUE;
          TRY
            SendToSource(cl.tcp, FIRST(jvprotocol.ClientNumCredits));
          EXCEPT
          | Wr.Failure => cl.tcpOpen := FALSE;
          END;
        END;
      END;
    END;
  END ReadyApply;


PROCEDURE NewReady (tcp: TCP.T): ReadyClosure =
  (* use this proc to get a new ReadyClosure, does some free list stuff *)
  VAR res: ReadyClosure := NIL;
  BEGIN
    LOCK readyMutex DO
      IF readyFrees # NIL THEN
        res := readyFrees;
        readyFrees := readyFrees.next;
      END;
    END;
    IF res = NIL THEN
      res := NEW(ReadyClosure);
      EVAL WeakRef.FromRef(res, CleanUpReady);
    END;
    res.next := NIL;
    res.tcp := tcp;
    res.called := FALSE;
    res.endToEnd := FALSE;
    res.tcpOpen := TRUE;
    RETURN res;
  END NewReady;

PROCEDURE CleanUpReady (<* UNUSED*> READONLY w: WeakRef.T; r: REFANY) =
  (* return to ready free list *)
  BEGIN
    WITH rc = NARROW(r, ReadyClosure) DO
      LOCK readyMutex DO rc.next := readyFrees; END;
    END;
  END CleanUpReady;

VAR
  readyMutex               := NEW(Thread.Mutex);
  readyFrees: ReadyClosure := NIL;

(* }}} *)
(* ignore SIGPIPES -- no longer needed? *)
(*
VAR
  vec := Usignal.struct_sigvec{Usignal.SIG_IGN, Usignal.empty_sv_mask, 0};
  ovec: Usignal.struct_sigvec;
*)
BEGIN
(*  EVAL Usignal.sigvec(Usignal.SIGPIPE, vec, ovec);
  IF ovec.sv_handler # Usignal.SIG_IGN
       AND ovec.sv_handler # Usignal.SIG_DFL THEN
    EVAL Usignal.sigvec(Usignal.SIGPIPE, ovec, vec);
  END;
*)
  hostNotFound := Atom.FromText("HostNotFound");
  sinkError := Atom.FromText("JVSink: ");

END JVSink.
