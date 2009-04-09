(* Copyright 1997-2003 John D. Polstra.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgment:
 *      This product includes software developed by John D. Polstra.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $Id: ChannelMux.m3,v 1.1.1.1 2009-04-09 17:01:52 jkrell Exp $ *)
<* PRAGMA LL *>

MODULE ChannelMux;

IMPORT
  Atom, AtomList, IP, Rd, RefSeq, TCP, Thread, Word, Wr;
IMPORT SupConnFD AS ConnFD;

(**)
IMPORT Fmt, IO;
CONST DoTrace = FALSE;
PROCEDURE Trace(msg: TEXT) =
  BEGIN
    IF DoTrace THEN IO.Put(msg & "\n") END;
  END Trace;
(**)

CONST
  ProtoVersion = 0;

  SendBufSize = 16 * 1024;
  RecvBufSize = 16 * 1024;

  MaxSegSize = 1024;

TYPE
  PacketType = {  (* Don't re-order these! *)
    StartupRequest,
    StartupReply,
    Connect,
    Accept,
    Reset,
    Data,
    Window,
    Close
  };

(* Locking conventions.

   We use medium-grained locking, to wit:  You can lock the multiplexer
   itself, and you can lock an individual channel.  The locking order,
   which must be followed in all cases, is:  multiplexer < channel.

   In other words, if you want to hold locks on the multiplexer and
   on a channel simultaneously, you must acquire the multiplexer
   lock first, and release it last. *)

(*****************************************************************************)
(* Multiplexers. *)
(*****************************************************************************)

REVEAL
  T = MUTEX BRANDED OBJECT
    channels: RefSeq.T;
    closed := TRUE;

    wr: Wr.T;
    sender: Sender;

    rd: Rd.T;
    receiver: Receiver;
  END;

PROCEDURE Open(rd: Rd.T;
               wr: Wr.T;
	       VAR (*OUT*) chan: Channel;
	       active: BOOLEAN): T
  RAISES {IP.Error, Thread.Alerted} =
  VAR
    mux: T;
  BEGIN
    mux := NEW(T,
      channels := NEW(RefSeq.T).init(),
      rd := rd,
      wr := wr);

    mux.receiver := NEW(Receiver).init(mux);
    mux.sender := NEW(Sender).init(mux);
    mux.closed := FALSE;

    TRY
      IF active THEN
	InitiateProtocol(mux, chan);
      ELSE
	AcceptProtocol(mux, chan);
      END;
    EXCEPT
    | Rd.Failure(l) => RAISE IP.Error(l);
    | Wr.Failure(l) => RAISE IP.Error(l);
    END;
    RETURN mux;
  END Open;

PROCEDURE Listen(mux: T): ChannelID
  RAISES {IP.Error} =
  <* LL = {} *>
  VAR
    chan: Channel;
  BEGIN
    LOCK mux DO
      chan := AllocChannel(mux);
    END;
    RETURN chan.id;
  END Listen;

PROCEDURE Accept(mux: T; id: ChannelID): Channel
  RAISES {IP.Error, Thread.Alerted} =
  <* LL = {} *>
  VAR
    chan: Channel;
  BEGIN
    LOCK mux DO
      chan := GetChannel(mux, id);
    END;
    LOCK chan DO
      WHILE chan.state = ChannelState.Listening DO
        Thread.AlertWait(chan, chan.rdReady);
      END;
      IF chan.state # ChannelState.Established THEN
	Raise(TCP.Closed);
      END;
    END;
    RETURN chan;
  END Accept;

PROCEDURE Connect(mux: T; id: ChannelID): Channel
  RAISES {IP.Error, Thread.Alerted} =
  VAR
    chan: Channel;
  BEGIN
    LOCK mux DO
      chan := GetChannel(mux, id);
    END;
    LOCK chan DO
      IF chan.state # ChannelState.Unused THEN
	Raise(IP.PortBusy);
      END;
      chan.state := ChannelState.Connecting;
      chan.flags := chan.flags + ChannelFlags{ChannelFlag.Connect};
    END;
    AwakenSender(mux);
    LOCK chan DO
      WHILE chan.state = ChannelState.Connecting DO
        Thread.AlertWait(chan, chan.wrReady);
      END;
      IF chan.state # ChannelState.Established THEN
	Raise(TCP.Refused);
      END;
    END;
    RETURN chan;
  END Connect;

PROCEDURE Close(mux: T) =
  BEGIN
    ShutdownProtocol(mux);
  END Close;

(*****************************************************************************)

PROCEDURE InitiateProtocol(mux: T;
                           VAR (*OUT*) chan: Channel)
  RAISES {IP.Error, Rd.Failure, Thread.Alerted, Wr.Failure} =
  VAR
    version: CARDINAL;
  BEGIN
    TRY
      PutStartupRequest(mux, ProtoVersion);
      NeedPacketType(mux, PacketType.StartupReply);
      GetStartupReply(mux, version);
      IF version # ProtoVersion THEN  (* There is only one version right now. *)
	Raise(TCP.Refused);
      END;

      StartThreads(mux);
      chan := Connect(mux, 0);
    EXCEPT Rd.EndOfFile =>
      Raise(TCP.ConnLost);
    END;
  END InitiateProtocol;

PROCEDURE AcceptProtocol(mux: T;
                         VAR (*OUT*) chan: Channel)
  RAISES {IP.Error, Rd.Failure, Thread.Alerted, Wr.Failure} =
  VAR
    hisVersion, myVersion: CARDINAL;
    id: ChannelID;
  BEGIN
    TRY
      NeedPacketType(mux, PacketType.StartupRequest);
      GetStartupRequest(mux, hisVersion);
      myVersion := ProtoVersion;  (* The only version. *)
      PutStartupReply(mux, myVersion);
      IF hisVersion # myVersion THEN  (* There is only one version right now. *)
	Raise(TCP.Refused);
      END;

      id := Listen(mux);
      <* ASSERT id = 0 *>
      StartThreads(mux);
      chan := Accept(mux, id);
    EXCEPT Rd.EndOfFile =>
      Raise(TCP.ConnLost);
    END;
  END AcceptProtocol;

PROCEDURE AllocChannel(mux: T): Channel
  RAISES {IP.Error} =
<* LL = mux *>
(* Returns an available "Channel" in the listening state. *)
  VAR
    chan: Channel;
  BEGIN
    WITH sz = mux.channels.size() DO
      FOR i := 0 TO sz-1 DO
	chan := mux.channels.get(i);
	LOCK chan DO
	  IF chan.state = ChannelState.Unused THEN
	    chan.state := ChannelState.Listening;
	    RETURN chan;
	  END;
	END;
      END;
      IF sz > LAST(ChannelID) THEN
	Raise(IP.NoResources);
      END;
      chan := NEW(Channel).init(mux, sz);
      chan.state := ChannelState.Listening;
      mux.channels.addhi(chan);
      RETURN chan;
    END;
  END AllocChannel;

PROCEDURE GetChannel(mux: T;
                     id: ChannelID): Channel =
<* LL = mux *>
(* Returns the "Channel" with the given "id", creating it if necessary. *)
  BEGIN
    FOR i := mux.channels.size() TO id DO
      mux.channels.addhi(NEW(Channel).init(mux, i));
    END;
    RETURN mux.channels.get(id);
  END GetChannel;

PROCEDURE StartThreads(mux: T) =
  BEGIN
    mux.receiver.thread := Thread.Fork(mux.receiver);
    mux.sender.thread := Thread.Fork(mux.sender);
  END StartThreads;

PROCEDURE ShutdownProtocol(mux: T;
                           error: AtomList.T := NIL) =
<* LL = {} *>
  VAR
    chan: Channel;
  BEGIN
    LOCK mux DO
      IF mux.closed THEN RETURN END;
      mux.closed := TRUE;
      WITH sz = mux.channels.size() DO
	FOR i := 0 TO sz-1 DO
	  chan := mux.channels.get(i);
	  LOCK chan DO
	    IF chan.state # ChannelState.Unused THEN
	      IF error # NIL THEN chan.error := error END;
	      chan.state := ChannelState.Closed;
	      chan.flags := ChannelFlags{};
	      Thread.Signal(chan.rdReady);
	      Thread.Signal(chan.wrReady);
	    END;
	  END;
	END;
      END;
    END;
    WITH me = Thread.Self() DO
      IF me # mux.sender.thread THEN
	Thread.Alert(mux.sender.thread);
	EVAL Thread.Join(mux.sender.thread);
      END;
      IF me # mux.receiver.thread THEN
	Thread.Alert(mux.receiver.thread);
	EVAL Thread.Join(mux.receiver.thread);
      END;
    END;
  END ShutdownProtocol;

PROCEDURE AwakenSender(mux: T) =
<* LL = {} *>
  VAR
    waitingForWork: BOOLEAN;
  BEGIN
    LOCK mux DO
      waitingForWork := mux.sender.waitingForWork;
    END;
    IF waitingForWork THEN
      Thread.Signal(mux.sender.newWork);
    END;
  END AwakenSender;

(*****************************************************************************)
(* Receiver thread. *)
(*****************************************************************************)

TYPE
  Receiver = Thread.Closure OBJECT
    mux: T;
    thread: Thread.T;
  METHODS
    init(mux: T): Receiver := RecvInit;
  OVERRIDES
    apply := RecvApply;
  END;

PROCEDURE RecvInit(self: Receiver; mux: T): Receiver =
  BEGIN
    self.mux := mux;
    RETURN self;
  END RecvInit;

PROCEDURE RecvApply(self: Receiver): REFANY =
  VAR
    type: PacketType;
    id: ChannelID;
    mss: CARDINAL;
    window: Word.T;
    length: CARDINAL;
    len1, len2: CARDINAL;
    chan: Channel;
    awakenSender: BOOLEAN;
    awakenClient: Thread.Condition;
  BEGIN
    TRY
      LOOP
	awakenSender := FALSE;
	awakenClient := NIL;
	type := GetPacketType(self.mux);
	CASE type OF
	| PacketType.StartupRequest, PacketType.StartupReply =>
	    RAISE Rd.Failure(AtomList.List1(ProtocolError));
	| PacketType.Connect =>
	    GetConnect(self.mux, id, mss, window);
	    LOCK self.mux DO
	      chan := GetChannel(self.mux, id);
	    END;
	    LOCK chan DO
	      IF chan.state = ChannelState.Listening THEN
		chan.sendMSS := mss;
		chan.sendWin := window;
		chan.state := ChannelState.Established;
		chan.flags := chan.flags + ChannelFlags{ChannelFlag.Accept};
		awakenClient := chan.rdReady;
	      ELSE
		chan.flags := chan.flags + ChannelFlags{ChannelFlag.Reset};
	      END;
	    END;
	    awakenSender := TRUE;
	| PacketType.Accept =>
	    GetAccept(self.mux, id, mss, window);
	    LOCK self.mux DO
	      chan := GetChannel(self.mux, id);
	    END;
	    LOCK chan DO
	      IF chan.state = ChannelState.Connecting THEN
		chan.sendMSS := mss;
		chan.sendWin := window;
		chan.state := ChannelState.Established;
		awakenClient := chan.wrReady;
	      ELSE
		chan.flags := chan.flags + ChannelFlags{ChannelFlag.Reset};
		awakenSender := TRUE;
	      END;
	    END;
	| PacketType.Reset =>
	    RAISE Rd.Failure(AtomList.List1(ProtocolError));
	| PacketType.Data =>
	    (* Read the packet directly into the buffer to avoid
	       unnecessary copying. *)
	    id := Get1(self.mux);
	    length := Get2(self.mux);
	    LOCK self.mux DO
	      chan := GetChannel(self.mux, id);
	    END;
	    LOCK chan DO
	      IF chan.state # ChannelState.Established
	      AND chan.state # ChannelState.WrClosed THEN
		RAISE Rd.Failure(AtomList.List1(ProtocolError));
	      END;
	      IF length > chan.recvMSS OR length > BufAvail(chan.recvBuf) THEN
		RAISE Rd.Failure(AtomList.List1(ProtocolError));
	      END;
	    END;
	    IF length > 0 THEN
	      WITH b = chan.recvBuf DO
		len1 := MIN(length, NUMBER(b.buf^) - b.in);
		len2 := length - len1;
		IF len1 > 0 THEN
		  GetN(self.mux, SUBARRAY(b.buf^, b.in, len1));
		END;
		IF len2 > 0 THEN
		  GetN(self.mux, SUBARRAY(b.buf^, 0, len2));
		END;
	      END;
	      LOCK chan DO
		INC(chan.recvBuf.in, length);
		IF chan.recvBuf.in >= NUMBER(chan.recvBuf.buf^) THEN
		  DEC(chan.recvBuf.in, NUMBER(chan.recvBuf.buf^));
		END;
		Trace("<D " & Fmt.Int(id) & " " & Fmt.Int(length)
		  & " [" & Fmt.Int(BufCount(chan.recvBuf)) & "]");
	      END;
	      awakenClient := chan.rdReady;
	    END;
	| PacketType.Window =>
	    GetWindow(self.mux, id, window);
	    LOCK self.mux DO
	      chan := GetChannel(self.mux, id);
	    END;
	    LOCK chan DO
	      IF chan.state = ChannelState.Established
	      OR chan.state = ChannelState.RdClosed THEN
		chan.sendWin := window;
		awakenSender := TRUE;
	      END;
	    END;
	| PacketType.Close =>
	    GetClose(self.mux, id);
	    LOCK self.mux DO
	      chan := GetChannel(self.mux, id);
	    END;
	    LOCK chan DO
	      IF chan.state = ChannelState.Established THEN
		chan.state := ChannelState.RdClosed;
	      ELSIF chan.state = ChannelState.WrClosed THEN
		chan.state := ChannelState.Closed;
	      ELSE
		RAISE Rd.Failure(AtomList.List1(ProtocolError));
	      END;
	    END;
	    awakenClient := chan.rdReady;
	END;
	IF awakenSender THEN AwakenSender(self.mux) END;
	IF awakenClient # NIL THEN Thread.Signal(awakenClient) END;
      END;
    EXCEPT
    | Rd.EndOfFile =>   ShutdownProtocol(self.mux);
    | Rd.Failure(l) =>  ShutdownProtocol(self.mux, l);
    | Thread.Alerted => (* We've been killed.  Just quit. *)
    END;
    Trace("Receiver terminates");
    RETURN NIL;
  END RecvApply;

(*****************************************************************************)
(* Sender thread. *)
(*****************************************************************************)

TYPE
  Sender = Thread.Closure OBJECT
    mux: T;
    thread: Thread.T;
    newWork: Thread.Condition;
    waitingForWork: BOOLEAN;
    lastID: CARDINAL;
  METHODS
    init(mux: T): Sender := SndrInit;
  OVERRIDES
    apply := SndrApply;
  END;

PROCEDURE SndrInit(self: Sender;
                   mux: T): Sender =
  BEGIN
    self.mux := mux;
    self.newWork := NEW(Thread.Condition);
    self.waitingForWork := FALSE;
    self.lastID := 0;
    RETURN self;
  END SndrInit;

PROCEDURE SndrApply(self: Sender): REFANY =
  VAR
    what: ChannelFlag;
    chan: Channel;
    id: ChannelID;
    mss: CARDINAL;
    window: Word.T;
    length, len1, len2: CARDINAL;
  BEGIN
    TRY
      LOOP
	SndrWaitForWork(self, what, chan);
	CASE what OF
	| ChannelFlag.Connect =>
	    LOCK chan DO
	      id := chan.id;
	      mss := chan.recvMSS;
	      window := Word.Extract(
		Word.Plus(chan.recvSeq, BufSize(chan.recvBuf)), 0, 32);
	    END;
	    PutConnect(self.mux, id, mss, window);
	| ChannelFlag.Accept =>
	    LOCK chan DO
	      id := chan.id;
	      mss := chan.recvMSS;
	      window := Word.Extract(
		Word.Plus(chan.recvSeq, BufSize(chan.recvBuf)), 0, 32);
	    END;
	    PutAccept(self.mux, id, mss, window);
	| ChannelFlag.Reset =>
	    LOCK chan DO
	      id := chan.id;
	    END;
	    PutReset(self.mux, id);
	| ChannelFlag.Window =>
	    LOCK chan DO
	      id := chan.id;
	      window := Word.Extract(
		Word.Plus(chan.recvSeq, BufSize(chan.recvBuf)), 0, 32);
	    END;
	    PutWindow(self.mux, id, window);
	| ChannelFlag.Data =>
	    LOCK chan DO
	      id := chan.id;
	      length := MIN(BufCount(chan.sendBuf), chan.sendMSS);
	      WITH winSize =
		Word.Extract(Word.Minus(chan.sendWin, chan.sendSeq), 0, 32)
	      DO
		IF Word.LT(winSize, length) THEN length := winSize END;
	      END;
	    END;
	    IF length > 0 THEN
	      (* Output the packet directly from the channel's send buffer, to
		 avoid unnecessary copying. *)
	      WITH b = chan.sendBuf DO
		len1 := MIN(length, NUMBER(b.buf^) - b.out);
		len2 := length - len1;
		PutPacketType(self.mux, PacketType.Data);
		Put1(self.mux, id);
		Put2(self.mux, length);
		IF len1 > 0 THEN
		  Wr.PutString(self.mux.wr, SUBARRAY(b.buf^, b.out, len1));
		END;
		IF len2 > 0 THEN
		  Wr.PutString(self.mux.wr, SUBARRAY(b.buf^, 0, len2));
		END;
		Wr.Flush(self.mux.wr);
	      END;
	      LOCK chan DO
		chan.sendSeq :=
		  Word.Extract(Word.Plus(chan.sendSeq, length), 0, 32);
		INC(chan.sendBuf.out, length);
		IF chan.sendBuf.out >= NUMBER(chan.sendBuf.buf^) THEN
		  DEC(chan.sendBuf.out, NUMBER(chan.sendBuf.buf^));
		END;
		Trace(">D " & Fmt.Int(id) & " " & Fmt.Int(length)
		  & " [" & Fmt.Int(BufCount(chan.sendBuf)) & "]");
	      END;
	      Thread.Signal(chan.wrReady);
	    END;
	| ChannelFlag.Close =>
	    LOCK chan DO
	      id := chan.id;
	    END;
	    PutClose(self.mux, id);
	END;
	(* ... *)
      END;
    EXCEPT
    | Thread.Alerted =>  (* We've been killed.  Just quit. *)
    | Wr.Failure(l)  =>  ShutdownProtocol(self.mux, l);
    END;
    Trace("Sender terminates");
    RETURN NIL;
  END SndrApply;

PROCEDURE SndrWaitForWork(self: Sender;
                          VAR (*OUT*) what: ChannelFlag;
                          VAR (*OUT*) chan: Channel)
  RAISES {Thread.Alerted} =
<* LL = {} *>
(* Waits until there is a channel that needs something done by the sender.
   Returns the task via "what" and the channel via "chan". *)
  BEGIN
    LOCK self.mux DO
      WHILE NOT SndrScan(self, what, chan) DO  (* Wait for something to do. *)
	self.waitingForWork := TRUE;
        Thread.AlertWait(self.mux, self.newWork);
      END;
      self.waitingForWork := FALSE;
    END;
  END SndrWaitForWork;

PROCEDURE SndrScan(self: Sender;
                   VAR (*OUT*) what: ChannelFlag;
                   VAR (*OUT*) chan: Channel): BOOLEAN =
<* LL = self.mux *>
(* Searches for a channel that needs something be done by the sender.  If
   such a channel is found, sets "what" and "chan" and returns "TRUE".  Else
   returns "FALSE". *)
  VAR
    flags: ChannelFlags;
    numChannels := self.mux.channels.size();
    id: CARDINAL := self.lastID;
  BEGIN
    IF numChannels > 0 THEN
      REPEAT
	INC(id);
	IF id >= numChannels THEN id := 0 END;
	chan := self.mux.channels.get(id);
	LOCK chan DO
	  IF chan.state # ChannelState.Unused THEN
	    flags := chan.flags;
	    IF chan.sendSeq # chan.sendWin AND BufCount(chan.sendBuf) > 0 THEN
	      (* We can send some data. *)
	      flags := flags + ChannelFlags{ChannelFlag.Data};
	    END;
	    IF flags # ChannelFlags{} THEN  (* Something to do. *)
	      FOR w := FIRST(ChannelFlag) TO LAST(ChannelFlag) DO
		IF w IN flags THEN
		  chan.flags := chan.flags - ChannelFlags{w};
		  self.lastID := id;
		  what := w;
		  RETURN TRUE;
		END;
	      END;
	    END;
	  END;
	END;
      UNTIL id = self.lastID;
    END;
    RETURN FALSE;
  END SndrScan;

(*****************************************************************************)
(* Channels. *)
(*****************************************************************************)

REVEAL
  Channel = ConnFD.T BRANDED OBJECT
    mux: T;
    id: ChannelID;
    state: ChannelState;
    flags: ChannelFlags;
    error: AtomList.T;
    rdReady: Thread.Condition;
    wrReady: Thread.Condition;

    (* Sender state variables. *)
    sendBuf: Buffer;
    sendSeq: Word.T;    (* Next byte number to send (MOD 2^32). *)
    sendWin: Word.T;    (* Allowed to advance sendSeq this far (MOD 2^32). *)
    sendMSS: CARDINAL;	(* Allowed to send data packets this large. *)

    (* Receiver state variables. *)
    recvBuf: Buffer;
    recvSeq: Word.T;    (* Next byte number for the application (MOD 2^32). *)
    recvMSS: CARDINAL;  (* Peer should never send us data packets larger. *)
  METHODS
    init(mux: T;
         id: ChannelID): Channel := ChanInit;
  OVERRIDES
    get := ChanGet;
    put := ChanPut;
    shutdownIn := ChanShutdownIn;
    shutdownOut := ChanShutdownOut;
    close := ChanClose;
  END;

TYPE
  ChannelState = {
    Unused,
    Listening,
    Connecting,
    Established,
    RdClosed,      (* Reading half has been closed. *)
    WrClosed,      (* Writing half has been closed. *)
    Closed
  };
  ChannelFlag = {  (* Ordered from most urgent to least urgent. *)
    Connect,  (* Must send a connect packet. *)
    Accept,   (* Must send an accept packet. *)
    Reset,    (* Must send a Reset packet. *)
    Window,   (* Must send a window update packet. *)
    Data,     (* Must send a data packet. *)
    Close     (* Must send a close packet. *)
  };
  ChannelFlags = SET OF ChannelFlag;

PROCEDURE ChanInit(self: Channel;
                   mux: T;
                   id: ChannelID): Channel =
  BEGIN
    self.mux := mux;
    self.id := id;
    self.state := ChannelState.Unused;
    self.flags := ChannelFlags{};
    self.rdReady := NEW(Thread.Condition);
    self.wrReady := NEW(Thread.Condition);
    self.error := NIL;

    self.sendBuf := NEW(Buffer).init(SendBufSize);
    self.sendSeq := 0;
    self.sendWin := 0;
    self.sendMSS := 0;

    self.recvBuf := NEW(Buffer).init(RecvBufSize);
    self.recvSeq := 0;
    self.recvMSS := MaxSegSize;
    RETURN self;
  END ChanInit;

PROCEDURE ChanGet(self: Channel;
                  VAR arr: ARRAY OF CHAR;
                  waitFor: LONGREAL := -1.0D0): CARDINAL
  RAISES {Rd.Failure, Thread.Alerted, ConnFD.TimedOut} =
  VAR
    count: CARDINAL;
    n: CARDINAL;
  BEGIN
    (* FIXME - We only handle a "waitFor" of -1.0d0 and 0.0d0. *)
    IF NUMBER(arr) = 0 THEN RETURN 0 END;
    LOCK self DO
      LOOP
        IF self.error # NIL THEN
          RAISE Rd.Failure(self.error);
        END;
	count := BufCount(self.recvBuf);
	CASE self.state OF
	| ChannelState.Established, ChannelState.WrClosed =>
	    IF count > 0 THEN EXIT END;
	| ChannelState.RdClosed, ChannelState.Closed =>
	    EXIT;
	ELSE
	  RAISE Rd.Failure(AtomList.List1(TCP.Closed));
	END;
	IF waitFor >= 0.0d0 THEN RAISE ConnFD.TimedOut END;
	Thread.AlertWait(self, self.rdReady);
      END;
      n := MIN(count, NUMBER(arr));
      BufGet(self.recvBuf, SUBARRAY(arr, 0, n));
      Trace("G  " & Fmt.Int(self.id) & " " & Fmt.Int(n)
	& " [" & Fmt.Int(BufCount(self.recvBuf)) & "]");
      self.recvSeq := Word.Extract(Word.Plus(self.recvSeq, n), 0, 32);
      self.flags := self.flags + ChannelFlags{ChannelFlag.Window};
    END;
    AwakenSender(self.mux);
    RETURN n;
  END ChanGet;

PROCEDURE ChanPut(self: Channel;
                  READONLY arr: ARRAY OF CHAR)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    pos := 0;
    avail: CARDINAL;
    n: CARDINAL;
  BEGIN
    WHILE pos < NUMBER(arr) DO
      LOCK self DO
	LOOP
          IF self.error # NIL THEN
            RAISE Wr.Failure(self.error);
          END;
	  IF self.state # ChannelState.Established
	  AND self.state # ChannelState.RdClosed THEN
	    RAISE Wr.Failure(AtomList.List1(TCP.Closed));
	  END;
	  avail := BufAvail(self.sendBuf);
	  IF avail > 0 THEN EXIT END;
	  Thread.AlertWait(self, self.wrReady);
	END;
	n := MIN(avail, NUMBER(arr) - pos);
	BufPut(self.sendBuf, SUBARRAY(arr, pos, n));
	Trace("P  " & Fmt.Int(self.id) & " " & Fmt.Int(n)
	  & " [" & Fmt.Int(BufCount(self.sendBuf)) & "]");
	INC(pos, n);
      END;
      AwakenSender(self.mux);
    END;
  END ChanPut;

PROCEDURE ChanShutdownIn(<*UNUSED*> self: Channel) =
  BEGIN
    (* Ignored for now. *)
  END ChanShutdownIn;

PROCEDURE ChanShutdownOut(self: Channel)
  RAISES {Wr.Failure} =
  VAR
    awakenSender := FALSE;
  BEGIN
    LOCK self DO
      CASE self.state OF
      | ChannelState.Established =>
          self.state := ChannelState.WrClosed;
	  self.flags := self.flags + ChannelFlags{ChannelFlag.Close};
	  awakenSender := TRUE;
      | ChannelState.RdClosed =>
          self.state := ChannelState.Closed;
	  self.flags := self.flags + ChannelFlags{ChannelFlag.Close};
	  awakenSender := TRUE;
      | ChannelState.WrClosed, ChannelState.Closed =>  (* Be tolerant. *)
          RETURN;
      ELSE
        RAISE Wr.Failure(AtomList.List1(TCP.Closed));
      END;
    END;
    IF awakenSender THEN AwakenSender(self.mux) END;
    (* It seems like we ought to wait here for the send buffer to empty
       out.  But the "ConnFD" interface doesn't define this method as
       alertable, and we don't want any possibility of blocking forever. *)
  END ChanShutdownOut;

PROCEDURE ChanClose(self: Channel) =
  BEGIN
    TRY self.shutdownOut() EXCEPT ELSE END;
    TRY self.shutdownIn() EXCEPT ELSE END;
  END ChanClose;

(*****************************************************************************)
(* Circular buffers. *)
(*****************************************************************************)

TYPE
  Buffer = OBJECT
    buf: REF ARRAY OF CHAR;
    in, out: CARDINAL;
  METHODS
    init(size: CARDINAL): Buffer := BufInit;
  END;

PROCEDURE BufInit(self: Buffer; size: CARDINAL): Buffer =
  BEGIN
    self.buf := NEW(REF ARRAY OF CHAR, size + 1);
    self.in := 0;
    self.out := 0;
    RETURN self;
  END BufInit;

PROCEDURE BufSize(self: Buffer): CARDINAL =
(* Returns the maximum capacity of the given buffer in bytes. *)
  BEGIN
    RETURN NUMBER(self.buf^) - 1;
  END BufSize;

PROCEDURE BufAvail(self: Buffer): CARDINAL =
  VAR
    avail: INTEGER;
  BEGIN
    avail := self.out - self.in - 1;
    IF avail < 0 THEN INC(avail, NUMBER(self.buf^)) END;
    RETURN avail;
  END BufAvail;

PROCEDURE BufCount(self: Buffer): CARDINAL =
  VAR
    count: INTEGER;
  BEGIN
    count := self.in - self.out;
    IF count < 0 THEN INC(count, NUMBER(self.buf^)) END;
    RETURN count;
  END BufCount;

PROCEDURE BufPut(self: Buffer; READONLY a: ARRAY OF CHAR) =
  VAR
    newIn: CARDINAL;
  BEGIN
    WITH len1 = NUMBER(self.buf^) - self.in DO
      IF len1 >= NUMBER(a) THEN  (* Not wrapping around. *)
	SUBARRAY(self.buf^, self.in, NUMBER(a)) := a;
      ELSE  (* Wrapping around. *)
	SUBARRAY(self.buf^, self.in, len1) := SUBARRAY(a, 0, len1);
	WITH len2 = NUMBER(a) - len1 DO
	  SUBARRAY(self.buf^, 0, len2) := SUBARRAY(a, len1, len2);
	END;
      END;
    END;
    newIn := self.in + NUMBER(a);
    IF newIn >= NUMBER(self.buf^) THEN DEC(newIn, NUMBER(self.buf^)) END;
    self.in := newIn;
  END BufPut;

PROCEDURE BufGet(self: Buffer; VAR a: ARRAY OF CHAR) =
  VAR
    newOut: CARDINAL;
  BEGIN
    WITH len1 = NUMBER(self.buf^) - self.out DO
      IF len1 >= NUMBER(a) THEN  (* Not wrapping around. *)
	a := SUBARRAY(self.buf^, self.out, NUMBER(a));
      ELSE
	SUBARRAY(a, 0, len1) := SUBARRAY(self.buf^, self.out, len1);
	WITH len2 = NUMBER(a) - len1 DO
	  SUBARRAY(a, len1, len2) := SUBARRAY(self.buf^, 0, len2);
	END;
      END;
    END;
    newOut := self.out + NUMBER(a);
    IF newOut >= NUMBER(self.buf^) THEN DEC(newOut, NUMBER(self.buf^)) END;
    self.out := newOut;
  END BufGet;

(*****************************************************************************)
(* Packet I/O. *)
(*****************************************************************************)

PROCEDURE PutStartupRequest(mux: T;
                            version: CARDINAL)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    PutPacketType(mux, PacketType.StartupRequest);
    Put2(mux, version);
    Wr.Flush(mux.wr);
  END PutStartupRequest;

PROCEDURE PutStartupReply(mux: T;
                          version: CARDINAL)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    PutPacketType(mux, PacketType.StartupReply);
    Put2(mux, version);
    Wr.Flush(mux.wr);
  END PutStartupReply;

PROCEDURE PutConnect(mux: T;
                     id: ChannelID;
		     mss: CARDINAL;
                     window: Word.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    PutPacketType(mux, PacketType.Connect);
    Put1(mux, id);
    Put2(mux, mss);
    Put4(mux, window);
    Wr.Flush(mux.wr);
  END PutConnect;

PROCEDURE PutAccept(mux: T;
                    id: ChannelID;
		    mss: CARDINAL;
                    window: Word.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    PutPacketType(mux, PacketType.Accept);
    Put1(mux, id);
    Put2(mux, mss);
    Put4(mux, window);
    Wr.Flush(mux.wr);
  END PutAccept;

PROCEDURE PutReset(mux: T;
                   id: ChannelID)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    PutPacketType(mux, PacketType.Reset);
    Put1(mux, id);
    Wr.Flush(mux.wr);
  END PutReset;

PROCEDURE PutWindow(mux: T;
                    id: ChannelID;
                    window: Word.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    PutPacketType(mux, PacketType.Window);
    Put1(mux, id);
    Put4(mux, window);
    Wr.Flush(mux.wr);
    Trace(">W " & Fmt.Int(id) & " " & Fmt.Unsigned(window, 10));
  END PutWindow;

PROCEDURE PutClose(mux: T;
                   id: ChannelID)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    PutPacketType(mux, PacketType.Close);
    Put1(mux, id);
    Wr.Flush(mux.wr);
    Trace(">C " & Fmt.Int(id));
  END PutClose;

(*****************************************************************************)

PROCEDURE GetStartupRequest(mux: T;
                            VAR (*OUT*) version: CARDINAL)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    version := Get2(mux);
  END GetStartupRequest;

PROCEDURE GetStartupReply(mux: T;
                          VAR (*OUT*) version: CARDINAL)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    version := Get2(mux);
  END GetStartupReply;

PROCEDURE GetConnect(mux: T;
                     VAR (*OUT*) id: ChannelID;
		     VAR (*OUT*) mss: CARDINAL;
                     VAR (*OUT*) window: Word.T)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    id := Get1(mux);
    mss := Get2(mux);
    window := Get4(mux);
  END GetConnect;

PROCEDURE GetAccept(mux: T;
                    VAR (*OUT*) id: ChannelID;
		    VAR (*OUT*) mss: CARDINAL;
                    VAR (*OUT*) window: Word.T)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    id := Get1(mux);
    mss := Get2(mux);
    window := Get4(mux);
  END GetAccept;

PROCEDURE GetWindow(mux: T;
                    VAR (*OUT*) id: ChannelID;
                    VAR (*OUT*) window: Word.T)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    id := Get1(mux);
    window := Get4(mux);
    Trace("<W " & Fmt.Int(id) & " " & Fmt.Unsigned(window, 10));
  END GetWindow;

PROCEDURE GetClose(mux: T;
                   VAR (*OUT*) id: ChannelID)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    id := Get1(mux);
    Trace("<C " & Fmt.Int(id));
  END GetClose;

(*****************************************************************************)

PROCEDURE PutPacketType(mux: T; type: PacketType)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    Put1(mux, ORD(type));
  END PutPacketType;

PROCEDURE Put1(mux: T; v: Word.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    Wr.PutChar(mux.wr, VAL(v, CHAR));
  END Put1;

PROCEDURE Put2(mux: T; v: Word.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    Wr.PutString(mux.wr, ARRAY [0..1] OF CHAR{
      VAL(Word.Extract(v, 8, 8), CHAR),
      VAL(Word.Extract(v, 0, 8), CHAR)});
  END Put2;

PROCEDURE Put4(mux: T; v: Word.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    Wr.PutString(mux.wr, ARRAY [0..3] OF CHAR{
      VAL(Word.Extract(v, 24, 8), CHAR),
      VAL(Word.Extract(v, 16, 8), CHAR),
      VAL(Word.Extract(v, 8, 8), CHAR),
      VAL(Word.Extract(v, 0, 8), CHAR)});
  END Put4;

(*****************************************************************************)

PROCEDURE NeedPacketType(mux: T; type: PacketType)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    IF GetPacketType(mux) # type THEN
      RAISE Rd.Failure(AtomList.List1(ProtocolError));
    END;
  END NeedPacketType;

PROCEDURE GetPacketType(mux: T): PacketType
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    v := Get1(mux);
  BEGIN
    IF v > ORD(LAST(PacketType)) THEN
      RAISE Rd.Failure(AtomList.List1(ProtocolError));
    END;
    RETURN VAL(v, PacketType);
  END GetPacketType;

PROCEDURE Get1(mux: T): Word.T
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN ORD(Rd.GetChar(mux.rd));
  END Get1;

PROCEDURE Get2(mux: T): Word.T
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    a: ARRAY[0..1] OF CHAR;
    v: Word.T;
  BEGIN
    IF Rd.GetSub(mux.rd, a) # NUMBER(a) THEN
      RAISE Rd.EndOfFile;
    END;
    v := ORD(a[0]);
    v := Word.Or(Word.LeftShift(v, 8), ORD(a[1]));
    RETURN v;
  END Get2;

PROCEDURE Get4(mux: T): Word.T
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    a: ARRAY[0..3] OF CHAR;
    v: Word.T;
  BEGIN
    IF Rd.GetSub(mux.rd, a) # NUMBER(a) THEN
      RAISE Rd.EndOfFile;
    END;
    v := ORD(a[0]);
    v := Word.Or(Word.LeftShift(v, 8), ORD(a[1]));
    v := Word.Or(Word.LeftShift(v, 8), ORD(a[2]));
    v := Word.Or(Word.LeftShift(v, 8), ORD(a[3]));
    RETURN v;
  END Get4;

PROCEDURE GetN(mux: T; VAR arr: ARRAY OF CHAR)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    IF Rd.GetSub(mux.rd, arr) # NUMBER(arr) THEN
      RAISE Rd.EndOfFile;
    END;
  END GetN;

(*****************************************************************************)

PROCEDURE Raise(a: Atom.T)
  RAISES {IP.Error} =
  BEGIN
    RAISE IP.Error(AtomList.List1(a));
  END Raise;

BEGIN
  ProtocolError := Atom.FromText("ChannelMux.ProtocolError");
END ChannelMux.
