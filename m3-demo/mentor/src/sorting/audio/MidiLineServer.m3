(* Copyright 1992 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* Last modified on Thu Jun 20 17:23:58 PDT 1996 by heydon   *)
(*      modified on Tue May  3 19:06:17 PDT 1994 by najork   *)
(*      modified on Tue Dec 22 13:56:41 PST 1992 by mhb      *)
(*      modified on Fri Nov 20 15:49:27 PST 1992 by sclafani *)

UNSAFE MODULE MidiLineServer;

IMPORT M3toC, Midi, MidiPrivate;
IMPORT Ctypes, Uerror, Uin, Unetdb, Unix, Usocket, Utime;
FROM Midi IMPORT Failure;

CONST
  MIDIPortNumber    = 4103;     (* UDP port used (AUDIO + 1) *)
  MaxTries          = 5;
  DelayMicroseconds = 250000;
  MaxDataSize       = 1024;

REVEAL
  T = Midi.T BRANDED OBJECT
        socket  : Ctypes.int;
        addr    : Uin.struct_sockaddr_in;
        callLock: MUTEX;
        seq     : INTEGER;
      OVERRIDES
        init  := Open;
        play  := Play;
        close := Close;
      END;

TYPE
  Byte = BITS 8 FOR [0 .. 255];

  Operation = {NoOP,            (* invalid *)
               PlayOP,          (* play a sample *)
               RecordOP,        (* get a sample *)
               LoopbackOP,      (* echo packet back to sender *)
               ResetOP          (* reset the midi system *)
              };

  Header = RECORD
             sequence: INTEGER;  (* packet sequence number *)
             time    : INTEGER;  (* MIDIServer time *)
             opcode  : INTEGER;  (* function to perform *)
             param   : INTEGER;  (* opcode parameter #1 *)
           END;

  Command = RECORD
              hdr : Header;
              data: ARRAY [1 .. MaxDataSize] OF Byte;  (* packet data *)
            END;

PROCEDURE Open (t: T; name: TEXT): Midi.T RAISES {Failure} =
  VAR hp: Unetdb.struct_hostent_star;
  BEGIN
    WITH string = M3toC.CopyTtoS(name) DO
      hp := Unetdb.gethostbyname(string);
      M3toC.FreeCopiedS(string);
    END;
    IF hp = NIL THEN RAISE Failure("host name unknown"); END;
    t.addr.sin_family := hp.h_addrtype;
    <* ASSERT hp.h_length = BYTESIZE (t.addr.sin_addr) *>
    t.addr.sin_addr.s_addr := 
        LOOPHOLE (hp.h_addr_list^, Ctypes.unsigned_int_star)^;
    Unetdb.endhostent();

    t.addr.sin_port := Uin.htons(MIDIPortNumber);
    t.socket :=
      Usocket.socket(Usocket.AF_INET, Usocket.SOCK_DGRAM, 0);
    IF t.socket < 0 THEN UnixFail("socket"); END;
    IF Usocket.connect(t.socket,
                       LOOPHOLE(ADR(t.addr), UNTRACED REF
                                Usocket.struct_sockaddr),
                       BYTESIZE(t.addr)) < 0 THEN
      UnixFail("connect");
    END;
    t.callLock := NEW(MUTEX);
    t.seq := 0;
    Reset(t);
    RETURN t;
  END Open;

PROCEDURE Close (<*UNUSED*> t: T) =
  BEGIN
  END Close;

PROCEDURE Reset (t: T) RAISES {Failure} =
  VAR req: Command;
  BEGIN
    LOCK t.callLock DO
      TRY
        Rpc (t, Operation.ResetOP, req, BYTESIZE (Header));
      FINALLY
        INC (t.seq);
      END;
    END;
  END Reset;

PROCEDURE Play (         t     : T;
                READONLY buf   : ARRAY OF Byte;
                         start : CARDINAL        := 0;
                         length: CARDINAL        := LAST (CARDINAL))
  RAISES {Failure} =
  VAR req: Command;
  BEGIN
    length := MIN (length, NUMBER (buf) - start);
    IF length > MaxDataSize THEN
      RAISE Failure ("too much data to pass to the LineServer");
    END;
    WITH count = MIN (
                   MIN (NUMBER (req.data), NUMBER (buf) - start), length) DO
      SUBARRAY (req.data, 0, count) := SUBARRAY (buf, start, count);
    END;
    LOCK t.callLock DO
      TRY
        Rpc (t, Operation.PlayOP, req, BYTESIZE (Header) + length);
      FINALLY
        INC (t.seq);
      END;
    END;
  END Play;

PROCEDURE Rpc (t: T; op: Operation; VAR request: Command; reqSize: INTEGER)
  RAISES {Failure} =
  VAR
    readfds: Unix.FDSet;
    timeout: Utime.struct_timeval;
    result : INTEGER;
    reply  : Command;
  BEGIN
    request.hdr.sequence := Uin.htonl (t.seq);
    request.hdr.time := 0;
    request.hdr.opcode := Uin.htonl (ORD (op));
    request.hdr.param := 0;

    timeout.tv_sec := 0;
    timeout.tv_usec := DelayMicroseconds;

    FOR i := 1 TO MaxTries DO
      result := Usocket.send (t.socket, ADR (request), reqSize, 0);
      IF result < 0 THEN UnixFail ("send"); END;
      IF result # reqSize THEN RAISE Failure ("partial send!?!"); END;
      readfds := Unix.FDSet {t.socket};
      result := Unix.select (t.socket + 1, ADR (readfds), NIL, NIL,
                             ADR (timeout));
      IF (result < 0) AND (Uerror.errno # Uerror.EINTR) THEN
        UnixFail ("select");
      END;
      IF result > 0 THEN
        result :=
          Usocket.recv (t.socket, ADR (reply), BYTESIZE (reply), 0);
        IF result < 0 THEN
          IF Uerror.errno # Uerror.EINTR THEN UnixFail ("recv"); END;
        ELSIF (Uin.ntohl (reply.hdr.sequence) = t.seq) THEN
          IF Uin.ntohl (reply.hdr.param) < 0 THEN
            UnixFail ("LineServer operation failed");
          END;
          RETURN;
        END;
      END;
    END;
    RAISE Failure ("no reply from the LineServer");
  END Rpc;

PROCEDURE UnixFail (msg: TEXT) RAISES {Failure} =
  BEGIN
    RAISE Failure (msg & ": unix error: "
                     & M3toC.StoT (Uerror.GetFrom_sys_errlist (
                                     Uerror.errno)));
  END UnixFail;

BEGIN
END MidiLineServer.
