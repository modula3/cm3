(* Copyright (C) 1989, 1990, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Tue Feb 22 23:10:15 PST 1994 by kalsow   *)
(*      modified on Mon Oct 25 12:21:12 PDT 1993 by sfreeman *)

(* note: SIGPIPE set to be ignored by JVSink *)

UNSAFE MODULE Jva;

IMPORT Atom, AtomList, Jv, JvaProtocol, M3toC, OSError, Text, Thread;

REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
        init          := Init;
        connect       := Connect;
        setMute       := SetMute;
        setVolume     := SetVolume;
        getStatistics := GetStatistics;
        close         := Close;
      END;

PROCEDURE Init (t: T): T RAISES {OSError.E} =
  BEGIN
    TRY
      LOCK t DO RETURN Jv.T.init(t, JvaProtocol.PipeName); END;
    EXCEPT
    | OSError.E (e) => RAISE OSError.E(AtomList.Cons(Jv.ServerFailure, e));
    END;
  END Init;

PROCEDURE Connect (t: T; hostname: TEXT)
  RAISES {OSError.E, Thread.Alerted} =
  VAR
    req  : JvaProtocol.ConnectReqRec;
    reply: JvaProtocol.ConnectReplyRec;
  BEGIN
    IF hostname = NIL OR Text.Empty(hostname) THEN
      RAISE OSError.E(AtomList.List1(invalidHostname));
    END;
    TRY
      req.hostnameLength := Text.Length(hostname) + 1;
      WITH string = M3toC.TtoS(hostname) DO
          LOCK t DO
            Jv.Send(t, ADR(req), BYTESIZE(req));
            Jv.Send(t, string, req.hostnameLength);
            Jv.Recv(t, ADR(reply), BYTESIZE(reply));
          END;
          IF reply.request # req.request THEN
            RAISE OSError.E(AtomList.List1(Atom.FromText("Connect")));
          END;
      END;
    EXCEPT
    | OSError.E (e) => RAISE OSError.E(AtomList.Cons(Jv.ServerFailure, e));
    END;
  END Connect;

CONST
  MuteVal = ARRAY BOOLEAN OF
              JvaProtocol.MuteCode{JvaProtocol.MuteOff, JvaProtocol.MuteOn};

PROCEDURE SetMute (t: T; on: BOOLEAN) RAISES {OSError.E, Thread.Alerted} =
  VAR
    req  : JvaProtocol.MuteReqRec;
    reply: JvaProtocol.MuteReplyRec;
  BEGIN
    TRY
      req.mute := MuteVal[on];
      LOCK t DO
        Jv.Send(t, ADR(req), BYTESIZE(req));
        Jv.Recv(t, ADR(reply), BYTESIZE(reply));
      END;
      IF reply.request # req.request THEN
        RAISE OSError.E(AtomList.List1(Atom.FromText("Mute")));
      END;
    EXCEPT
    | OSError.E (e) => RAISE OSError.E(AtomList.Cons(Jv.ServerFailure, e));
    END;
  END SetMute;

PROCEDURE SetVolume (t: T; volume: Volume)
  RAISES {OSError.E, Thread.Alerted} =
  VAR
    req  : JvaProtocol.VolumeReqRec;
    reply: JvaProtocol.VolumeReplyRec;
  BEGIN
    TRY
      req.volume := volume;
      LOCK t DO
        Jv.Send(t, ADR(req), BYTESIZE(req));
        Jv.Recv(t, ADR(reply), BYTESIZE(reply));
      END;
      IF reply.request # req.request THEN
        RAISE OSError.E(AtomList.List1(Atom.FromText("Volume")));
      END;
    EXCEPT
    | OSError.E (e) => RAISE OSError.E(AtomList.Cons(Jv.ServerFailure, e));
    END;
  END SetVolume;

PROCEDURE GetStatistics (t: T): Statistics
  RAISES {OSError.E, Thread.Alerted} =
  VAR
    req  : JvaProtocol.StatisticsReqRec;
    reply: JvaProtocol.StatisticsReplyRec;
  BEGIN
    TRY
      LOCK t DO
        Jv.Send(t, ADR(req), BYTESIZE(req));
        Jv.Recv(t, ADR(reply), BYTESIZE(reply));
      END;
      IF reply.request # req.request THEN
        RAISE OSError.E(AtomList.List1(Atom.FromText("Statistics")));
      END;
    EXCEPT
    | OSError.E (e) => RAISE OSError.E(AtomList.Cons(Jv.ServerFailure, e));
    END;
    RETURN reply.statistics;
  END GetStatistics;

PROCEDURE Close (t: T) =
  BEGIN
    LOCK t DO Jv.T.close(t); END;
  END Close;

BEGIN
  invalidHostname := Atom.FromText("Jva: invalid hostname");
END Jva.
