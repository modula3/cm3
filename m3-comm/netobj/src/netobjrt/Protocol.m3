(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Protocol.m3 *)
(* Last modified on Fri Nov 12 15:45:07 PST 1993 by wobber *)
(*      modified on Wed Jun 10 17:14:36 PDT 1992 by owicki *)

MODULE Protocol;

IMPORT NetObj, StubLib, Rd, Wr, Thread;

PROCEDURE RecvOp(c: StubLib.Conn; state: State; VAR rep: StubLib.DataRep) : Op
     RAISES {NetObj.Error, Rd.Failure, Thread.Alerted} =
  VAR i: CARDINAL;
      op: Op;
  BEGIN
    IF state # State.CallWait THEN
      IF NOT c.rd.nextMsg() THEN
        StubLib.RaiseUnmarshalFailure();
      END;
    END;
    StubLib.InBytes(c, rep.byte);
    i := StubLib.InInt32(c, rep, 0, ORD(LAST(Op)));
    op := VAL(i, Op);
    CASE state OF
    | State.CallWait =>
        IF op # Op.MethodCall THEN StubLib.RaiseUnmarshalFailure(); END;
    | State.ReturnWait =>
        IF op # Op.Return AND op # Op.CallFailed THEN
          StubLib.RaiseUnmarshalFailure();
        END;
    | State.AckWait =>
        IF op # Op.ResultAck THEN StubLib.RaiseUnmarshalFailure(); END;
    | State.StreamWait =>
        IF op # Op.StreamData THEN StubLib.RaiseUnmarshalFailure(); END;
    | State.StreamResWait =>
        IF op # Op.StreamRes THEN StubLib.RaiseUnmarshalFailure(); END;
    END;
    RETURN op;
  END RecvOp;
     
PROCEDURE SendOp(c: StubLib.Conn; op: Op)
     RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    StubLib.OutBytes(c, StubLib.NativeRep.byte);
    StubLib.OutInt32(c, ORD(op));
  END SendOp;

BEGIN
END Protocol.

