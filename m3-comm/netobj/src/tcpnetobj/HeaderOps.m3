(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Created on Sat Jan 11 15:49:00 PST 1992 by gnelson *)
(* Last modified on Wed Aug 31 16:14:27 PDT 1994 by wobber *)
(*      modified on Sun Jan 12 16:17:07 PST 1992 by meehan *)

UNSAFE MODULE HeaderOps;

IMPORT Atom, AtomList, TCP, ConnFD, Swap, Text, Thread, Rd, Wr;

CONST
  CurrentVersion = 1;
  MaxHeaderData = 500;

TYPE
  Header = RECORD
    fx: Fixed;
    data: ARRAY [0..MaxHeaderData-1] OF CHAR;
  END;
  HeaderAlias = UNTRACED REF ARRAY [0..BYTESIZE(Header)-1] OF CHAR;

  Fixed = RECORD
    version: BITS 8 FOR [0..255];
    opCode: BITS 8 FOR [0..255];
    length: BITS 16 FOR [0..65535];  (* non-inclusive, little-endian *)
  END;
  FixedAlias = UNTRACED REF ARRAY [0..BYTESIZE(Fixed)-1] OF CHAR;

VAR ProtocolError: Atom.T;

PROCEDURE Send(t: TCP.T; op: Op; hisEP, myEP: TEXT := NIL)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR hdr: Header;
      length: CARDINAL;
  BEGIN
    hdr.fx.version := CurrentVersion;
    hdr.fx.opCode := ORD(op);
    hdr.fx.length := 0;
    IF op = Op.Connect OR op = Op.Ping THEN
      StuffText(hdr, hisEP);
      StuffText(hdr, myEP);
    END;
    length := hdr.fx.length;
    IF Swap.endian = Swap.Endian.Big THEN
      hdr.fx.length := Swap.Swap2U(length);
    END;
    t.put(SUBARRAY(LOOPHOLE(ADR(hdr), HeaderAlias)^,
                     0, length+BYTESIZE(Fixed)));
  END Send;

PROCEDURE StuffText(VAR hdr: Header; t: TEXT) =
  VAR len := MIN(Text.Length(t), NUMBER(hdr.data)-(hdr.fx.length+1));
  BEGIN
    Text.SetChars(SUBARRAY(hdr.data, hdr.fx.length, len), t);
    hdr.data[hdr.fx.length+len] := '\000';
    INC(hdr.fx.length, len+1);
  END StuffText;

PROCEDURE Receive(
    t: TCP.T;
    timeout: LONGREAL;
    VAR myEP: TEXT;
    VAR hisEP: TEXT) : Op
    RAISES {Rd.Failure, Thread.Alerted, ConnFD.TimedOut} =
  PROCEDURE RaiseProtocolError() RAISES {Rd.Failure} =
    BEGIN
      RAISE Rd.Failure(AtomList.List1(ProtocolError));
    END RaiseProtocolError;
    VAR hdr: Header;
      x, pos: INTEGER;
  BEGIN
    x := t.get(LOOPHOLE(ADR(hdr.fx), FixedAlias)^, timeout);
    IF Swap.endian = Swap.Endian.Big THEN
      hdr.fx.length := Swap.Swap2U(hdr.fx.length);
    END;
    IF x # BYTESIZE(hdr.fx) OR
           hdr.fx.length > BYTESIZE(hdr.data) OR
           hdr.fx.version # CurrentVersion THEN
      RaiseProtocolError();
    END;
    IF hdr.fx.length # 0 THEN
      x := t.get(SUBARRAY(hdr.data, 0, hdr.fx.length), timeout);
      IF x # hdr.fx.length THEN RaiseProtocolError(); END;
    END;
    CASE hdr.fx.opCode OF
    | ORD(Op.Connect), ORD(Op.Ping) =>
        pos := 0;
        myEP := ExtractText(hdr, pos);
        hisEP := ExtractText(hdr, pos);
        IF myEP = NIL OR hisEP = NIL THEN RaiseProtocolError(); END;
    | ORD(Op.PingAck), ORD(Op.PingError) =>
    ELSE RaiseProtocolError();
    END;
    RETURN VAL(hdr.fx.opCode, Op);
  END Receive;

PROCEDURE ExtractText(VAR hdr: Header; VAR pos: INTEGER) : TEXT =
  VAR t: TEXT;
  BEGIN
    FOR i := pos TO hdr.fx.length-1 DO
      IF hdr.data[i] = '\000' THEN
        t := Text.FromChars(SUBARRAY(hdr.data, pos, i-pos));
        pos := i + 1;
        RETURN t;
      END;
    END;
    RETURN NIL;
  END ExtractText;

BEGIN
  ProtocolError := Atom.FromText("TCPNetObj.ProtocolError");
END HeaderOps.

