(* Copyright (C) 1989, 1990, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Tue Feb 22 23:08:42 PST 1994 by kalsow   *)
(*      modified on Fri Jan  7 14:56:04 PST 1994 by msm      *)
(*      modified on Thu Oct 21 17:03:16 PDT 1993 by sfreeman *)

UNSAFE MODULE Jv;

IMPORT Atom, AtomList, Ctypes, FilePosix, FileRd, FileWr, M3toC,
       OSError, OSErrorPosix, Rd, RTMisc, Text, Thread, Uin, Usocket, Wr;

REVEAL
  T = Public BRANDED OBJECT
        rd: Rd.T;
        wr: Wr.T;
      OVERRIDES
        init  := Init;
        close := Close;
      END;

PROCEDURE Init (t: T; pipeName: TEXT): T RAISES {OSError.E} =
  (* open Unix domain connection to server. *)
  VAR
    unaddr: Usocket.struct_sockaddr_un;
    fd    : INTEGER;
    strlen := Text.Length(pipeName);
    addrlen: Usocket.socklen_t := BYTESIZE(unaddr.sun_family) + strlen;
  BEGIN
    unaddr.sun_family := Usocket.AF_UNIX;
    WITH string = M3toC.SharedTtoS(pipeName) DO
      RTMisc.Copy(
        string, ADR(unaddr.sun_path[0]), strlen + 1 (* +1 for '\0' *));
      M3toC.FreeSharedS(pipeName, string);
    END;

    fd := Usocket.socket(Usocket.AF_UNIX, Usocket.SOCK_STREAM, 0);
    IF fd < 0 THEN OSErrorPosix.Raise(); END;

    WITH addr = LOOPHOLE(ADR(unaddr), UNTRACED REF Uin.struct_sockaddr_in) DO
      IF Usocket.connect(fd, addr, addrlen) < 0 THEN
        OSErrorPosix.Raise();
      END;
    END;

    WITH file = FilePosix.NewPipe(fd, FilePosix.ReadWrite) DO
      t.rd := NEW(FileRd.T).init(file);
      t.wr := NEW(FileWr.T).init(file, FALSE);
    END;
    RETURN t;
  END Init;

PROCEDURE Close (t: T) =
  BEGIN
    TRY
      Wr.Close(t.wr);
      Rd.Close(t.rd);
    EXCEPT
    | Thread.Alerted, Wr.Failure, Rd.Failure => (* skip *)
    END;
  END Close;

(* -- send and receiving stuff, assume BYTESIZE(CHAR) = byte -- *)
TYPE
  LongArrayPtr =
    UNTRACED REF
      ARRAY [0 .. ((LAST(CARDINAL) DIV BITSIZE(CHAR)) - 1)] OF CHAR;
(* hack to get around type system.  The horrible formula for the length of
   the array gives the longest possible array *)

PROCEDURE Send (t: T; buf: ADDRESS; nbytes: CARDINAL)
  RAISES {OSError.E, Thread.Alerted} =
  BEGIN
    TRY
      WITH chars = LOOPHOLE(buf, LongArrayPtr) DO
        Wr.PutString(t.wr, SUBARRAY(chars^, 0, nbytes));
      END;
      Wr.Flush(t.wr);
    EXCEPT
    | Wr.Failure (e) =>
        RAISE OSError.E(AtomList.Cons(Atom.FromText("Write"), e));
    END;
  END Send;

PROCEDURE Recv (t: T; buf: ADDRESS; nbytes: CARDINAL)
  RAISES {OSError.E, Thread.Alerted} =
  VAR nchars: CARDINAL;
  BEGIN
    TRY
      WITH chars = LOOPHOLE(buf, LongArrayPtr) DO
        nchars := Rd.GetSub(t.rd, SUBARRAY(chars^, 0, nbytes));
      END;
      IF nchars < nbytes THEN
        RAISE
          OSError.E(AtomList.List2(ServerFailure, Atom.FromText("Read")));
      END;
    EXCEPT
    | Rd.Failure (e) =>
        RAISE OSError.E(AtomList.Cons(Atom.FromText("Read"), e));
    END;
  END Recv;

BEGIN
  <* ASSERT BYTESIZE(CHAR) = BYTESIZE(Ctypes.char) *>
  (* dumb consistency check *)
  ServerFailure := Atom.FromText("Jv ServerFailure");

END Jv.
