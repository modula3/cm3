(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu May 23 16:59:39 PDT 1996 by mcjones    *)

UNSAFE MODULE LecternServerPosix EXPORTS LecternServer;

FROM Ctypes IMPORT int;

IMPORT Atom, (*Cstring,*) FilePosix, FileRd, Fmt, FS, OSError,
   OSErrorPosix, Rd, SchedulerPosix, Text, Thread, Uerror, Unix,
   Usocket, Uugid, Word;

VAR
  mutex := NEW(MUTEX);
  initialized := FALSE;
  s: int;

(*
PROCEDURE GetHostname(): TEXT RAISES {OSError.E} =
  VAR str: ARRAY [0..31] OF CHAR;
  BEGIN
    IF Unix.gethostname(ADR(str), BYTESIZE(str)) < 0 THEN
      OSErrorPosix.Raise()
    END;
    RETURN Text.FromChars(SUBARRAY(str, 0, Cstring.strlen(ADR(str))))
  END GetHostname;
*)

PROCEDURE Initialize() RAISES {OSError.E} =
  VAR
    name: TEXT;
    addr: Usocket.struct_sockaddr_un;
    mode: int;
  BEGIN
    s := Usocket.socket(Usocket.AF_UNIX, Usocket.SOCK_STREAM, 0);
    IF s < 0 THEN OSErrorPosix.Raise() END;
    name := "/tmp/lectern"
      & Fmt.Int(Uugid.geteuid()) (* & "-" & GetHostname() *);
    TRY FS.DeleteFile(name) EXCEPT OSError.E => END;
    addr.sun_family := Usocket.AF_UNIX;
    Text.SetChars(
      LOOPHOLE(addr.sun_path, ARRAY [0..NUMBER(addr.sun_path)-1] OF CHAR),
      name);
    WITH n = Text.Length(name) DO
      addr.sun_path[n] := ORD('\000');
      IF Usocket.bind(
           s,
           LOOPHOLE(ADR(addr), UNTRACED REF Usocket.struct_sockaddr),
           BYTESIZE(addr.sun_family) + n) < 0 THEN
        OSErrorPosix.Raise()
      END
    END;
    (* *** Call chmod before bind? *)
    IF Unix.chmod(ADR(addr.sun_path[0]), Unix.MROWNER+Unix.MWOWNER) < 0 THEN
      OSErrorPosix.Raise()
    END;
    IF Usocket.listen(s, 5) < 0 THEN OSErrorPosix.Raise() END;
    mode := Word.Or(Unix.fcntl(s, Unix.F_GETFL, 0), Unix.O_NDELAY);
    IF Unix.fcntl(s, Unix.F_SETFL, mode) # 0 THEN
      OSErrorPosix.Raise()
    END;
    initialized := TRUE
  END Initialize;

EXCEPTION Retry;

PROCEDURE GetInt(rd: Rd.T)
  : INTEGER RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR r := 0;
  BEGIN
    FOR i := 0 TO 24 BY 8 DO
      r := Word.Or(r, Word.Shift(LOOPHOLE(Rd.GetChar(rd), [0..255]), i))
    END;
    RETURN r
  END GetInt;

PROCEDURE Accept(s: int): int RAISES {OSError.E} =
(* Block until a new connection request arrives on file descriptor "s",
   and return a file descriptor for it. *)
  VAR
    addr: Usocket.struct_sockaddr_un;
    addrlen: int;
    fd: int;
  BEGIN
    LOOP
      addr.sun_family := Usocket.AF_UNIX;
      addrlen := BYTESIZE(addr);
      fd := Usocket.accept(
	s,
	LOOPHOLE(ADR(addr), UNTRACED REF Usocket.struct_sockaddr), 
	ADR(addrlen));
      IF fd < 0
	AND Uerror.errno # Uerror.EWOULDBLOCK
	AND Uerror.errno # Uerror.EAGAIN THEN OSErrorPosix.Raise()
      ELSIF fd >= 0 THEN EXIT
      END;
      EVAL SchedulerPosix.IOWait(s, TRUE)
    END;
    RETURN fd
  END Accept;

PROCEDURE AwaitRequest(): REF ARRAY OF TEXT RAISES {Error} =
(* "Error" is only raised if something unrecoverable happens;
   otherwise "AwaitRequest" waits for another connection attempt. *)
  CONST
    MaxArgs = 50;
    MaxChars = 300;
  VAR
    fd: int;
    rd: Rd.T;
    nArgs, nChars: INTEGER;
    result: REF ARRAY OF TEXT;
  BEGIN
    TRY
      LOCK mutex DO
	IF NOT initialized THEN Initialize() END;

        (* Retry until a well-formed request is received. *)
        LOOP
          TRY
            fd := Accept(s);
	    rd := NEW(FileRd.T).init(FilePosix.NewPipe(fd, FilePosix.Read));
            TRY

	      nArgs := GetInt(rd);
              IF nArgs < 0 OR nArgs > MaxArgs THEN
                RAISE Error(
                  "Tell LecternClient implementor: nArgs out of range: "
                  & Fmt.Int(nArgs)
                  )
              END;
	      result := NEW(REF ARRAY OF TEXT, nArgs);

	      FOR i := 0 TO nArgs - 1 DO

		nChars := GetInt(rd);
                IF nChars < 0 OR nChars > MaxChars THEN
                  RAISE Error(
                    "Tell LecternClient implementor: nChars out of range: "
                    & Fmt.Int(nChars)
                    )
                END;
		result[i] := Rd.GetText(rd, nChars);
		IF Text.Length(result[i]) < nChars THEN RAISE Retry END;

	      END

            FINALLY EVAL Unix.close(fd)
            END;
            RETURN result
          EXCEPT Retry, Rd.EndOfFile, Rd.Failure, Thread.Alerted => (*SKIP*)
          END
        END

      END
    EXCEPT OSError.E(code) => RAISE Error(Atom.ToText(code.head))
    END
  END AwaitRequest;

BEGIN
END LecternServerPosix.
