(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu May 23 16:59:39 PDT 1996 by mcjones    *)

UNSAFE MODULE LecternServerPosix EXPORTS LecternServer;

FROM Ctypes IMPORT int;

IMPORT Atom, Cerrno, (*Cstring,*) Ctypes, FilePosix, FileRd, Fmt, FS, M3toC,
   OSError, OSErrorPosix, Rd, SchedulerPosix, Text, Thread, Uerror, Unix,
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
    mode: int;
    sun_path: Ctypes.char_star;
    err: INTEGER;
  BEGIN
    s := Usocket.socket(Usocket.AF_UNIX, Usocket.SOCK_STREAM, 0);
    IF s < 0 THEN OSErrorPosix.Raise() END;
    name := "/tmp/lectern"
      & Fmt.Int(Uugid.geteuid()) (* & "-" & GetHostname() *);
    TRY FS.DeleteFile(name) EXCEPT OSError.E => END;
    sun_path := M3toC.SharedTtoS(name);
    err := Usocket.bind_un(s, sun_path);
    IF err = 0 THEN
      (* *** Call chmod before bind? *)
      err := Unix.chmod(sun_path, Unix.MROWNER+Unix.MWOWNER);
    END;

    M3toC.FreeSharedS(name, sun_path);

    IF err < 0 THEN
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
    fd: int;
  BEGIN
    LOOP
      fd := Usocket.accept_un(s);
      IF fd < 0 THEN
        WITH errno = Cerrno.GetErrno() DO
          IF errno # Uerror.EWOULDBLOCK AND errno # Uerror.EAGAIN THEN
            OSErrorPosix.Raise()
          END
        END
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
