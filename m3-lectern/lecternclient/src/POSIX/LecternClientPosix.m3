(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(* Last modified on Thu May 23 16:59:27 PDT 1996 by mcjones        *)

(* Send a request to an instance of Lectern, starting one if necessary. *)

(* Inspired by emacsclient. *)

UNSAFE MODULE LecternClientPosix EXPORTS LecternClient;

FROM Ctypes IMPORT int;

IMPORT Atom, Cerrno, Ctypes, FilePosix, FileWr, Fmt, M3toC, OSError, OSErrorPosix, Process,
  Text, Thread, Uerror, Unix, Usocket, Ustat, Uugid, Word, Wr;

PROCEDURE PutInt(wr: Wr.T; n: INTEGER) RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    FOR i := 0 TO 24 BY 8 DO
      Wr.PutChar(wr, VAL(Word.Extract(n, i, 8), CHAR))
    END
  END PutInt;

PROCEDURE StartLectern() RAISES {OSError.E, Error} =
  BEGIN
    IF Process.Wait(Process.Create(
        "sh",
        ARRAY OF TEXT{"-c", "Lectern&"}
        (* , wd := $HOME ??? *)
        )) # 0 THEN
      RAISE Error("Couldn't start Lectern")
    END;
  END StartLectern;

CONST Retries = 6;

PROCEDURE Send(READONLY params: ARRAY OF TEXT) RAISES {Error} =
  VAR
    s: int := -1;
    sun_path: Ctypes.char_star := NIL;
    name: TEXT;
    statbuffer: Ustat.struct_stat;
    wr: Wr.T;
  BEGIN
    TRY
      s := Usocket.socket(Usocket.AF_UNIX, Usocket.SOCK_STREAM, 0);
      IF s < 0 THEN OSErrorPosix.Raise() END;
      TRY
        name := "/tmp/lectern" & Fmt.Int(Uugid.geteuid()) (* & "-" & GetHostname() *);
        sun_path := M3toC.SharedTtoS(name);

        FOR i := 1 TO Retries DO

          IF Ustat.stat(sun_path, ADR(statbuffer)) = 0 THEN
            IF statbuffer.st_uid # Uugid.geteuid() THEN
              OSErrorPosix.Raise0(Uerror.EACCES)
            END;
            IF Usocket.connect_un(s, sun_path) = 0 THEN
              EXIT (* connection to Lectern established *)
            ELSIF Cerrno.GetErrno() # Uerror.ECONNREFUSED THEN
              OSErrorPosix.Raise()
            END
          ELSIF Cerrno.GetErrno() # Uerror.ENOENT THEN
            OSErrorPosix.Raise()
          END;

          IF i = 1 THEN StartLectern() END;
          IF i = Retries THEN OSErrorPosix.Raise() END;
          Thread.Pause(3.0d0)

        END;

        wr := NEW(FileWr.T).init(FilePosix.NewPipe(s, FilePosix.Write));

        TRY
          PutInt(wr, NUMBER(params));
          FOR i := 0 TO LAST(params) DO
            PutInt(wr, Text.Length(params[i]));
            Wr.PutText(wr, params[i])
          END;
          Wr.Flush(wr)
        EXCEPT
          Wr.Failure, Thread.Alerted =>
        END;
        FINALLY
          M3toC.FreeSharedS(name, sun_path);
          EVAL Unix.close(s)
        END
      EXCEPT OSError.E(code) => RAISE Error(Atom.ToText(code.head))
      END
    END
  END Send;

BEGIN
END LecternClientPosix.
