MODULE GZipRd;


IMPORT Pipe, Process, FS, Pathname, IO;
IMPORT FileRd, Rd, RdClass, (* close method of Rd.T *) Text;
IMPORT FileWr, Wr, Fmt;
IMPORT OSError, AtomList, Thread;
IMPORT LongRealTrans AS RT;


REVEAL
  T = FileRd.T BRANDED "GZipRd.T" OBJECT
        gzipOutPipe, meInPipe: Pipe.T;
        gzipProc             : Process.T;
      OVERRIDES
        close := Close;
      END;

PROCEDURE Open (p: Pathname.T; ): T RAISES {OSError.E} =
  VAR rd := NEW(T);
  BEGIN
    Pipe.Open(rd.meInPipe, rd.gzipOutPipe);
    rd.gzipProc := Process.Create("gunzip", ARRAY OF TEXT{"--stdout", p},
                                  stdout := rd.gzipOutPipe);
    EVAL rd.init(rd.meInPipe);
    (* The pipes must be closed to maintain the correct reference counts on
       the underlying channels. *)
    TRY
      rd.gzipOutPipe.close();
    EXCEPT
      OSError.E =>               (*SKIP*)
    END;
    RETURN rd;
  END Open;

PROCEDURE Close (rd: T; ) RAISES {Rd.Failure, Thread.Alerted} =
  VAR
    failure, alerted             := FALSE;
    error           : AtomList.T;
  BEGIN
    TRY
      FileRd.T.close(rd);
    EXCEPT
    | Rd.Failure (code) => failure := TRUE; error := code;
    | Thread.Alerted => alerted := TRUE;
    END;

    EVAL Process.Wait(rd.gzipProc);
    IF failure THEN RAISE Rd.Failure(error); END;
    IF alerted THEN RAISE Thread.Alerted; END;
  END Close;

BEGIN
END GZipRd.
