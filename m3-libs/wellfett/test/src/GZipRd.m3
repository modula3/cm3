MODULE GZipRd;


IMPORT Pipe, Process, Pathname;
IMPORT FileRd, Rd, RdClass;      (* close method of Rd.T *)
IMPORT OSError, Thread;


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
  BEGIN
    TRY FileRd.T.close(rd); FINALLY EVAL Process.Wait(rd.gzipProc); END;
  END Close;

BEGIN
END GZipRd.
