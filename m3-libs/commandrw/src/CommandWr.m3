MODULE CommandWr;

IMPORT Pipe, Process;
IMPORT File, FileWr, Wr, WrClass; (* close method of Wr.T *)
IMPORT OSError, Pathname, Thread;


REVEAL
  T = FileWr.T BRANDED "CommandWr" OBJECT
        cmdProc: Process.T;
      OVERRIDES
        close := Close;
      END;

PROCEDURE Open (         command   : Pathname.T;
                READONLY args      : ARRAY OF TEXT;
                         stdoutUser: File.T;        ): T
  RAISES {OSError.E} =
  VAR
    wr                            := NEW(T);
    meOutPipe, cmdInPipe : Pipe.T;
    stdin, stdout, stderr: File.T;
  BEGIN
    Pipe.Open(cmdInPipe, meOutPipe);
    Process.GetStandardFileHandles(stdin, stdout, stderr);
    IF stdoutUser # NIL THEN stdout := stdoutUser; END;
    wr.cmdProc := Process.Create(command, args, stdin := cmdInPipe,
                                 stdout := stdout, stderr := stderr);
    EVAL wr.init(meOutPipe);
    (* The pipes must be closed to maintain the correct reference counts on
       the underlying channels. *)
    TRY
      cmdInPipe.close();
    EXCEPT
      OSError.E =>               (* SKIP *)
    END;
    RETURN wr;
  END Open;

PROCEDURE Close (wr: T; ) RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    TRY FileWr.T.close(wr); FINALLY EVAL Process.Wait(wr.cmdProc); END;
  END Close;

BEGIN
END CommandWr.
