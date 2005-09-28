MODULE CommandRd;

IMPORT Pipe, Process;
IMPORT File, FileRd, Rd, RdClass; (* close method of Rd.T *)
IMPORT OSError, Thread;


REVEAL
  T = FileRd.T BRANDED "CommandRd" OBJECT
        cmdProc: Process.T;
      OVERRIDES
        close := Close;
      END;

PROCEDURE Open
  (command: TEXT; READONLY args: ARRAY OF TEXT; stdinUser: File.T; ): T
  RAISES {OSError.E} =
  VAR
    rd                            := NEW(T);
    cmdOutPipe, meInPipe : Pipe.T;
    stdin, stdout, stderr: File.T;
  BEGIN
    Pipe.Open(meInPipe, cmdOutPipe);
    Process.GetStandardFileHandles(stdin, stdout, stderr);
    IF stdinUser # NIL THEN stdin := stdinUser; END;
    rd.cmdProc := Process.Create(command, args, stdin := stdin,
                                 stdout := cmdOutPipe, stderr := stderr);
    EVAL rd.init(meInPipe);
    (* The pipes must be closed to maintain the correct reference counts on
       the underlying channels. *)
    TRY
      cmdOutPipe.close();
    EXCEPT
      OSError.E =>               (*SKIP*)
    END;
    RETURN rd;
  END Open;

PROCEDURE Close (rd: T; ) RAISES {Rd.Failure, Thread.Alerted} =
  BEGIN
    TRY FileRd.T.close(rd); FINALLY EVAL Process.Wait(rd.cmdProc); END;
  END Close;

BEGIN
END CommandRd.
