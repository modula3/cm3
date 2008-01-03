(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* OSUtils.m3                                                  *)
(* Last modified on Thu May 16 17:16:40 PDT 1996 by mcjones    *)
(*      modified on Mon Jun  6 17:53:51 PDT 1994 by birrell    *)

UNSAFE MODULE OSUtils EXPORTS OSUtils;

IMPORT File, FileRd, OSError, Pipe, Process, Rd, RdClass;

(* *)
(* Sub-processes *)
(* *)

TYPE MyRd = FileRd.T OBJECT
    stdout, stderr: Pipe.T;
    child: Process.T;
  OVERRIDES
    close := MyClose;
  END;

PROCEDURE MyClose(rd: MyRd) RAISES { Rd.Failure } =
  BEGIN
    TRY
      rd.stdout.close();
    EXCEPT OSError.E(code) => RAISE Rd.Failure(code)
    END;
    TRY
      rd.stderr.close();
    EXCEPT OSError.E(code) => RAISE Rd.Failure(code)
    END;
    EVAL Process.Wait(rd.child);
  END MyClose;

PROCEDURE RunFilter(READONLY argv: ARRAY OF TEXT; stdin: File.T;
                    VAR stdoutR: Pipe.T): Rd.T
                    RAISES {OSError.E} =
  VAR
    stdoutW, stderrR, stderrW: Pipe.T;
    res: Rd.T;
  BEGIN
    Pipe.Open(hr := stdoutR, hw := stdoutW);
    Pipe.Open(hr := stderrR, hw := stderrW);
    res := NEW(MyRd,
               stdout := stdoutR,
               stderr := stderrR,
               child := Process.Create(argv[0],
                                  SUBARRAY(argv, 1, NUMBER(argv)-1),
                                  NIL, NIL,
                                  stdin, stdoutW, stderrW)
               ).init(stdoutR);
    stdoutW.close();
    stderrW.close();
    RETURN res
  END RunFilter;

BEGIN
END OSUtils.
