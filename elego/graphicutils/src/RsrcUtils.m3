(*---------------------------------------------------------------------------*)
MODULE RsrcUtils;

IMPORT Env, Process, Rsrc, Rd, Stdio, Thread, Wr;
IMPORT System;

(*---------------------------------------------------------------------------*)
PROCEDURE ListResource(rpath : Rsrc.Path; name : TEXT) RAISES {Error} =
  VAR
    data : TEXT;
  BEGIN
    TRY
      data := Rsrc.Get(name, rpath);
      Wr.PutText(Stdio.stdout, data);
      Wr.Flush(Stdio.stdout);
    EXCEPT
      Rd.Failure     => RAISE Error("cannot read resource " & name);
    | Wr.Failure     => RAISE Error("cannot write resource " & name);
    | Rsrc.NotFound  => RAISE Error("resource " & name & " not found");
    | Thread.Alerted => RAISE Error("interrupted listing resource " & name);
    END;
  END ListResource;

(*---------------------------------------------------------------------------*)
PROCEDURE PipeResource(rpath : Rsrc.Path; name : TEXT; cmd : TEXT)
  RAISES {Error} =
  VAR
    data : TEXT;
    pid  : Process.T;
    wr   : Wr.T;
  BEGIN
    TRY
      data := Rsrc.Get(name, rpath);
      pid := System.PipeTo(cmd, wr);
      Wr.PutText(wr, data);
      Wr.Close(wr);
      EVAL Process.Wait(pid);
    EXCEPT
      Rd.Failure     => RAISE Error("cannot read resource " & name);
    | Wr.Failure     => RAISE Error("cannot write resource " & name);
    | Rsrc.NotFound  => RAISE Error("resource " & name & " not found");
    | Thread.Alerted => RAISE Error("interrupted listing resource " & name);
    | System.ExecuteError(e) => RAISE Error("execution failed: " & e);
    END;
  END PipeResource;

(*---------------------------------------------------------------------------*)
TYPE
  RsrcFilterProcess = Thread.Closure OBJECT
    data : TEXT;
    wr   : Wr.T;
    pid  : Process.T;
  METHODS
    init(data : TEXT; wr : Wr.T; pid : Process.T) : RsrcFilterProcess := 
        RsrcFilterThreadInit;
  OVERRIDES
    apply := RsrcFilterThread;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE RsrcFilterThreadInit(self : RsrcFilterProcess;
                               data : TEXT;
                               wr   : Wr.T;
                               pid  : Process.T;
                               ) :  RsrcFilterProcess =
  BEGIN
    self.data := data;
    self.wr := wr;
    self.pid := pid;
    RETURN self;
  END RsrcFilterThreadInit;

(*---------------------------------------------------------------------------*)
PROCEDURE RsrcFilterThread(self : RsrcFilterProcess) : REFANY = 
  BEGIN
    TRY Wr.PutText(self.wr, self.data) EXCEPT ELSE END;
    TRY Wr.Close(self.wr) EXCEPT ELSE END;
    EVAL Process.Wait(self.pid);
    RETURN NIL;
  END RsrcFilterThread;

(*---------------------------------------------------------------------------*)
PROCEDURE FilterResource(rpath : Rsrc.Path; name : TEXT; cmd : TEXT;
                         VAR rd : Rd.T) RAISES {Error} =
  VAR
    data : TEXT;
    pid  : Process.T;
    wr   : Wr.T;
  BEGIN
    TRY
      data := Rsrc.Get(name, rpath);
      pid := System.Filter(cmd, rd, wr);
      EVAL Thread.Fork(NEW(RsrcFilterProcess).init(data, wr, pid));
    EXCEPT
      Rd.Failure     => RAISE Error("cannot read resource " & name);
    | Rsrc.NotFound  => RAISE Error("resource " & name & " not found");
    | Thread.Alerted => RAISE Error("interrupted listing resource " & name);
    | System.ExecuteError(e) => RAISE Error("execution failed: " & e);
    END;
  END FilterResource;

(*---------------------------------------------------------------------------*)
PROCEDURE PageResource(rpath : Rsrc.Path; name : TEXT;
                       usePager := TRUE) RAISES {Error} =
  VAR pagerCmd := Env.Get("PAGER");
  BEGIN
    IF pagerCmd = NIL THEN
      pagerCmd := "more";
    END;
    IF usePager THEN
      PipeResource(rpath, name, pagerCmd);
    ELSE
      ListResource(rpath, name);
    END;
  END PageResource;

BEGIN
END RsrcUtils.
