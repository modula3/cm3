(* $Id: ProcUtils.m3,v 1.21 2011/02/17 21:07:02 mika Exp $ *)

MODULE ProcUtils;

(* wrapper to simplify starting and running of Unix processes under
   Modula-3.  Authors: Karl Papadantonakis, Mika Nystrom *)

IMPORT FS;
IMPORT File;
IMPORT FileRd;
IMPORT FileWr;
IMPORT Fmt; FROM Fmt IMPORT Int, F;
IMPORT Pathname;
IMPORT Pipe;
IMPORT Process;
IMPORT Rd;
IMPORT Stdio;
IMPORT Text;
IMPORT TextList;
IMPORT TextRd;
IMPORT Thread;
IMPORT Wr;
IMPORT OSError;
IMPORT Atom;
IMPORT AL;
IMPORT Debug;
IMPORT Time;
IMPORT Usignal;
IMPORT Wx;

<* FATAL Thread.Alerted *>

VAR DoDebug := Debug.DebugThis("PROCUTILS") OR Debug.DebugThis("ProcUtils");

TYPE
  PrivateCompletion = Completion OBJECT
    po, pe  : Writer;
    pi      : Reader;
    main    : MainClosure;
    th      : Thread.T;
  OVERRIDES
    wait        := Wait;
    abort       := Abort;
    getState    := GetState;
    getPID      := GetPID;
    waitTimeout := WaitTimeout;
  END;

  MainClosure = Thread.Closure OBJECT
    c       : PrivateCompletion;
    src     : Rd.T;
    wd0     : Pathname.T;
    mu      : MUTEX;
    cn      : Thread.Condition;

    env     : Env       := NIL;
    sub     : Process.T := NIL;
    created             := FALSE;

    state               := State.New;
    
  OVERRIDES
    apply := Apply;
  END;

  SSClosure = Thread.Closure OBJECT ss: SS; OVERRIDES apply:=SSApply; END;

VAR fileMu := NEW(MUTEX); (* issues with file descriptors ? *)
    
PROCEDURE GetPID(pc : PrivateCompletion) : Process.ID =
  BEGIN
    IF pc.main = NIL OR pc.main.sub = NIL THEN
      RETURN 0
    ELSE
      RETURN Process.GetID(pc.main.sub)
    END
  END GetPID;
  
PROCEDURE Abort(pc : PrivateCompletion) =
  VAR
    id : Process.ID;
  BEGIN
    Debug.Out("ProcUtils.Abort");
    LOCK pc.main.mu DO
      IF pc.main.sub # NIL THEN
        id := Process.GetID(pc.main.sub);
        Debug.Out("ProcUtils.Abort id=" & Int(id))
      ELSE
        Debug.Out("ProcUtils.Abort pc.main.sub was NIL");
        RETURN
      END
    END;


    IF DoDebug THEN
      Debug.Out(F("Abort killing sub: HUP (%s,%s)",
                  Int(id), Int(Usignal.SIGHUP)))
    END;
    EVAL Usignal.kill(id, Usignal.SIGHUP); (* HangUP *)

    Thread.Pause(1.0d0);

    IF DoDebug THEN
      Debug.Out(F("Abort killing sub: INT (%s,%s)",
                  Int(id), Int(Usignal.SIGINT)))
    END;
    EVAL Usignal.kill(id, Usignal.SIGINT); (* INTerrupt *)

    Thread.Pause(1.0d0);

    IF DoDebug THEN
      Debug.Out(F("Abort killing sub: TERM (%s,%s)",
                  Int(id), Int(Usignal.SIGTERM)))
    END;
    EVAL Usignal.kill(id, Usignal.SIGTERM); (* TERMinate *)
    
    Thread.Pause(1.0d0);

    IF DoDebug THEN
      Debug.Out(F("Abort killing sub: KILL (%s,%s)",
                  Int(id), Int(Usignal.SIGKILL)))
    END;
    EVAL Usignal.kill(id, Usignal.SIGKILL); (* KILL *)
  END Abort;

PROCEDURE GetState(pc : PrivateCompletion) : State =
  BEGIN
    LOCK pc.main.mu DO
      RETURN pc.main.state
    END
  END GetState;
  
PROCEDURE SSApply(self: SSClosure): REFANY =
  BEGIN
    IF DoDebug THEN
      Debug.Out("SSApply")
    END;
    TRY
      LOOP
        Wr.PutChar(self.ss.wr,Rd.GetChar(self.ss.rd));
        Wr.PutText(self.ss.wr,Rd.GetText(self.ss.rd,
                                         Rd.CharsReady(self.ss.rd)));
      END;
    EXCEPT 
      Rd.EndOfFile => 
      IF DoDebug THEN Debug.Out("SSApply exiting on Rd.EndOfFile") END;
      LOCK fileMu DO
        TRY Rd.Close(self.ss.rd) EXCEPT ELSE END;
      END;
      RETURN NIL
    |
      Rd.Failure, Wr.Failure => 
         Process.Crash("I/O Error in ProcUtils.SSApply.")
    END;
    <* ASSERT FALSE *>
  END SSApply;

PROCEDURE ForkWriter(w: Writer): Writer =
  BEGIN
    IF DoDebug THEN Debug.Out("ForkWriter") END;
    IF w # NIL AND w.ss # NIL THEN
      w.th := Thread.Fork(NEW(SSClosure, ss:=w.ss, apply := SSApply));
      IF DoDebug THEN Debug.Out("ForkWriter w.th = 16_" & Debug.FmtPointer(w.th)) END;
    END;
    RETURN w;
  END ForkWriter;

PROCEDURE ForkReader(r: Reader): Reader =
  BEGIN
    IF r # NIL AND r.ss # NIL THEN
      EVAL Thread.Fork(NEW(SSClosure, ss:=r.ss, apply := SSApply));
    END;
    RETURN r;
  END ForkReader;

PROCEDURE RunText(source        : TEXT;
                  stdout,stderr : Writer;
                  stdin         : Reader;
                  wd0           : Pathname.T;
                  env           : Env): Completion =
  BEGIN RETURN Run(TextRd.New(source),stdout,stderr,stdin,wd0,env) END RunText;

PROCEDURE Run(source        : Rd.T;
              stdout,stderr : Writer     := NIL;
              stdin         : Reader     := NIL;
              wd0           : Pathname.T := NIL;
              env           : Env        := NIL): Completion =
  VAR
    c := NEW(PrivateCompletion,
             po := ForkWriter(stdout),
             pe := ForkWriter(stderr),
             pi := ForkReader(stdin),
             main := NEW(MainClosure, 
                         src := source, 
                         wd0 := wd0,
                         env := env,
                         mu  := NEW(MUTEX), 
                         cn  := NEW(Thread.Condition)));
  BEGIN
    c.main.c := c;
    c.th := Thread.Fork(c.main);
    IF DoDebug THEN Debug.Out("Run : c.th = 16_" & Debug.FmtPointer(c.th)) END;

    LOCK c.main.mu DO
      (* if we dont wait for the process to be created here we get a very 
         nasty race condition!  The Pipe will have zero writers before this
         point.  That means that it will immediately hit EOF if it is read
         from!  So be sure the pipe has a writer before it is passed back to
         the calling process! *)
      WHILE NOT c.main.created DO
        Thread.Wait(c.main.mu, c.main.cn)
      END
    END;

    IF DoDebug THEN
      Debug.Out(F("Run : process PID %s has been created, continuing...",
                  Int(c.getPID())))
    END;
    
    RETURN c;
  END Run;

PROCEDURE Apply(self: MainClosure): REFANY =
  CONST
    White = SET OF CHAR{' ','\t'};
    Break = SET OF CHAR{'\n',';'};
    Break1 = SET OF CHAR{'\n',';','|'};
    WB = White + Break;
    Special = SET OF CHAR{'|','&','<','>'} + WB;
    ReDir = SET OF CHAR{'<','>'};
  VAR
    wd := self.wd0;
    cm := self.c;
    rd := self.src;
    env:= self.env;
    c: CHAR;
    p: TEXT;
    i2, stdout,stderr,stdin: File.T := NIL;
    rMode: CHAR;
    l: TextList.T;

  PROCEDURE PutArg() RAISES { OSError.E } =
    BEGIN
        CASE rMode OF
        | '>' => stdout := FS.OpenFile(p);
        | '&' => stdout := FS.OpenFile(p); stderr := stdout;
        | '<' => stdin := FS.OpenFileReadonly(p);
        ELSE
          l := TextList.Cons(p, l);
        END
    END PutArg;

  PROCEDURE Exec() RAISES { Rd.Failure, OSError.E, ErrorExit } =
    BEGIN
      IF c = '|' THEN
        TRY
          c := Rd.GetChar(rd);
          WHILE c IN White DO c := Rd.GetChar(rd); END;
          LOCK fileMu DO
            VAR
              r,w: Pipe.T;
            BEGIN
              Pipe.Open(r,w);
              i2 := r;
              stdout := w;
            END;
          END;
          IF c = '&' THEN
            LOCK fileMu DO
              c := Rd.GetChar(rd);
              stderr := stdout;
            END
          END
        EXCEPT Rd.EndOfFile => <* ASSERT FALSE *>
        END;
      ELSE
        IF cm.pi = NIL THEN i2:=NIL ELSE i2:=cm.pi.f; END;
      END;
      VAR
        params := NEW(REF ARRAY OF TEXT, TextList.Length(l)-1);
      BEGIN
        FOR i := LAST(params^) TO 0 BY -1 DO
          params[i] := l.head;
          l := l.tail;
        END;
        IF Text.Equal(l.head, "cd") THEN
          wd := Pathname.Join(wd, l.head);
        ELSE
          TRY
            IF DoDebug THEN
              Debug.Out("ProcUtils.Apply.Exec: running command: " & l.head);
              FOR i := FIRST(params^) TO LAST(params^) DO
                Debug.Out("ProcUtils.Apply.Exec: params[" & Fmt.Int(i) & "] : " & 
                  params[i])
              END
            END;

            LOCK self.mu DO
              self.state := State.Starting
            END;
            
            VAR
              code : Process.ExitCode;
              sub  : Process.T;
            BEGIN
              IF DoDebug THEN
                VAR
                  wx := Wx.New();
                BEGIN
                  Wx.PutText(wx, l.head);
                  FOR i := FIRST(params^) TO LAST(params^) DO
                    Wx.PutChar(wx, ' ');
                    Wx.PutText(wx, params[i])
                  END;
                  Debug.Out("ProcUtils.Apply.Exec: creating subprocess \"" & Wx.ToText(wx) & "\"")
                END
              END;
              TRY 
                sub := Process.Create(l.head,
                                      params^,
                                      env,
                                      wd,
                                      stdin,
                                      stdout,
                                      stderr);
                LOCK self.mu DO
                  self.state := State.Created
                END
              EXCEPT
                OSError.E(x) =>
                  Debug.Out("Got exception OSError.E in Process.Create(" & l.head & "...) : " & AL.Format(x));
                  RAISE OSError.E(x)
              END;
              IF DoDebug THEN
                Debug.Out("ProcUtils.Apply.Exec: created subprocess PID=" & Int(Process.GetID(sub)))
              END;

              LOCK self.mu DO
                (* mark process as created so that requester can continue 
                   
                   This routine is NOT allowed to continue until the Pipe
                   between the subprocess and the calling process has been
                   opened for writing by the subprocess, which happens
                   in Process.Create.
                *)
                self.sub := sub;
                self.created := TRUE;
                Thread.Signal(self.cn)
              END;

              IF DoDebug THEN
                Debug.Out("ProcUtils.Apply.Exec: waiting for subprocess")
              END;

              LOCK self.mu DO
                self.state := State.Running
              END;
              
              code := Process.Wait(sub);

              LOCK self.mu DO
                self.state := State.CleaningUp
              END;

              IF DoDebug THEN
                Debug.Out("ProcUtils.Apply.Exec: subprocess returned " & 
                  Fmt.Int(code))
              END;

              (* we need to attempt closing all three of stdin, stdout,
                 and stderr.  Only the LAST exception, if any, will be
                 reported. (right now none are reported) *)
              (* move the close to the caller ... because we need to check
                 *.close *)
              (*
              IF DoDebug THEN Debug.Out("Closing descriptors") END;

              IF stdin  # NIL THEN TRY stdin .close() EXCEPT ELSE 
                IF DoDebug THEN Debug.Out("cant close stdin") END
              END END;

              IF stdout # NIL THEN TRY stdout.close() EXCEPT ELSE 
                IF DoDebug THEN Debug.Out("cant close stdout") END
              END END;

              IF stderr # NIL THEN TRY stderr.close() EXCEPT ELSE 
                IF DoDebug THEN Debug.Out("cant close stderr") END
              END END;
              *)

              IF DoDebug THEN Debug.Out("Descriptors closed") END;

              IF code # 0 THEN
                IF DoDebug THEN Debug.Out("Process exited with code " & 
                  Fmt.Int(code)) 
                END;
                RAISE ErrorExit(NEW(ExitCode, code := code))
              END;

              IF DoDebug THEN Debug.Out("Exec done") END;
            END
          EXCEPT
            OSError.E(e) => 
              RAISE ErrorExit(NEW(OS, al := e))
          END
        END
      END
    END Exec;

  <*FATAL Timeout*>
  BEGIN
    IF wd = NIL THEN wd := "."; END;

    LOCK self.mu DO self.state := State.Parsing END;
    
    TRY
      TRY
      TRY
        (* default input *)
        IF cm.pi = NIL THEN i2:=NIL ELSE i2:=cm.pi.f; END;
        c := Rd.GetChar(rd);
        (* loop invariant:
           c is the character just before
           the mark,
           and everything before that has been processed and then added to p
        *)
        LOOP
          (* set up default i/o *)
          stdin := i2;
          IF cm.po # NIL THEN stdout := cm.po.f; END;
          IF cm.pe # NIL THEN stderr := cm.pe.f; END;
          l := NIL;
          WHILE c IN WB DO c := Rd.GetChar(rd); END;
          REPEAT
            IF c IN ReDir THEN
              rMode := c;
              IF c = '>' THEN
                WHILE c IN White DO c := Rd.GetChar(rd); END;
                IF c = '&' THEN
                  c := Rd.GetChar(rd);
                  rMode := '&';
                END;
              END;
            ELSE
              rMode := '-';
            END;
            p := "";
            CASE c OF
            | '\'' =>
              c := Rd.GetChar(rd);
              WHILE c # '\'' DO p:=p&Fmt.Char(c); c := Rd.GetChar(rd) END;
              c := Rd.GetChar(rd) (* to maintain loop invariant *)
            | '`' =>
              c := Rd.GetChar(rd);
              WHILE c # '`' DO p:=p&Fmt.Char(c); c := Rd.GetChar(rd) END;
              c := Rd.GetChar(rd); (* to maintain loop invariant *)
              p := ToText(p, wd0:=wd);
            ELSE
              WHILE NOT c IN Special DO p:=p&Fmt.Char(c); c:=Rd.GetChar(rd); END;
            END;
            WHILE c IN White DO c := Rd.GetChar(rd); END;
            PutArg();
          UNTIL c IN Break1;
          IF DoDebug THEN Debug.Out("Calling Exec") END;

          
          Exec();

          IF DoDebug THEN Debug.Out("Exec Returned") END;
        END
      EXCEPT Rd.EndOfFile =>
        IF DoDebug THEN
          Debug.Out("ProcUtils.Apply : short read, putting semicolon")
        END;
        PutArg();
        c := ';';
        Exec();
      END;
      
      FINALLY
        (* close i/o *)
        IF DoDebug THEN 
          Debug.Out("ProcUtils.Apply: finally clause : close i/o");
        END;

        LOCK fileMu DO
        IF DoDebug THEN Debug.Out("ProcUtils.Apply: closing cm.po") END;

        IF cm.po#NIL AND cm.po.close THEN 
          IF stdout # NIL THEN TRY stdout.close() EXCEPT ELSE 
            IF DoDebug THEN Debug.Out("cant close stdout") END
          END END;
          
          TRY cm.po.f.close() EXCEPT ELSE
            IF DoDebug THEN Debug.Out("cant close cm.po.f") END
          END; 
          (*
          IF cm.po.aux # NIL THEN
            TRY cm.po.aux.close() EXCEPT ELSE END 
          END
          *)
        END;

        IF DoDebug THEN Debug.Out("ProcUtils.Apply: closing cm.pe") END;

        IF cm.pe#NIL AND cm.pe.close AND cm.po#cm.pe THEN 
          IF stderr # NIL THEN TRY stderr.close() EXCEPT ELSE 
            IF DoDebug THEN Debug.Out("cant close stderr") END
          END END;
          
          TRY cm.pe.f.close() EXCEPT ELSE
            IF DoDebug THEN Debug.Out("cant close cm.pe.f") END
          END;
          (*
          IF cm.pe.aux # NIL THEN
            TRY cm.pe.aux.close() EXCEPT ELSE END 
          END
          *)
        END;

        IF DoDebug THEN Debug.Out("ProcUtils.Apply: closing cm.pi") END;

        IF cm.pi#NIL AND cm.pi.close THEN 
          IF stdin  # NIL THEN TRY stdin .close() EXCEPT ELSE 
            IF DoDebug THEN Debug.Out("cant close stdin") END
          END END;
          
          TRY cm.pi.f.close() EXCEPT ELSE
            IF DoDebug THEN Debug.Out("cant close cm.pi.f") END
          END;
          (*
          IF cm.pi.aux # NIL THEN
            TRY cm.pi.aux.close() EXCEPT ELSE END 
          END
          *)
        END;

        END
      END;
      LOCK self.mu DO
        self.state := State.Done
      END;
      RETURN NIL;
    EXCEPT
      ErrorExit(ee) =>
      LOCK self.mu DO
        self.state := State.ErrorExit
      END;
      RETURN ee
    |
      OSError.E(e) => 
      LOCK self.mu DO
        self.state := State.OsErrorExit
      END;
      RETURN NEW(OS, error := FormatOSError(e))
    |
      Rd.Failure(e) =>
      LOCK self.mu DO
        self.state := State.RdFailureExit
      END;

      RETURN NEW(Error, error := FormatOSError(e))
    END
  END Apply;

PROCEDURE Wait(c: PrivateCompletion) RAISES { ErrorExit } =
  VAR
    r : REFANY;
  BEGIN
    IF DoDebug THEN Debug.Out("c.th = 16_" & Debug.FmtPointer(c.th)) END;
    r := Thread.Join(c.th);
    IF DoDebug THEN Debug.Out("Wait") END;
    IF c.po # NIL AND c.po.th # NIL THEN EVAL Thread.Join(c.po.th) END; 
    IF c.pe # NIL AND c.pe.th # NIL THEN EVAL Thread.Join(c.pe.th) END;

    (* we don't need to Join std input, that's the child's problem *)

    IF r # NIL AND ISTYPE(r,Error) THEN 
      IF DoDebug THEN
        Debug.Out("ProcUtils.Wait: Raising ErrorExit: " & FormatError(r))
      END;
      RAISE ErrorExit(r) 
    END

  END Wait;

  (* *)

TYPE
  Waiter = OBJECT
    mu      : MUTEX;
    cond    : Thread.Condition;
    done    : BOOLEAN;
    expired : BOOLEAN;
    error   : Error;
  END;

  WaitCl = Thread.Closure OBJECT
    w : Waiter;
    c : PrivateCompletion;
  OVERRIDES
    apply := WaitApply;
  END;

  TimeCl = Thread.Closure OBJECT
    w     : Waiter;
    timeo : LONGREAL;
  OVERRIDES
    apply := TimeApply;
  END;

PROCEDURE WaitApply(cl : WaitCl) : REFANY =
  BEGIN
    TRY
      cl.c.wait();
      LOCK cl.w.mu DO
        cl.w.done := TRUE;
        Thread.Signal(cl.w.cond)
      END
    EXCEPT
      ErrorExit(x) =>
      LOCK cl.w.mu DO
        cl.w.error := x;
        Thread.Signal(cl.w.cond)
      END
    END;
    RETURN NIL
  END WaitApply;

PROCEDURE TimeApply(cl : TimeCl) : REFANY =
  BEGIN
    Thread.Pause(cl.timeo);
    LOCK cl.w.mu DO
      cl.w.expired := TRUE;
      Thread.Signal(cl.w.cond)
    END;
    RETURN NIL
  END TimeApply;
  
PROCEDURE WaitTimeout(c : PrivateCompletion; timeo : LONGREAL)
  RAISES { ErrorExit, Timeout } =
  VAR
    w   := NEW (Waiter,
                mu      := NEW(MUTEX),
                cond    := NEW(Thread.Condition),
                done    := FALSE,
                expired := FALSE,
                error   := NIL );
    wcl := NEW(WaitCl, w := w, c     := c);
    tcl := NEW(TimeCl, w := w, timeo := timeo);
  BEGIN
    EVAL Thread.Fork(wcl);
    EVAL Thread.Fork(tcl);
    LOCK w.mu DO
      WHILE NOT w.done AND NOT w.expired AND w.error = NIL DO
        Thread.Wait(w.mu, w.cond)
      END;
      IF    w.error # NIL THEN
        RAISE ErrorExit(w.error)
      ELSIF w.done THEN
        RETURN
      ELSE
        <*ASSERT w.expired*>
        c.abort();
        RAISE Timeout
      END
    END
  END WaitTimeout;
  
(* Helpers *)

PROCEDURE ToText(source: T;
                 stderr:  Writer := NIL;
                 stdin: Reader := NIL;
                 wd0: Pathname.T := NIL;
                 timeout := LAST(Time.T)): TEXT
  RAISES { Rd.Failure, ErrorExit, OSError.E, Timeout } =
  VAR
    rd: Rd.T;
    srcRd := TextRd.New(source);
    comp : PrivateCompletion;
    res : TEXT;
    watchdog : Thread.T := NIL;
    alerted := FALSE;
  BEGIN
    comp := RdToRd(srcRd, stderr, stdin, wd0, rd);
    IF DoDebug THEN Debug.Out("Reading out rd") END;

    (* the sequence below is curious.

       first, we read from the output of the child process.  RdToRd guarantees 
       there is a writer to the pipe.  Since we read to LAST(INTEGER), the
       subprocess has to close before we can return to this procedure.

       THEN, we wait for the subprocess to exit and its puppeteering thread
       to exit.

       Finally we close the reader.  If something went wrong and the sequence
       happened incorrectly, the writer will deadlock before this since we
       did not read from it! 
    *)

    (* start watchdog if requested *)
    IF timeout # LAST(Time.T) THEN
      WITH et = Time.Now() + timeout DO
        watchdog := Thread.Fork(NEW(WatchdogCl,
                                    killSub := comp.main.sub,
                                    alertTh := Thread.Self(),
                                    alertAt := et))
      END
    END;

    TRY
      IF DoDebug THEN Debug.Out("ProcUtils.ToText: Calling Rd.GetText") END;

      (* this doesnt seem to work as expected: even if this thread 
         is alerted here, Rd.GetText doesn't return! *)
      
      res := Rd.GetText(rd, LAST(INTEGER));
      IF Thread.TestAlert() THEN
        alerted := TRUE
      END;
      IF DoDebug THEN Debug.Out("ProcUtils.ToText: Rd.GetText returned alerted=" & Fmt.Bool(alerted)) END
    EXCEPT
      Thread.Alerted =>
      IF DoDebug THEN Debug.Out("ProcUtils.ToText: Rd.GetText alerted") END;
      alerted := TRUE
    END;

    (* kill the watchdog *)
    IF watchdog # NIL THEN
      Thread.Alert(watchdog);
      EVAL Thread.Join(watchdog);
      IF DoDebug THEN Debug.Out("ProcUtils.ToText: Killed watchdog") END
    END;
    
    IF DoDebug THEN Debug.Out("ProcUtils.ToText: Calling comp.wait()") END;
    comp.wait();
    (* because of how the watchdog works...(it kills the subprocess
       and attempts to alert us) 
       if we get a ProcUtils.ErrorExit here, maybe we should catch it,
       check whether TestAlert is true.  If so, raise Timeout.
       Else re-raise ErrorExit *)
       
    
    IF DoDebug THEN Debug.Out("ProcUtils.ToText: comp.wait() done") END;
    TRY Rd.Close(rd) EXCEPT ELSE END;
    IF DoDebug THEN Debug.Out("ProcUtils.ToText: Rd.Close done") END;
    IF alerted THEN
      RAISE Timeout
    ELSE
      RETURN res;
    END
  END ToText;

TYPE
  WatchdogCl = Thread.Closure OBJECT
    alertTh : Thread.T;
    alertAt : Time.T;
    killSub : Process.T;
  OVERRIDES
    apply := WDApply;
  END;

PROCEDURE WDApply(cl : WatchdogCl) : REFANY =
  BEGIN
    IF DoDebug THEN Debug.Out("Watchdog started") END;
    TRY
      LOOP
        WITH now = Time.Now() DO
          IF DoDebug THEN Debug.Out(Fmt.F("Watchdog now %s deadline %s wait %s",
                                      Fmt.LongReal(now),
                                      Fmt.LongReal(cl.alertAt),
                                      Fmt.LongReal(cl.alertAt-now))) END;
          IF now < cl.alertAt THEN
            Thread.AlertPause(cl.alertAt - now)
          ELSE
            IF DoDebug THEN Debug.Out("Watchdog alerting") END;
            Thread.Alert(cl.alertTh);

            WITH subId = Process.GetID(cl.killSub) DO
              IF DoDebug THEN
                Debug.Out(F("Watchdog killing sub: INT (%s,%s)",
                            Int(subId), Int(Usignal.SIGINT)))
              END;
              EVAL Usignal.kill(subId, Usignal.SIGINT);
              
              Thread.Pause(1.0d0);
              
              IF DoDebug THEN
                Debug.Out(F("Watchdog killing sub:  (%s,%s)",
                            Int(subId), Int(Usignal.SIGKILL)))
              END;
              EVAL Usignal.kill(subId, Usignal.SIGKILL);
              
            END;

            RETURN NIL
          END
        END
      END
    EXCEPT
      Thread.Alerted => RETURN NIL
    END
  END WDApply;

PROCEDURE RdToRd(source: Rd.T;
                 stderr: Writer := NIL;
                 stdin: Reader := NIL;
                 wd0: Pathname.T := NIL;
                 VAR rd: Rd.T): Completion RAISES { OSError.E } =
  VAR
    myWriter := GimmeRd(rd);
  BEGIN
    IF DoDebug THEN Debug.Out("calling Run") END;
    WITH cpl = Run(source, myWriter, stderr, stdin, wd0) DO
      IF DoDebug THEN Debug.Out("Run returned") END;
      (*TRY myWriter.f.close(); EXCEPT ELSE END;*)
      RETURN cpl
    END
  END RdToRd;



(* I/O control *)

REVEAL
  Reader = BRANDED "ProcUtilRd" OBJECT 
    f, aux: File.T := NIL; close: BOOLEAN; ss: SS; END;
  Writer = BRANDED "ProcUtilWr" OBJECT 
    f, aux: File.T := NIL; close: BOOLEAN; ss: SS; th : Thread.T END;

TYPE
  SS = OBJECT rd: Rd.T; wr: Wr.T; END;

PROCEDURE WriteHere(wr: Wr.T): Writer RAISES { OSError.E } =
  BEGIN
    IF wr = Stdio.stdout THEN RETURN Stdout();
    ELSIF wr = Stdio.stderr THEN RETURN Stderr();
    ELSE
      VAR
        rd: Rd.T;
        w := GimmeRd(rd);
      BEGIN
        w.ss := NEW(SS,rd:=rd,wr:=wr);
        RETURN w;
      END;
    END;
  END WriteHere;

PROCEDURE GimmeRd(VAR rd: Rd.T): Writer RAISES { OSError.E } =
  VAR
    hr,hw: Pipe.T;
  BEGIN
    LOCK fileMu DO
      Pipe.Open(hr, hw);
      rd := NEW(FileRd.T).init(hr);
    END;
    RETURN NEW(Writer,f:=hw,ss:=NIL,aux := hr,close:=TRUE);
  END GimmeRd;

PROCEDURE Stdout(): Writer =
  BEGIN
    RETURN NEW(Writer,f:=so,ss:=NIL,close:=FALSE);
  END Stdout;

PROCEDURE Stderr(): Writer =
  BEGIN
    RETURN NEW(Writer,f:=se,ss:=NIL,close:=FALSE);
  END Stderr; 

PROCEDURE ReadHere(rd: Rd.T): Reader  RAISES { OSError.E } =
  BEGIN
    IF rd = Stdio.stdin THEN RETURN Stdin();
    ELSE
      VAR
        wr: Wr.T;
        r := GimmeWr(wr);
      BEGIN
        r.ss := NEW(SS,rd:=rd,wr:=wr);
        RETURN r;
      END;
    END;
  END ReadHere;

PROCEDURE ReadThis(t: TEXT): Reader  RAISES { OSError.E } =
  BEGIN
    RETURN ReadHere(TextRd.New(t));
  END ReadThis;

PROCEDURE GimmeWr(VAR wr: Wr.T): Reader RAISES { OSError.E } =
  VAR
    hr,hw: Pipe.T;
  BEGIN
    LOCK fileMu DO
      Pipe.Open(hr, hw);
      wr := NEW(FileWr.T).init(hw);
    END;
    RETURN NEW(Reader,f:=hr,ss:=NIL,aux:=hw,close:=TRUE);
  END GimmeWr;

PROCEDURE Stdin(): Reader =
  BEGIN
    RETURN NEW(Reader,f:=si,ss:=NIL,close:=FALSE)
  END Stdin; 

PROCEDURE FormatOSError(e : OSError.Code) : TEXT =
  VAR
    res := "";
  BEGIN
    WHILE e # NIL DO
      res := res & Atom.ToText(e.head);
      IF e.tail # NIL THEN res := res & " " END;
      e := e.tail
    END;
    RETURN res
  END FormatOSError;

PROCEDURE FormatError(e : Error) : TEXT =
  BEGIN
    TYPECASE e OF
      OS(os) => RETURN "ProcUtils.Error.OS: " & UnNil(e.error) & "; " & AL.Format(os.al)
    |
      ExitCode(ec) => RETURN "ProcUtils.Error.ExitCode: " & UnNil(e.error) & 
        " exitCode=" & Fmt.Int(ec.code)
    |
      Error => RETURN "ProcUtils.Error.Unknown: " & UnNil(e.error)
    END
  END FormatError;

PROCEDURE UnNil(txt : TEXT) : TEXT = 
  BEGIN IF txt = NIL THEN RETURN "**NIL**" ELSE RETURN txt END END UnNil;

VAR
  so,si,se: File.T;
BEGIN
  Process.GetStandardFileHandles(si,so,se);
END ProcUtils.
