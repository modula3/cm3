(* $Id$ *)

MODULE SchemeCommandRunner;
IMPORT Scheme;
FROM Scheme IMPORT E, Object;
IMPORT ProcUtils, TextWr, SchemeProcedure;
IMPORT SchemePrimitive, SchemePair, SchemeUtils, Wr, SchemeLongReal;
IMPORT Debug;
IMPORT TextRd;
IMPORT XTime AS Time;
IMPORT Thread;
IMPORT SchemeString;
IMPORT Rd;

EXCEPTION Timeout(TEXT);

(* Why doesn't the version with a SchemeInputPort as result work? *)
   
PROCEDURE RunCommandApply(proc : Procedure; 
                          interp : Scheme.T; 
                          args : Object) : Object RAISES { E } =
  BEGIN
    RETURN RunTimeoutCommandApply(proc,
                                  interp,
                                  NEW(SchemePair.T,
                                      first := SchemeLongReal.FromLR(LAST(Time.T)),
                                      rest := args))
  END RunCommandApply;
  
TYPE
  Done = { NotDone, Completed, TimedOut, RaisedException };

  Closure = Thread.Closure OBJECT
    c : Thread.Condition;
    mu : MUTEX;
    done : REF Done;
  END;

  WaitClosure = Closure OBJECT
    completion : ProcUtils.Completion;
    err : ProcUtils.Error;
  OVERRIDES
    apply := WCApply;
  END;

  TimeOClosure = Closure OBJECT
    timeo : Time.T;
  OVERRIDES
    apply := TOCApply;
  END;

PROCEDURE WCApply(wc : WaitClosure) : REFANY =
  BEGIN
    TRY
      wc.completion.wait();
      LOCK wc.mu DO
        IF wc.done^ = Done.NotDone THEN wc.done^ := Done.Completed END
      END;
    EXCEPT
      ProcUtils.ErrorExit(err) => 
      wc.err := err;
      LOCK wc.mu DO
        IF wc.done^ = Done.NotDone THEN wc.done^ := Done.RaisedException END
      END;
    END;

    Thread.Broadcast(wc.c);
    
    RETURN NIL
  END WCApply;

PROCEDURE TOCApply(toc : TimeOClosure) : REFANY =
  BEGIN
    WITH till = Time.Now() + toc.timeo DO
      WHILE Time.Now() < till DO 
        Thread.Pause(MIN(1.0d0, till-Time.Now()));
        LOCK toc.mu DO
          IF toc.done^ # Done.NotDone THEN RETURN NIL END
        END
      END
    END;
    LOCK toc.mu DO
      IF toc.done^ = Done.NotDone THEN toc.done^ := Done.TimedOut END
    END;

    Thread.Broadcast(toc.c);

    RETURN NIL
  END TOCApply;

PROCEDURE RunHooksCommandApply(proc : Procedure; 
                               interp : Scheme.T; 
                               args : Object) : Object RAISES { E } =
  VAR p := args;
      timeoutContinuation, errorContinuation : Object;
  CONST
    First = SchemeUtils.First;
    Rest = SchemeUtils.Rest;
  BEGIN
    timeoutContinuation := First(p);
    p := Rest(p);
    errorContinuation := First(p);
    p := Rest(p);

    TRY
      <*ASSERT proc.outputParser # NIL*>
      RETURN RealRunTimeoutCommandApply(proc,interp,p)
    EXCEPT
      E(err) => 
      WITH toRun = SchemeUtils.List2(errorContinuation,
                                     SchemeString.FromText(err)) DO
        EVAL interp.evalInGlobalEnv(toRun)
      END
    |
      Timeout(err) =>
      WITH toRun = SchemeUtils.List2(timeoutContinuation,
                                     SchemeString.FromText(err)) DO
        EVAL interp.evalInGlobalEnv(toRun)
      END
    END;
    RETURN NIL
  END RunHooksCommandApply;

PROCEDURE RunTimeoutCommandApply(proc : Procedure; 
                                 interp : Scheme.T; 
                                 args : Object) : Object RAISES { E } =
  BEGIN
    TRY
      RETURN RealRunTimeoutCommandApply(proc,interp,args)
    EXCEPT
      Timeout(err) => RAISE E(err)
    END
  END RunTimeoutCommandApply;

PROCEDURE RealRunTimeoutCommandApply(proc : Procedure; 
                                     <*UNUSED*>interp : Scheme.T; 
                                     args : Object) : Object RAISES { E, 
                                                                  Timeout } =
  <*FATAL Wr.Failure, Thread.Alerted*>
  VAR p := SchemeUtils.Rest(args);
      timeo := SchemeLongReal.FromO(SchemeUtils.First(args));
      wr := TextWr.New();
  BEGIN
    WHILE ISTYPE(p, SchemePair.T) AND p # NIL DO
      WITH word = SchemeUtils.StringifyQ(SchemeUtils.First(p),
                                         quoted := FALSE) DO
        Wr.PutText(wr, word);
        p := SchemeUtils.Rest(p);
        IF p # NIL THEN Wr.PutChar(wr, ' ') END
      END
    END;

    WITH owr = NEW(TextWr.T).init(),
         writer = ProcUtils.WriteHere(owr),
         owr2 = NEW(TextWr.T).init(),
         writer2 = ProcUtils.WriteHere(owr2),
         cmdtext = TextWr.ToText(wr),
         completion = ProcUtils.RunText(cmdtext,
                                        stdout := writer,
                                        stderr := writer2) DO

      TRY
        IF timeo = LAST(Time.T) THEN
          completion.wait()
        ELSE
          WITH c = NEW(Thread.Condition),
               mu = NEW(MUTEX),
               done = NEW(REF Done),
               waitCl = NEW(WaitClosure, 
                            c := c, mu := mu, done := done,
                            completion := completion),
               timeoCl = NEW(TimeOClosure, 
                             c := c, mu := mu, done := done,
                             timeo := timeo) DO
            done^ := Done.NotDone;
            
            EVAL Thread.Fork(waitCl); EVAL Thread.Fork(timeoCl);

            LOCK mu DO
              LOOP
                Thread.Wait(mu,c);
                CASE done^ OF
                  Done.NotDone => (* skip *)
                |
                  Done.TimedOut => RAISE Timeout("Timeout running \""& cmdtext & "\"" )
                |
                  Done.Completed => EXIT
                |
                  Done.RaisedException => RAISE ProcUtils.ErrorExit(waitCl.err)
                END
              END
            END
          END
        END
      EXCEPT
        ProcUtils.ErrorExit(err) => RAISE E("ProcUtils.ErrorExit ("&
          ProcUtils.FormatError(err) &") from running \"" & 
          cmdtext & "\"; stderr: " & TextWr.ToText(owr2))
      END;

      (* here we grab the results from running command and parse them...*)
      WITH result = TextWr.ToText(owr),
           rd = TextRd.New(result) DO
        Debug.Out("SchemeCommandRunner.RunCommandApply: cmd=\"" & cmdtext & 
          "\"; result: \"" & result & "\"");
        RETURN proc.outputParser.parseRd(rd)
      END
    END
  END RealRunTimeoutCommandApply;

TYPE 
  Procedure = SchemeProcedure.T OBJECT
    outputParser : OutputParser;
  END;

PROCEDURE Extend(op : OutputParser;
                 definer : SchemePrimitive.ExtDefiner) : SchemePrimitive.ExtDefiner =
  BEGIN

    (* (run-command <cmd> <arg1> <arg2> ... ) *)
    definer.addPrim("run-command",
                    NEW(Procedure,
                        outputParser := op,
                        apply := RunCommandApply),
                    1, LAST(CARDINAL));

    (* (run-command-with-timeout <timeo> <cmd> <arg1> <arg2> ... ) *)
    definer.addPrim("run-command-with-timeout",
                    NEW(Procedure,
                        outputParser := op,
                        apply := RunTimeoutCommandApply),
                    2, LAST(CARDINAL));

    (* (run-command-with-hooks
                   <timeout-hook> 
                   <error-hook> 
                   <timeo> <cmd> <arg1> <arg2> ... ) *)
    definer.addPrim("run-command-with-hooks",
                    NEW(Procedure,
                        outputParser := op,
                        apply := RunHooksCommandApply),
                    4, LAST(CARDINAL));

    definer.addPrim("run-raw-command-with-hooks",
                    NEW(Procedure,
                        outputParser := NEW(TextOutputParser),
                        apply := RunHooksCommandApply),
                    4, LAST(CARDINAL));



    RETURN definer
  END Extend;

REVEAL 
  TextOutputParser = OutputParser BRANDED OBJECT OVERRIDES
    parseRd := TextParseRd;
  END;

PROCEDURE TextParseRd(<*UNUSED*>p : TextOutputParser; 
                         rd : Rd.T) : Scheme.Object =
  <*FATAL Rd.Failure, Thread.Alerted, Wr.Failure*>

  (* passed a TextRd.T, convert it into a TextWr and write the text...
     a bit roundabout but good enough for now *)

  BEGIN
    WITH wr = NEW(TextWr.T).init() DO
      LOOP
        Wr.PutText(wr,Rd.GetText(rd,1024));
        IF Rd.EOF(rd) THEN EXIT END
      END;
      RETURN SchemeString.FromText(TextWr.ToText(wr))
    END
  END TextParseRd;

BEGIN END SchemeCommandRunner.
