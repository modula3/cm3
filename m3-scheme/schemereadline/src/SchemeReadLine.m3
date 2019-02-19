(* $Id$ *)

MODULE SchemeReadLine;
IMPORT Scheme, Csighandler;
FROM Scheme IMPORT EnvDisablesTracebacks;
IMPORT NetObj, Thread;
IMPORT ReadLine, ReadLineError;
IMPORT SchemeInputPort, SchemeSymbol, SchemeUtils;
IMPORT SchemeInputPortClass;
IMPORT Rd, AL, Debug;
IMPORT TextWr;
IMPORT Text, IntSeq;
IMPORT RuntimeError;
IMPORT SchemeClass, Wr;
IMPORT SchemePair;

<* FATAL Thread.Alerted *>

TYPE 
  Interrupter = Scheme.Interrupter OBJECT
  OVERRIDES
    interrupt := Interrupt;
  END;

PROCEDURE Interrupt(<*UNUSED*>i : Interrupter) : BOOLEAN =
  BEGIN
    IF Csighandler.have_signal() = 1 THEN 
      Csighandler.clear_signal();
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END Interrupt;

TYPE 
  InputPort = SchemeInputPort.T OBJECT
    rl : ReadLine.T;
    buff : IntSeq.T;
  METHODS
    init(rl : ReadLine.T) : InputPort:= InitPort; (* shadows normal init, whch
                                                     isnt called at all *)
  OVERRIDES
    getCh := GetCh;
    (* should override close, too *)

    fastGetCh := GetCh;
    lock := NopDummy;
    unlock := NopDummy;
  END;

PROCEDURE NopDummy(<*UNUSED*>p : InputPort) = BEGIN (*skip*) END NopDummy;

PROCEDURE InitPort(p : InputPort; rl : ReadLine.T) : InputPort =
  BEGIN
    p.rl := rl;
    p.buff := NEW(IntSeq.T).init();
    RETURN p
  END InitPort;

CONST DebugALL = FALSE;

PROCEDURE GetCh(p : InputPort) : INTEGER =
  BEGIN
    LOOP
      IF p.buff.size() > 0 THEN 
        WITH ch = p.buff.remlo() DO
          IF DebugALL THEN
            IF ch = SchemeInputPort.ChEOF THEN
              Debug.Out("GetCh = EOF")
            ELSE
              Debug.Out("GetCh = " & Text.FromChar(VAL(ch,CHAR)))
            END
          END;
          RETURN ch
        END
      END;
      
      WHILE p.buff.size() = 0 DO
        TRY
          WITH txt = p.rl.readLine() DO
            p.rl.setPrompt("- ");
            FOR i := 0 TO Text.Length(txt) - 1 DO
              p.buff.addhi(ORD(Text.GetChar(txt,i)))
            END;
            p.buff.addhi(ORD('\n'))
          END
        EXCEPT
          Rd.EndOfFile => p.buff.addhi(SchemeInputPort.ChEOF)
        |
          ReadLineError.E(err) => 
          Debug.Warning("Caught ReadLineError.E : " & AL.Format(err));
          p.buff.addhi(SchemeInputPort.ChEOF)
        |
          NetObj.Error(err) => Debug.Warning("Caught NetObj.Error : " & 
            AL.Format(err));
          p.buff.addhi(SchemeInputPort.ChEOF)
          
        END
      END
    END
  END GetCh;

(* for the returning version:
   pre-define a return hook such that we can catch the output *)
PROCEDURE MainLoop(rl : ReadLine.T; scm : Scheme.T) RAISES { NetObj.Error,
                                                             ReadLineError.E }=
  <*FATAL Wr.Failure*> (* no point in trying to put errors to a broken pipe *)

  PROCEDURE Display(what : TEXT) RAISES { Wr.Failure, 
                                          NetObj.Error, 
                                          ReadLineError.E } =
    BEGIN
      IF doReadLine THEN
        rl.display(what)
      ELSE
        Wr.PutText(scm.output, what)
      END
    END Display;

  VAR
    sip : SchemeInputPort.T;
    doReadLine := rl # NIL;
  BEGIN
    Csighandler.install_int_handler();
    IF doReadLine THEN
      rl.startProc();
      rl.display("M-Scheme Experimental\nLITHP ITH LITHENING.\n");
      rl.setPrompt("> ");
      sip := NEW(InputPort).init(rl)
    ELSE
      sip := scm.input
    END;

    scm.setInterrupter(NEW(Interrupter));

    scm.bind(SchemeSymbol.Symbol("bang-bang"), NIL);

    LOOP
      IF doReadLine THEN
        rl.setPrompt("> ");
      ELSE
        Wr.PutText(scm.output, ">"); Wr.Flush(scm.output)
      END;

      TRY

        PROCEDURE Do() : BOOLEAN 
          RAISES { Scheme.E, NetObj.Error, ReadLineError.E } =
          BEGIN
            WITH x = sip.read() DO
              IF SchemeInputPort.IsEOF(x) THEN RETURN FALSE END;
              
              IF DebugALL THEN Debug.Out("Eval!") END;
              Csighandler.clear_signal();
              WITH res = scm.evalInGlobalEnv(x) DO
                IF doReadLine THEN
                  WITH wr = NEW(TextWr.T).init() DO
                    EVAL SchemeUtils.Write(res, wr, TRUE);
                    rl.display(TextWr.ToText(wr) & "\n")
                  END
                ELSE
                  EVAL SchemeUtils.Write(res, scm.output, TRUE)
                END;
                
                scm.setInGlobalEnv(SchemeSymbol.Symbol("bang-bang"),res)
              END
            END;
            RETURN TRUE
          END Do;

        BEGIN
          IF scm.attemptToMapRuntimeErrors() THEN
            TRY
              IF NOT Do() THEN RETURN END;
            EXCEPT
              <*NOWARN*>RuntimeError.E(err) => 
              Display("EXCEPTION! RuntimeError! " &  RuntimeError.Tag(err) & "\n")
            END
          ELSE
            IF NOT Do() THEN RETURN END
          END
        END
      EXCEPT
        Scheme.E(e) => Display("EXCEPTION! " & Debug.UnNil(e) & "\n");
        IF EnvDisablesTracebacks THEN
          Display("(Tracebacks disabled by NOMSCHEMETRACEBACKS.)\n")
        END
      END;

      IF NOT doReadLine THEN
        Wr.PutText(scm.output, "\n"); Wr.Flush(scm.output)
      END
    END
  END MainLoop;

VAR ReturnHookAtom := SchemeSymbol.Symbol("**return-hook**");

PROCEDURE ReturningMainLoop(rl : ReadLine.T; scm : Scheme.T) : Scheme.Object
  RAISES { NetObj.Error, ReadLineError.E }=
  <*FATAL Wr.Failure*> (* no point in trying to put errors to a broken pipe *)

  PROCEDURE Display(what : TEXT) RAISES { Wr.Failure, 
                                          NetObj.Error, 
                                          ReadLineError.E } =
    BEGIN
      IF doReadLine THEN
        rl.display(what)
      ELSE
        Wr.PutText(scm.output, what)
      END
    END Display;

  VAR
    sip : SchemeInputPort.T;
    doReadLine := rl # NIL;
  BEGIN
    Csighandler.install_int_handler();
    IF doReadLine THEN
      rl.startProc();
      rl.display("M-Scheme Experimental\nLITHP ITH LITHENING.\n");
      rl.display(
        "return "& SchemeSymbol.ToText(ReturnHookAtom) & " to quit.\n");
      rl.setPrompt("> ");
      sip := NEW(InputPort).init(rl)
    ELSE
      sip := scm.input
    END;

    scm.setInterrupter(NEW(Interrupter));

    scm.bind(SchemeSymbol.Symbol("bang-bang"), NIL);

    LOOP
      IF doReadLine THEN
        rl.setPrompt("> ");
      ELSE
        Wr.PutText(scm.output, ">"); Wr.Flush(scm.output)
      END;

      TRY

        PROCEDURE Do(VAR result : Scheme.Object) : BOOLEAN 
          RAISES { Scheme.E, NetObj.Error, ReadLineError.E } =
          BEGIN
            WITH x = sip.read() DO
              IF SchemeInputPort.IsEOF(x) THEN result := NIL; RETURN FALSE END;
              
              IF DebugALL THEN Debug.Out("Eval!") END;
              Csighandler.clear_signal();
              WITH res = scm.evalInGlobalEnv(x) DO
                TYPECASE res OF
                  NULL => (* skip *)
                |
                  SchemePair.T(p) =>
                  IF p.first = ReturnHookAtom THEN 
                    result := p.rest; RETURN FALSE
                  END
                ELSE (* skip *)
                END;
                IF doReadLine THEN
                  WITH wr = NEW(TextWr.T).init() DO
                    EVAL SchemeUtils.Write(res, wr, TRUE);
                    rl.display(TextWr.ToText(wr) & "\n")
                  END
                ELSE
                  EVAL SchemeUtils.Write(res, scm.output, TRUE)
                END;
                
                scm.setInGlobalEnv(SchemeSymbol.Symbol("bang-bang"),res);
              END
            END;
            RETURN TRUE
          END Do;

        VAR res : Scheme.Object;
        BEGIN
          IF scm.attemptToMapRuntimeErrors() THEN
            TRY
              IF NOT Do(res) THEN RETURN res END
            EXCEPT
              <*NOWARN*>RuntimeError.E(err) => 
              Display("EXCEPTION! RuntimeError! " & 
                RuntimeError.Tag(err))
            END
          ELSE
            IF NOT Do(res) THEN RETURN res END
          END
        END
      EXCEPT
        Scheme.E(e) => Display("EXCEPTION! " & e & "\n");
        IF EnvDisablesTracebacks THEN
          Display("(Tracebacks disabled by NOMSCHEMETRACEBACKS.)\n")
        END

      END;

      IF NOT doReadLine THEN
        Wr.PutText(scm.output, "\n"); Wr.Flush(scm.output)
      END
    END
  END ReturningMainLoop;

BEGIN END SchemeReadLine.
  
