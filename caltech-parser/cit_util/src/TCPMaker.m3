(* $Id$ *)

MODULE TCPMaker;
IMPORT IP;
IMPORT Scan, Debug, Fmt, TCP, Thread;
IMPORT AL;
IMPORT TextReader;
IMPORT FloatMode, Lex;

PROCEDURE MakeMaker(maker       : Default;
                    nameString  : TEXT;
                    defaultPort : IP.Port) : Default RAISES { ConnErr } =
  VAR
    addr : IP.Address;
    ep : IP.Endpoint;
    port := defaultPort;
  BEGIN
    TRY
      VAR
        nameReader := NEW(TextReader.T).init(nameString);
        hostName := nameReader.nextE(":");
      BEGIN
        IF NOT nameReader.isEmpty() THEN
          port := Scan.Int(nameReader.nextE(":"))
        END;
        
        TRY 
          IF NOT IP.GetHostByName(hostName, addr) THEN
            RAISE IP.Error(NIL)
          END;
          Debug.Out("Attempting to connect to " & hostName & " port " &
            Fmt.Int(port));
          
          ep.addr := addr;
          ep.port := port;
          
          maker.ep := ep;

          RETURN maker
        EXCEPT
          IP.Error(err) => 
          RAISE ConnErr("Internet Protocol error:"&AL.Format(err)&" \n  Is host \"" & hostName & 
            "\" for the server at all sensible?")
        END
      END
    EXCEPT
      TextReader.NoMore => RAISE ConnErr("Null  server name.")
    |
      FloatMode.Trap, Lex.Error => 
      RAISE ConnErr("Non-numeric port name specified for .")
    END
  END MakeMaker;

REVEAL
  Default = PubDefault BRANDED Brand & " Default" OBJECT
    ep : IP.Endpoint;
  OVERRIDES
    makeTCP := MMT;
    init := MakeMaker;
  END;

PROCEDURE MMT(m : Default) : TCP.T RAISES { IP.Error, Thread.Alerted } = 
  BEGIN 
    IF Debug.GetLevel() >= 10 THEN
      VAR
        str := "TCPMaker.MMT: connecting to addr 16_";
      BEGIN
        FOR i := FIRST(m.ep.addr.a) TO LAST(m.ep.addr.a) DO
          str := str & Fmt.Int(m.ep.addr.a[i],base := 16) 
        END;
        str := str & " port " & Fmt.Int(m.ep.port);
        Debug.Out(str)
      END
    END;
    RETURN TCP.Connect(m.ep) 
  END MMT;

REVEAL
  Simple = PubSimple BRANDED Brand & " Simple" OBJECT
    tcp : TCP.T;
  OVERRIDES
    makeTCP := SMT;
    init := InitSimple;
  END;

PROCEDURE InitSimple(s : Simple; tcp : TCP.T) : Simple =
  BEGIN s.tcp := tcp; RETURN s END InitSimple;

PROCEDURE SMT(s : Simple) : TCP.T = 
  BEGIN TRY RETURN s.tcp FINALLY s.tcp := NIL END END SMT;

BEGIN END TCPMaker.
