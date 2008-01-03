(* Copyright 1993 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* NetObjTest.m3 *)
(* Last modified on Tue Aug 24 17:01:51 PDT 1993 by wobber *)

MODULE Echo EXPORTS Main;

IMPORT EchoRW, TCPNetObj, IP,
       NetObj, Rd, Wr, IO, Params, Fmt, Thread;

FROM Stdio IMPORT stdin, stdout;

CONST EchoObjName = "EchoObj";
      MyIPPort    = 7777;

(* invocation:

     echo <no args>
     echo hostname

        The former is a server, which forks a thread to
        echo all input on the argument reader to the argument
        writer with flushes any newlines.

        The latter passes stdin and stdout to the server,
        and waits until stdout is closed.
*)

(* server side code *)

TYPE
  T = EchoRW.T OBJECT OVERRIDES
    echo := DoEcho;
  END;

  Copier = Thread.Closure OBJECT
    rd: Rd.T;
    wr: Wr.T;
  OVERRIDES
    apply := DoCopy;
  END;

  <* FATAL NetObj.Error *>
  <* FATAL NetObj.Invalid *>
  <* FATAL Thread.Alerted *>
  <* FATAL Wr.Failure *>
  <* FATAL Rd.Failure *>
  <* FATAL IP.Error *>
  <* FATAL TCPNetObj.Failed *>

PROCEDURE DoEcho (<*UNUSED*> t: T; rd: Rd.T; wr: Wr.T) =
  BEGIN
    EVAL Thread.Fork(NEW(Copier, rd := rd, wr := wr));
  END DoEcho;

PROCEDURE DoCopy(c: Copier): REFANY =
  BEGIN
    TRY
      LOOP
        Wr.PutText(c.wr, Rd.GetLine(c.rd) & "\n");
        Wr.Flush(c.wr);
      END;
    EXCEPT
    | Rd.Failure, Rd.EndOfFile, Wr.Failure, Thread.Alerted =>
    END;
    RETURN NIL;
  END DoCopy;

(* common code *)

BEGIN
  IF Params.Count <= 1 THEN
    TRY
      NetObj.Export (EchoObjName, NEW(T), NIL);
      IO.Put("Exported target via netobjd\n");
    EXCEPT
    | NetObj.Error =>
        NetObj.Export (EchoObjName, NEW(T), TCPNetObj.Listen(MyIPPort));
        IO.Put("Exported target at private port\n");
    END;
    IO.Put("Echo server running, type <CR> to terminate\n");
    TRY EVAL IO.GetLine() EXCEPT IO.Error => END;
  ELSE
    VAR host := Params.Get(1);
        addr: IP.Address;
        t: EchoRW.T;
        agent: NetObj.Address;
    BEGIN
      IF NOT IP.GetHostByName(host, addr) THEN
        IO.Put(Fmt.F("No such host \"%s\"\n", host));
      ELSE
        agent := NetObj.Locate(host);
        TRY
          t := NetObj.Import(EchoObjName, agent);
          IO.Put("Located target via netobjd\n");
        EXCEPT
        | NetObj.Error =>
            agent := TCPNetObj.Locate(IP.Endpoint{addr, MyIPPort});
            t := NetObj.Import(EchoObjName, agent);
            IO.Put("Located target at private port\n");
        END;
        t.echo(stdin, stdout);
        LOOP Thread.Pause(6.0d1); END;
      END;
    END;
  END;
END Echo.

