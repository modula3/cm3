(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* NetObjTest.m3 *)
(* Last modified on Thu Aug 26 10:30:04 PDT 1993 by wobber *)

MODULE LongCallTest EXPORTS Main;

IMPORT LongCall, TCPNetObj, IP, NetObj, IO, Params, Fmt, Thread;

CONST
  LongCallObjName = "LongCallObj";
  MyIPPort        = 8877;

(* invocation:

   longcall <no args> 
   longcall hostname

   The former is a server.  A call to the server waits for a long time,
   then catches an alert and prints a message when it does so.

   The latter is a client.  At each client <CR>, it invokes the server long
   call method, and then alerts the waiting thread. *)

(* server side code *)

TYPE
  T = LongCall.T OBJECT
        mu  : MUTEX;
        cond: Thread.Condition;
      OVERRIDES
        wait := DoLongWait;
      END;

<* FATAL NetObj.Error *>
<* FATAL NetObj.Invalid *>
<* FATAL Thread.Alerted *>
<* FATAL IP.Error *>
<* FATAL TCPNetObj.Failed *>

VAR seqno := 1;
VAR gmu := NEW(MUTEX);

PROCEDURE DoLongWait (t: T)
  RAISES {NetObj.Error, Thread.Alerted} =
  VAR seq: CARDINAL;
  BEGIN
    LOCK gmu DO seq := seqno; INC(seqno); END;
    TRY
      IO.Put(Fmt.F("Blocking seqno=%s\n", Fmt.Int(seq)));
      LOCK t.mu DO Thread.AlertWait(t.mu, t.cond); END;
    EXCEPT
    | Thread.Alerted => IO.Put(Fmt.F("Alerted seqno=%s\n", Fmt.Int(seq)));
    END;
  END DoLongWait;


(* client side code *)

TYPE
  Fork = Thread.Closure OBJECT
           t: LongCall.T;
         OVERRIDES
           apply := LongCallFork;
         END;

PROCEDURE LongCallFork (f: Fork): REFANY =
  BEGIN
    TRY
      f.t.wait();
    EXCEPT
    | NetObj.Error (e) =>
      <* ASSERT(e.head = NetObj.Alerted) *>
    END;
    RETURN NIL;
  END LongCallFork;

BEGIN
  IF Params.Count <= 1 THEN
    VAR t := NEW(T, mu := NEW(MUTEX), cond := NEW(Thread.Condition));
    BEGIN
      TRY
        NetObj.Export(LongCallObjName, t, NIL);
        IO.Put("Exported target via netobjd\n");
      EXCEPT
      | NetObj.Error =>
          NetObj.Export(LongCallObjName, t, TCPNetObj.Listen(MyIPPort));
          IO.Put("Exported target at private port\n");
      END;
      IO.Put("Test server running, type <CR> to terminate\n");
      TRY EVAL IO.GetLine() EXCEPT IO.Error => END;
    END;
  ELSE
    VAR
      host                  := Params.Get(1);
      addr : IP.Address;
      fork                  := NEW(Fork, t := NIL);
      agent: NetObj.Address;
      tt   : Thread.T;
    BEGIN
      IF NOT IP.GetHostByName(host, addr) THEN
        IO.Put(Fmt.F("No such host \"%s\"\n", host));
      ELSE
        agent := NetObj.Locate(host);
        TRY
          fork.t := NetObj.Import(LongCallObjName, agent);
          IO.Put("Located target via netobjd\n");
        EXCEPT
        | NetObj.Error =>
            agent := TCPNetObj.Locate(IP.Endpoint{addr, MyIPPort});
            fork.t := NetObj.Import(LongCallObjName, agent);
            IO.Put("Located target at private port\n");
        END;
        TRY
          LOOP
            EVAL IO.GetLine();
            IO.Put("Forking call\n");
            tt := Thread.Fork(fork);
            Thread.Pause(1.0D0);
            Thread.Alert(tt);
            EVAL Thread.Join(tt);
            IO.Put("Joined call\n");
          END;
        EXCEPT
          IO.Error =>
        END;
      END;
    END;
  END;
END LongCallTest.

