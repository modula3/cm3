(* Copyright 1993 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* FDTest.m3 *)
(* Last modified on Thu Mar 24 16:12:14 PST 1994 by wobber *)

MODULE FDTest EXPORTS Main;

IMPORT Atom, AtomList, Test, TCPNetObj, IP, SchedulerPosix,
       NetObj, IO, Params, Fmt, TextRd, Thread, Time, Unix;

CONST TestObjName = "FDTestObj";
      MyIPPort    = 9999;

(* invocation:

     test <no args>
     test hostname [nWaiters]

        First form runs the server side of test program.
        Second form runs client side.
*)

<* FATAL Thread.Alerted *>

TYPE
  T = Test.T OBJECT OVERRIDES
    null := DoNull;
  END;

TYPE
  Fork = Thread.Closure OBJECT
  OVERRIDES
    apply := ForkedWait;
  END;

VAR
  mu := NEW(MUTEX);
  waiters: CARDINAL := 0;
  dup := FALSE;
  fdmax := 0;
  cond := NEW(Thread.Condition);

(* server-side code *)

PROCEDURE DoNull(<*UNUSED*> t: T)
    RAISES {NetObj.Error, Thread.Alerted} =
  BEGIN
  END DoNull;

(* client-side code *)

PROCEDURE ForkedWait(<*UNUSED*> fork: Fork): REFANY =
  VAR fd: INTEGER := 0;   (* stdin = 0 *)
  BEGIN
    fd := Unix.dup(fd);
    LOCK mu DO INC(waiters); fdmax := MAX(fd, fdmax); END;
    IF fd < 0 THEN RETURN NIL; END;
    IF dup OR waiters = nWaiters THEN
      LOOP EVAL SchedulerPosix.IOAlertWait(fd, TRUE); END;
    ELSE
      LOCK mu DO LOOP Thread.Wait(mu, cond); END; END;
    END;
    <*NOWARN*> RETURN NIL;
  END ForkedWait;

PROCEDURE DoTest(t: Test.T; n: CARDINAL) =
  VAR lastWaiters: CARDINAL := 0;
  BEGIN
    IO.Put("Waiting for forked threads to call back.\n");
    FOR i := 0 TO n-1 DO EVAL Thread.Fork(NEW(Fork)); END;
    LOOP
      LOCK mu DO
        IF waiters = n THEN EXIT; END;
        IF waiters # lastWaiters THEN
          IO.Put("waiters=" & Fmt.Int(waiters) & "\n");
          lastWaiters := waiters;
        END;
      END;
      Thread.Pause(1.0D0);
    END;
    TRY
      IO.Put("fdmax=" & Fmt.Int(fdmax) & "\n");
      IO.Put("Null call test (" & Fmt.Int(n) & " threads waiting) ... ");
      VAR start := Time.Now(); nn := NumNullCalls; BEGIN
        WHILE nn # 0 DO t.null(); DEC(nn) END;
        IO.Put(Fmt.F("%s usec/call.\n",
          Fmt.Int(ROUND(((Time.Now()-start)*1.0D6)/
                  FLOAT(NumNullCalls, LONGREAL)))));
      END;
    EXCEPT
    | Thread.Alerted =>
    | NetObj.Error(ec) => IO.Put("NetObj.Error(" & ErrStr(ec) & ")\n");
    END;
  END DoTest;

(* common code *)

PROCEDURE ErrStr(x: AtomList.T): TEXT =
  VAR t: TEXT;
  BEGIN
    IF x = NIL THEN RETURN ""; END;
    t := Atom.ToText(x.head);
    IF x.tail # NIL THEN t := t  & "(" & ErrStr(x.tail) & ")"; END;
    RETURN t;
  END ErrStr;

CONST DefaultNWaiters = 50;
      NumNullCalls = 2000;

VAR nWaiters := DefaultNWaiters;

BEGIN
  TRY
    IF Params.Count <= 1 THEN
      TRY
        NetObj.Export (TestObjName, NEW(T), NIL);
        IO.Put("Exported target via netobjd\n");
      EXCEPT
      | NetObj.Error =>
          NetObj.Export (TestObjName, NEW(T), TCPNetObj.Listen(MyIPPort));
          IO.Put("Exported target at private port\n");
      END;
      IO.Put("Test server running, type <CR> to terminate\n");
      TRY EVAL IO.GetLine() EXCEPT IO.Error => END;
    ELSE
      VAR host := Params.Get(1);
          addr: IP.Address;
          t: Test.T;
          agent: NetObj.Address;
      BEGIN
        IF Params.Count > 2 THEN
          TRY
            nWaiters := IO.GetInt(TextRd.New(Params.Get(2)));
            IF nWaiters < 0 THEN
              dup := TRUE;
              nWaiters := -nWaiters;
            END;
          EXCEPT
          | IO.Error => IO.Put("Parameter must be an integer\n");
          END;
        END;
        IF NOT IP.GetHostByName(host, addr) THEN
          IO.Put(Fmt.F("No such host \"%s\"\n", host));
        ELSE
          agent := NetObj.Locate(host);
          TRY
            t := NetObj.Import(TestObjName, agent);
            IO.Put("Located target via netobjd\n");
          EXCEPT
          | NetObj.Error =>
              agent := TCPNetObj.Locate(IP.Endpoint{addr, MyIPPort});
              t := NetObj.Import(TestObjName, agent);
              IO.Put("Located target at private port\n");
          END;
          DoTest(t, nWaiters);
        END;
      END;
    END;
  EXCEPT
  | IP.Error => IO.Put("IP.Error\n");
  | TCPNetObj.Failed => IO.Put("TCPNetObj.Failed\n");
  | NetObj.Invalid => IO.Put("NetObj.Invalid\n");
  | NetObj.Error(x) => IO.Put("NetObj.Error(" & ErrStr(x) & ")\n");
  END;
END FDTest.

