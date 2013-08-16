(* Copyright 1993 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* LucaTest.m3 *)
(* Last modified on Fri Mar 11 15:18:58 PST 1994 by wobber *)

MODULE LucaTest EXPORTS Main;

IMPORT Atom, AtomList, Test, TCPNetObj, IP, OSError, FileRd,
       NetObj, IO, Params, Fmt, Thread, Stdio, Rd, Wr;

CONST TestObjName = "LucaTestObj";
      MyIPPort    = 9999;

(* invocation:

     test <no args>
     test hostname pathname
        pathname is to a test file containing text, to be read. 

        First form runs the server side of test program.
        Second form runs client side.
*)

TYPE
  T = Test.T OBJECT OVERRIDES
    getRd := DoGetRd;
    getRdEmbedded := DoGetRdEmbedded;
  END;

<* FATAL Thread.Alerted *>

(* server-side code *)

PROCEDURE DoGetRd(<*UNUSED*> t: T; path: TEXT): Rd.T
    RAISES {NetObj.Error, OSError.E} =
  BEGIN
    RETURN FileRd.Open(path);
  END DoGetRd;

PROCEDURE DoGetRdEmbedded(<*UNUSED*> t: T; path: TEXT): Test.RdPkl
    RAISES {NetObj.Error, OSError.E} =
  BEGIN
    RETURN NEW(Test.RdPkl, rd := FileRd.Open(path));
  END DoGetRdEmbedded;

(* client-side code *)

PROCEDURE TestPath(t: Test.T; path: TEXT) RAISES {OSError.E, NetObj.Error} =
  VAR rd: Rd.T;
  BEGIN
    IO.Put("Opening " & path & " ... ");
    rd := t.getRd(path);
    IO.Put("ok\n\n");
    SuckRd(rd);
    IO.Put("\nOpening " & path & " ... ");
    rd := t.getRdEmbedded(path).rd;
    IO.Put("ok\n\n");
    SuckRd(rd);
  END TestPath;

PROCEDURE SuckRd(rd: Rd.T) =
  VAR buf: ARRAY [0..99] OF CHAR;
      n: CARDINAL;
      <* FATAL Wr.Failure *>
  BEGIN
    TRY
      TRY
        LOOP
          n := Rd.GetSub(rd, buf);
          Wr.PutString(Stdio.stdout, SUBARRAY(buf, 0, n));
          IF n # NUMBER(buf) THEN EXIT; END;
        END;
      FINALLY
        Rd.Close(rd);
      END;
    EXCEPT
    | Rd.Failure(x) =>
        IO.Put("Rd.Failure(" & ErrStr(x) & ")\n");
    END;
  END SuckRd;

(* common code *)

PROCEDURE ErrStr(x: AtomList.T): TEXT =
  VAR t: TEXT;
  BEGIN
    IF x = NIL THEN RETURN ""; END;
    t := Atom.ToText(x.head);
    IF x.tail # NIL THEN t := t  & "(" & ErrStr(x.tail) & ")"; END;
    RETURN t;
  END ErrStr;

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
        IF Params.Count <= 2 THEN
          IO.Put("Usage:  test hostname pathname\n");
        ELSIF NOT IP.GetHostByName(host, addr) THEN
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
          TestPath(t, Params.Get(2));
        END;
      END;
    END;
  EXCEPT
  | IP.Error => IO.Put("IP.Error\n");
  | TCPNetObj.Failed => IO.Put("TCPNetObj.Failed\n");
  | NetObj.Invalid => IO.Put("NetObj.Invalid\n");
  | NetObj.Error(x) => IO.Put("NetObj.Error(" & ErrStr(x) & ")\n");
  | OSError.E(x) => IO.Put("OSError.E(" & ErrStr(x) & ")\n");
  END;
END LucaTest.

