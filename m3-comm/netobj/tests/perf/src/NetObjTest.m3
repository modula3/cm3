(* Copyright 1993 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* NetObjTest.m3 *)
(* Last modified on Mon Dec 12 15:53:31 PST 1994 by wobber *)

MODULE NetObjTest EXPORTS Main;

IMPORT Test, XRd, TCPNetObj, IP,
       NetObj, Rd, Wr, IO, Params, NullWr, Text, Time, Fmt, Thread;

FROM Test IMPORT ObjType;

CONST TestObjName = "TestObj";
      MyIPPort    = 9999;

(* invocation:

     test <no args>
     test hostname [-help] [-short] [-null] [-ten] [-rd] [-wr]
                   [-o] [-oc] [-oq] [-r] [-rc] [-rq] [-sp] [-bp]

        First form runs the server side of test program.
        Second form runs client side.  The "-short" option
        produces a shorter test suitable for etp'ing.
        The "null" option runs only the null call test.
        The "ten" option runs only the ten integer call test.
        The "o" option runs only the same surrogate object argument test.
        The "oc" option runs only the same concrete object argument test.
        The "oq" option runs only the unique object argument test.
        The "r" option runs only the same surrogate object  return test.
        The "rc" option runs only the same concrete object return test.
        The "rq" option runs only the unique object return test.
        The "rd" option runs only the stream reader test.
        The "wr" option runs only the stream writer test.
        The "sp" option runs only the small pickle test.
        The "bp" option runs only the large pickle test.
*)

(* server side code *)

TYPE
  T = Test.T OBJECT OVERRIDES
    null := NullCall;
    ten := TenIntCall;
    objarg := OneObjCall;
    rd := GetReader;
    wr := GetWriter;
    newObj := NewObject;
    newReturn := NewReturn;
    pickle := PickleCall;
  END;

  Return = Test.Return OBJECT
    type: ObjType;
    obj: NetObj.T := NIL;
  OVERRIDES
    doIt := ReturnObj;
  END;

  ObjectArg = NetObj.T OBJECT
    x: INTEGER := 0;
  END;

  <* FATAL NetObj.Error *>
  <* FATAL NetObj.Invalid *>
  <* FATAL Thread.Alerted *>
  <* FATAL Wr.Failure *>
  <* FATAL Rd.Failure *>
  <* FATAL IP.Error *>
  <* FATAL TCPNetObj.Failed *>

VAR fixedObj := NEW(ObjectArg);

PROCEDURE NullCall (<*UNUSED*> t: T) =
  BEGIN
  END NullCall;

PROCEDURE TenIntCall (<*UNUSED*> t: T;
              <*UNUSED*> a0, a1, a2, a3, a4, a5, a6, a7, a8, a9: INTEGER) =
  BEGIN
  END TenIntCall;

PROCEDURE OneObjCall (<*UNUSED*> t: T; <*UNUSED*> o: NetObj.T) =
  BEGIN
  END OneObjCall;

PROCEDURE PickleCall (<*UNUSED*> t: T; <*UNUSED*> r: REFANY) =
  BEGIN
  END PickleCall;

PROCEDURE GetReader (<*UNUSED*> t: T): Rd.T =
  BEGIN
    RETURN NEW (XRd.T).init (1000000000)
  END GetReader;

PROCEDURE GetWriter (<*UNUSED*> t: T): Wr.T =
  BEGIN
    RETURN NEW (NullWr.T).init ()
  END GetWriter;


(* client side code *)

CONST BuffSize = 8*1024 - 16;
      LongStreamTestLength = 50000000;
      LongCallTestReps = 5000;
      ShortStreamTestLength = 5000000;
      ShortCallTestReps = 1000;

VAR buff : ARRAY [0..BuffSize-1] OF CHAR;
    callReps: CARDINAL := LongCallTestReps;
    streamBytes: CARDINAL := LongStreamTestLength;

PROCEDURE DoNullCallTest(t: Test.T) =
  VAR start: Time.T;
      nn := callReps;
  BEGIN
    IO.Put("Null call test ... ");
    start := Time.Now();
    WHILE nn # 0 DO t.null(); DEC(nn) END;
    IO.Put(Fmt.F("%s usec/call.\n",
        Fmt.Int(ROUND(((Time.Now()-start)*1.0D6)/FLOAT(callReps, LONGREAL)))));
  END DoNullCallTest;

PROCEDURE DoTenIntTest(t: Test.T) =
  VAR start: Time.T;
      nn := callReps;
  BEGIN
    IO.Put("Ten integer call test ... ");
    start := Time.Now();
    WHILE nn # 0 DO t.ten(1,2,3,4,5,6,7,8,9,10); DEC(nn) END;
    IO.Put(Fmt.F("%s usec/call.\n",
        Fmt.Int(ROUND(((Time.Now()-start)*1.0D6)/FLOAT(callReps, LONGREAL)))));
  END DoTenIntTest;

PROCEDURE DoPickleTest(t: Test.T; small: BOOLEAN) =
  VAR start: Time.T;
      nn := callReps;
      r: REFANY;
  BEGIN
    IF small THEN
      IO.Put("REF CHAR pickle call test  ... ");
      r := NEW(REF CHAR);
    ELSE
      IO.Put("Linked list pickle call test  ... ");
      r := InitLinkedList();
    END;
    start := Time.Now();
    WHILE nn # 0 DO t.pickle(r); DEC(nn) END;
    IO.Put(Fmt.F("%s usec/call.\n",
        Fmt.Int(ROUND(((Time.Now()-start)*1.0D6)/FLOAT(callReps, LONGREAL)))));
  END DoPickleTest;

TYPE LinkedList = REF RECORD last, next: LinkedList; END;

CONST NListElems = 25;

PROCEDURE InitLinkedList(): LinkedList =
  VAR this, first, last: LinkedList := NIL;
  BEGIN
    FOR i := 0 TO NListElems-1 DO
      this := NEW(LinkedList, last := last, next := NIL);
      IF first = NIL THEN
        first := this;
      ELSE
        last.next := this;
      END;
      last := this;
    END;
    RETURN first;
  END InitLinkedList;

PROCEDURE DoObjArgTest(t: Test.T; argType: ObjType) =
  VAR start: Time.T;
      nn := callReps;
      obj: NetObj.T := NIL;
  BEGIN
    CASE argType OF
    | ObjType.Surrogate =>
        IO.Put("Same surrogate object argument test ... ");
        obj := t.newObj();
    | ObjType.Concrete =>
        IO.Put("Same concrete object argument test ... ");
        obj := fixedObj;
    | ObjType.Unique =>
        IO.Put("Unique object argument test ... ");
    END;
    start := Time.Now();
    IF argType # ObjType.Unique THEN
      WHILE nn # 0 DO t.objarg(obj); DEC(nn) END;
    ELSE
      WHILE nn # 0 DO t.objarg(NEW(ObjectArg)); DEC(nn) END;
    END;
    IO.Put(Fmt.F("%s usec/call.\n",
        Fmt.Int(ROUND(((Time.Now()-start)*1.0D6)/FLOAT(callReps, LONGREAL)))));
  END DoObjArgTest;

PROCEDURE DoObjRetTest(t: Test.T; argType: ObjType) =
  VAR start: Time.T;
      nn := callReps;
      cl: Test.Return;
  BEGIN
    CASE argType OF
    | ObjType.Surrogate =>
        IO.Put("Same surrogate object result test ... ");
    | ObjType.Concrete =>
        IO.Put("Same concrete object result test ... ");
    | ObjType.Unique =>
        IO.Put("Unique object result test ... ");
    END;
    cl := t.newReturn(argType, fixedObj);
    start := Time.Now();
    WHILE nn # 0 DO EVAL cl.doIt(); DEC(nn) END;
    IO.Put(Fmt.F("%s usec/call.\n",
        Fmt.Int(ROUND(((Time.Now()-start)*1.0D6)/FLOAT(callReps, LONGREAL)))));
  END DoObjRetTest;

PROCEDURE DoReaderTest(t: Test.T) =
  VAR start: Time.T;
      rd: Rd.T;
      nn := streamBytes;
  BEGIN
    IO.Put("Reader test ... ");
    rd := t.rd();
    start := Time.Now();
    WHILE nn # 0 DO
      EVAL Rd.GetSub (rd, buff);
      DEC (nn, MIN (nn, BuffSize))
    END;
    IO.Put(Fmt.F("%s KBytes/sec.\n",
        Fmt.Int(
          ROUND(FLOAT(streamBytes, LONGREAL)/(1.0D3*(Time.Now()-start))))));
  END DoReaderTest;

PROCEDURE DoWriterTest(t: Test.T) =
  VAR start: Time.T;
      wr: Wr.T;
      nn := streamBytes;
  BEGIN
    IO.Put("Writer test ... ");
    wr := t.wr();
    start := Time.Now();
    WHILE nn # 0 DO
      Wr.PutString (wr, buff);
      DEC (nn, MIN (nn, BuffSize))
    END;
    IO.Put(Fmt.F("%s KBytes/sec.\n",
        Fmt.Int(
          ROUND(FLOAT(streamBytes, LONGREAL)/(1.0D3*(Time.Now()-start))))));
  END DoWriterTest;

PROCEDURE NewReturn(<*UNUSED*> t: T;
                   type: ObjType; surr: NetObj.T): Test.Return =
  VAR res := NEW(Return, type := type);
  BEGIN
    CASE type OF
    | ObjType.Surrogate => res.obj := surr;
    | ObjType.Concrete => res.obj := fixedObj;
    | ObjType.Unique =>
    END;
    RETURN res;
  END NewReturn;

PROCEDURE NewObject (<*UNUSED*> t: T) : NetObj.T =
  BEGIN
    RETURN NEW(ObjectArg);
  END NewObject;

PROCEDURE ReturnObj(r: Return): NetObj.T =
  BEGIN
    CASE r.type OF
    | ObjType.Surrogate, ObjType.Concrete => RETURN r.obj;
    | ObjType.Unique => RETURN NEW(ObjectArg);
    END;
  END ReturnObj;


(* common code *)

TYPE TestType = {Invalid, All, Null, TenInt, Read, Write,
                 ObjArg, ObjArgConcrete, ObjArgUniq,
                 ObjRet, ObjRetConcrete, ObjRetUniq,
                 SmallPickle, BigPickle};

PROCEDURE CheckOptions(VAR host: TEXT) : TestType =
  VAR type: TestType := TestType.All;
  BEGIN
    host := NIL;
    FOR i := 1 TO Params.Count-1 DO
      VAR arg := Params.Get(i); BEGIN
        IF NOT Text.Empty(arg) THEN
          IF Text.GetChar(arg, 0) # '-' THEN
            IF host = NIL THEN host := arg; END;
          ELSE
            IF Text.Equal(arg, "-short") THEN
              callReps := ShortCallTestReps;
              streamBytes := ShortStreamTestLength;
            ELSIF Text.Equal(arg, "-null") THEN type := TestType.Null
            ELSIF Text.Equal(arg, "-ten") THEN type := TestType.TenInt
            ELSIF Text.Equal(arg, "-o") THEN type := TestType.ObjArg
            ELSIF Text.Equal(arg, "-oc") THEN type := TestType.ObjArgConcrete
            ELSIF Text.Equal(arg, "-oq") THEN type := TestType.ObjArgUniq
            ELSIF Text.Equal(arg, "-r") THEN type := TestType.ObjRet
            ELSIF Text.Equal(arg, "-rc") THEN type := TestType.ObjRetConcrete
            ELSIF Text.Equal(arg, "-rq") THEN type := TestType.ObjRetUniq
            ELSIF Text.Equal(arg, "-rd") THEN type := TestType.Read
            ELSIF Text.Equal(arg, "-wr") THEN type := TestType.Write
            ELSIF Text.Equal(arg, "-sp") THEN type := TestType.SmallPickle
            ELSIF Text.Equal(arg, "-bp") THEN type := TestType.BigPickle
            ELSE RETURN TestType.Invalid;
            END;
          END;
        END;
      END;
    END;
    IF host = NIL THEN RETURN TestType.Invalid; END;
    RETURN type;
  END CheckOptions;

PROCEDURE PrintHelp() =
  BEGIN
    IO.Put("Usage: test <host> [options]\n");
    IO.Put("Options are:\n");
    IO.Put("    -help    Print this message.\n");
    IO.Put("    -short   Repeat test 1000 times, otherwise 5000 times.\n");
    IO.Put("               (For byte stream tests this is 5MB vs. 50MB.)\n");
    IO.Put("  Various method call tests:\n");
    IO.Put("    -null    The null method.\n");
    IO.Put("    -ten     A method with ten integer arguments.\n");
    IO.Put("    -o       The same surrogate obj arg each time.\n");
    IO.Put("    -oc      The same concrete obj arg each time.\n");
    IO.Put("    -oq      A unique concrete obj arg each call.\n");
    IO.Put("    -r       Return the same surrogate obj each call.\n");
    IO.Put("    -rc      Return a different concrete obj each call.\n");
    IO.Put("    -rq      Return a different concrete obj each call.\n");
    IO.Put("    -sp      A REF CHAR argument for each call.\n");
    IO.Put("    -bp      A 25-element linked list argument for each call.\n");
    IO.Put("  Byte stream tests:\n");
    IO.Put("    -rd      Read data from a marshaled reader.\n");
    IO.Put("    -wr      Write data to a marshaled writer.\n");
  END PrintHelp;

BEGIN
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
    VAR host: TEXT;
        type := CheckOptions(host);
        addr: IP.Address;
        t: Test.T;
        agent: NetObj.Address;
    BEGIN
      IF type = TestType.Invalid THEN
        PrintHelp();
      ELSE
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
          VAR qname: TEXT;
            <* FATAL IP.Error *>
          BEGIN
            qname := IP.GetCanonicalByName(host);
            IO.Put("Connected to " & qname & "\n");
          END;
          CASE type OF
          | TestType.Invalid =>
          | TestType.Null =>
              DoNullCallTest(t);
          | TestType.TenInt =>
              DoTenIntTest(t);
          | TestType.SmallPickle =>
              DoPickleTest(t, TRUE);
          | TestType.BigPickle =>
              DoPickleTest(t, FALSE);
          | TestType.Read =>
              DoReaderTest(t);
          | TestType.Write =>
              DoWriterTest(t);
          | TestType.ObjArg =>
              DoObjArgTest(t, ObjType.Surrogate);
          | TestType.ObjArgConcrete =>
              DoObjArgTest(t, ObjType.Concrete);
          | TestType.ObjArgUniq =>
              DoObjArgTest(t, ObjType.Unique);
          | TestType.ObjRet =>
              DoObjRetTest(t, ObjType.Surrogate);
          | TestType.ObjRetConcrete =>
              DoObjRetTest(t, ObjType.Concrete);
          | TestType.ObjRetUniq =>
              DoObjRetTest(t, ObjType.Unique);
          | TestType.All =>
              DoNullCallTest(t);
              DoTenIntTest(t);
              DoObjArgTest(t, ObjType.Surrogate);
              DoObjArgTest(t, ObjType.Concrete);
              DoObjArgTest(t, ObjType.Unique);
              DoObjRetTest(t, ObjType.Surrogate);
              DoObjRetTest(t, ObjType.Concrete);
              DoObjRetTest(t, ObjType.Unique);
              DoPickleTest(t, TRUE);
              DoPickleTest(t, FALSE);
              DoReaderTest(t);
              DoWriterTest(t);
          END;
        END;
      END;
    END;
  END;
END NetObjTest.

