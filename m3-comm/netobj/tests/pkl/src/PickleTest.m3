(* Copyright 1993 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PickleTest.m3 *)
(* Last modified on Thu Mar  3 10:46:09 PST 1994 by wobber *)

MODULE PickleTest EXPORTS Main;

IMPORT Atom, AtomList, Test, TCPNetObj, IP,
       NetObj, IO, Params, Text, Fmt, Thread;

CONST TestObjName = "PickleTestObj";
      MyIPPort    = 8888;

(* invocation:

     test <no args>
     test hostname

        First form runs the server side of test program.
        Second form runs client side.
*)

CONST SampleText1 = "this is a test";
CONST SampleText2 = "this is another";
CONST SampleText3 = "this is yet another";

(* object types for test *)

TYPE
  TestElem = OBJECT
    name: TEXT;
  METHODS
    new(): REFANY;
    equal(r: REFANY): BOOLEAN;
  END;


(* one text test *)

TYPE
  TextRef = REF RECORD t: TEXT; END;
  TextTest = TestElem OBJECT OVERRIDES
    new := NewText;
    equal := TextEqual;
  END;

PROCEDURE NewText(<*UNUSED*> t: TestElem): REFANY =
  BEGIN
    RETURN NEW(TextRef, t := SampleText1)
  END NewText;

PROCEDURE TextEqual(<*UNUSED*> t: TestElem; r: REFANY) : BOOLEAN =
  VAR tt: TextRef := r;
  BEGIN
    RETURN Text.Equal(tt.t, SampleText1);
  END TextEqual;


(* two text test *)

TYPE
  TwoTextRef = REF RECORD t1, t2: TEXT; END;
  TwoTextTest = TestElem OBJECT OVERRIDES
    new := NewTwoText;
    equal := TwoTextEqual;
  END;

PROCEDURE NewTwoText(<*UNUSED*> t: TestElem): REFANY =
  BEGIN
    RETURN NEW(TwoTextRef, t1 := SampleText1, t2 := SampleText2);
  END NewTwoText;

PROCEDURE TwoTextEqual(<*UNUSED*> t: TestElem; r: REFANY) : BOOLEAN =
  VAR tt: TwoTextRef := r;
  BEGIN
    RETURN Text.Equal(tt.t1, SampleText1) AND Text.Equal(tt.t2, SampleText2);
  END TwoTextEqual;


(* text array test *)

TYPE
  TextArrayRef = REF RECORD a: REF ARRAY OF TEXT; END;
  TextArrayTest = TestElem OBJECT OVERRIDES
    new := NewTextArray;
    equal := TextArrayEqual;
  END;

PROCEDURE NewTextArray(<*UNUSED*> t: TestElem): REFANY =
  VAR arg := NEW(TextArrayRef, a := NEW(REF ARRAY OF TEXT, 3));
  BEGIN
    arg.a[0] := SampleText1;
    arg.a[1] := SampleText2;
    arg.a[2] := SampleText3;
    RETURN arg;
  END NewTextArray;

PROCEDURE TextArrayEqual(<*UNUSED*> t: TestElem; r: REFANY) : BOOLEAN =
  VAR res: TextArrayRef := r;
  BEGIN
    RETURN NUMBER(res.a^) = 3 AND
              Text.Equal(res.a[0], SampleText1) AND
              Text.Equal(res.a[1], SampleText2) AND
              Text.Equal(res.a[2], SampleText3);
  END TextArrayEqual;


(* self-referential ref test *)

TYPE
  SelfRef = REF RECORD a: SelfRef; END;
  SelfTest = TestElem OBJECT OVERRIDES
    new := NewSelf;
    equal := SelfEqual;
  END;

PROCEDURE NewSelf(<*UNUSED*> t: TestElem): REFANY =
  VAR r := NEW(SelfRef);
  BEGIN
    r.a := r;
    RETURN r;
  END NewSelf;

PROCEDURE SelfEqual(<*UNUSED*> t: TestElem; r: REFANY): BOOLEAN =
  VAR res: SelfRef := r;
  BEGIN
    RETURN res = res.a;
  END SelfEqual;


(* site ref test *)

TYPE
  IPPort = BITS 32 FOR [0..65535];
  SiteRef = REF RECORD name: TEXT;
                       port: IPPort; port1: IPPort; route: TEXT; END;
  SiteTest = TestElem OBJECT OVERRIDES
    new := NewSite;
    equal := SiteEqual;
  END;

CONST SamplePort = 5555;

PROCEDURE NewSite(<*UNUSED*> t: TestElem): REFANY =
  BEGIN
    RETURN NEW(SiteRef, name := SampleText1, port := SamplePort,
                        route := SampleText2);
  END NewSite;

PROCEDURE SiteEqual(<*UNUSED*> t: TestElem; r: REFANY) : BOOLEAN =
  VAR s: SiteRef := r;
  BEGIN
    RETURN (s.port = SamplePort) AND Text.Equal(s.name, SampleText1) AND
                                   Text.Equal(s.route, SampleText2);
  END SiteEqual;


(* file info test *)

TYPE
  FilePerm = [0..65535];
  FileType = {Normal, SLink, Dir, Other};
  FileInfo = REF RECORD
               type: BITS 16 FOR FileType;
               perm: BITS 16 FOR FilePerm;
               length: BITS 32 FOR [-16_7FFFFFFF-1..16_7FFFFFFF];
               date: LONGREAL;
             END;
  FileInfoTest = TestElem OBJECT OVERRIDES
    new := NewInfo;
    equal := InfoEqual;
  END;

CONST SamplePerm = 755;
      SampleLength = 1000000;
      SampleDate = 3.0D08;
      SampleType = FileType.Normal;

PROCEDURE NewInfo(<*UNUSED*> t: TestElem): REFANY =
  BEGIN
    RETURN NEW(FileInfo,
                 type := SampleType,
                 length := SampleLength,
                 date := SampleDate,
                 perm := SamplePerm);
  END NewInfo;

PROCEDURE InfoEqual(<*UNUSED*> t: TestElem; r: REFANY) : BOOLEAN =
  VAR f: FileInfo := r;
  BEGIN
    RETURN (f.type = SampleType) AND
           (f.length = SampleLength) AND
           (f.date = SampleDate) AND
           (f.perm = SamplePerm);
  END InfoEqual;


(* cardinal test *)

TYPE
  Card32 = BITS 32 FOR [-16_7FFFFFFF-1..16_7FFFFFFF];
  Card= REF RECORD
               card1: Card32;
               pad1: Card32;
               card2: Card32;
               pad2: Card32;
        END;
  CardTest = TestElem OBJECT OVERRIDES
    new := NewCard;
    equal := CardEqual;
  END;


CONST SampleCard1 = 135;
      SampleCard2 = 799999;

PROCEDURE NewCard(<*UNUSED*> t: TestElem): REFANY =
  BEGIN
    RETURN NEW(Card,
                 card1 := SampleCard1,
                 card2 := SampleCard2);
  END NewCard;

PROCEDURE CardEqual(<*UNUSED*> t: TestElem; r: REFANY) : BOOLEAN =
  VAR c: Card := r;
  BEGIN
    RETURN (c.card1 = SampleCard1) AND
           (c.card2 = SampleCard2);
  END CardEqual;


(* char test *)

TYPE
  Char = REF RECORD
               char: BITS 32 FOR CHAR;
               pad1: Card32;
         END;
  CharTest = TestElem OBJECT OVERRIDES
    new := NewChar;
    equal := CharEqual;
  END;

CONST SampleChar = 'A';

PROCEDURE NewChar(<*UNUSED*> t: TestElem): REFANY =
  BEGIN
    RETURN NEW(Char, char := SampleChar);
  END NewChar;

PROCEDURE CharEqual(<*UNUSED*> t: TestElem; r: REFANY) : BOOLEAN =
  VAR c: Char := r;
  BEGIN
    RETURN (c.char = SampleChar);
  END CharEqual;


(* server side code *)

TYPE
  T = Test.T OBJECT OVERRIDES
    swap := DoSwap;
  END;

  <* FATAL NetObj.Error *>
  <* FATAL NetObj.Invalid *>
  <* FATAL Thread.Alerted *>
  <* FATAL IP.Error *>
  <* FATAL TCPNetObj.Failed *>

(* server-side code *)

PROCEDURE DoSwap(<*UNUSED*> t: T; r: REFANY): REFANY RAISES {NetObj.Error} =
  BEGIN
    RETURN r;
  END DoSwap;

(* client-side code *)

PROCEDURE TestIt(t: Test.T; test: TestElem) RAISES {NetObj.Error} =
  VAR arg, res: REFANY;
  BEGIN
    IO.Put(test.name & " test ... ");
    arg := test.new();
    res := t.swap(arg);
    IF res = NIL THEN
      IO.Put("failed: returned NIL\n");
    ELSIF TYPECODE(res) # TYPECODE(arg) THEN
      IO.Put("failed: different typecode\n");
    ELSIF test.equal(res) THEN
      IO.Put("ok\n");
    ELSE
      IO.Put("failed: different contents\n");
    END;
  END TestIt;


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
        TRY
          TestIt(t, NEW(TextTest, name := "text"));
          TestIt(t, NEW(TwoTextTest, name := "two text"));
          TestIt(t, NEW(TextArrayTest, name := "text array"));
          TestIt(t, NEW(SelfTest, name := "self"));
          TestIt(t, NEW(SiteTest, name := "site"));
          TestIt(t, NEW(FileInfoTest, name := "file info"));
          TestIt(t, NEW(CardTest, name := "card"));
          TestIt(t, NEW(CharTest, name := "char"));
        EXCEPT
        | NetObj.Error(x) => IO.Put("NetObj.Error(" & ErrStr(x) & ")\n");
        END;
      END;
    END;
  END;
END PickleTest.

