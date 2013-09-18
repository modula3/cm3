(* Copyright 1993 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PickleTest.m3 *)
(* Last modified on Thu Mar  3 10:46:09 PST 1994 by wobber *)

MODULE PickleTestWide EXPORTS Main;

IMPORT Atom, AtomList, Test, TCPNetObj, IP,
       NetObj, IO, Params, Text, Fmt, Thread;

IMPORT StubLib;

CONST TestObjName = W"PickleTestObjWide";
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

CONST SampleTextWide1 = W"this is a widechar test \xFF00\xFF0F\xFFFE\xFFFF";
CONST SampleTextWide2 = W"this is another widechar test \xFF01\xFFFF\xFFFC";
CONST SampleTextWide3 = W"this is yet another widechar test \xFF0E\xFFF0\xFFF8";

PROCEDURE MapWch (Wch: WIDECHAR): WIDECHAR = 

  CONST Lo = 16_FF00;
  CONST Hi = 16_FF0F;
  VAR ResultI: INTEGER; 
  VAR Result: WIDECHAR; 
  BEGIN 
    IF Lo <= ORD(Wch) AND ORD(Wch) <= Hi
    THEN (* In the range we map. *)  
      IF LAST(WIDECHAR) = W'\xFFFF' 
      THEN (* Replace by the Unicode substitution char. *) 
        Result := W'\xFFFD';
      ELSE (* Shift up to the top of Unicode range. *) 
        ResultI := ORD(Wch) - Hi + 16_10FFFF; 
        Result := VAL (ResultI, WIDECHAR) 
      END; 
    ELSE Result := Wch;
    END;
    RETURN Result;
  END MapWch; 

PROCEDURE MapText ( Txt : TEXT ) : TEXT = 

  TYPE StrTyp = REF ARRAY OF WIDECHAR; 
  VAR Len : CARDINAL; 
  VAR Str : StrTyp;  
  VAR Result : TEXT;

  BEGIN 
    IF LAST ( WIDECHAR ) = W'\xFFFF' 
    THEN RETURN Txt 
    ELSIF Txt = NIL 
    THEN RETURN Txt 
    ELSE 
      Len := Text . Length (Txt); 
      Str := NEW (StrTyp, Len); 
      FOR RI := 0 TO Len - 1 DO
        Str ^[RI] := MapWch (Text . GetWideChar (Txt, RI));
      END;  
      Result := Text.FromWideChars(Str^);
      RETURN Result; 
    END; 
  END MapText; 

PROCEDURE RecdTextEqual(Got: TEXT; Exp: TEXT): BOOLEAN = 

  VAR AltExp: TEXT;
  BEGIN
    IF Text.Equal(Got, Exp) 
    THEN RETURN TRUE;
    ELSE 
      AltExp := MapText(Exp);
      IF Text.Equal(Got, AltExp) 
      THEN RETURN TRUE;
      ELSE RETURN FALSE;
      END;
    END; 
  END RecdTextEqual;  

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

TYPE
  WideTextTest = TestElem OBJECT OVERRIDES
    new := NewTextWide;
    equal := TextWideEqual;
  END;

PROCEDURE NewTextWide(<*UNUSED*> t: TestElem): REFANY =
  BEGIN
    RETURN NEW(TextRef, t := MapText(SampleTextWide1))
  END NewTextWide;

PROCEDURE TextWideEqual(<*UNUSED*> t: TestElem; r: REFANY) : BOOLEAN =
  VAR tt: TextRef := r;
  BEGIN
    RETURN RecdTextEqual(tt.t, SampleTextWide1);
  END TextWideEqual;


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

TYPE
  TwoWideTextTest = TestElem OBJECT OVERRIDES
    new := NewTwoTextWide;
    equal := TwoTextWideEqual;
  END;

PROCEDURE NewTwoTextWide(<*UNUSED*> t: TestElem): REFANY =
  BEGIN
    RETURN NEW(TwoTextRef, 
               t1 := MapText(SampleTextWide1), t2 := MapText(SampleTextWide2)
              );
  END NewTwoTextWide;

PROCEDURE TwoTextWideEqual(<*UNUSED*> t: TestElem; r: REFANY) : BOOLEAN =
  VAR tt: TwoTextRef := r;
  BEGIN
    RETURN RecdTextEqual(tt.t1, SampleTextWide1) 
           AND RecdTextEqual(tt.t2, SampleTextWide2);
  END TwoTextWideEqual;


(* NIL array of TEXT. *) 

TYPE
  TextArrayNilTest = TestElem OBJECT OVERRIDES
    new := NewTextArrayNil;
    equal := TextArrayNilEqual;
  END;

PROCEDURE NewTextArrayNil(<*UNUSED*> t: TestElem): REFANY =
  VAR arg : REF ARRAY OF TEXT := NIL; 
  BEGIN
    RETURN arg;
  END NewTextArrayNil;

PROCEDURE TextArrayNilEqual(<*UNUSED*> t: TestElem; r: REFANY) : BOOLEAN =
  VAR res: REF ARRAY OF TEXT := r;
  BEGIN
    IF res = NIL THEN RETURN TRUE; 
    ELSIF NUMBER(res^) = 0 THEN 
      IO.Put ("NIL array of TEXT converted to empty" );
      RETURN FALSE;  
    ELSE RETURN FALSE;
    END; 
  END TextArrayNilEqual;

(* Empty Array of TEXT. *)

TYPE
  TextArrayEmptyTest = TestElem OBJECT OVERRIDES
    new := NewTextArrayEmpty;
    equal := TextArrayEmptyEqual;
  END;

PROCEDURE NewTextArrayEmpty(<*UNUSED*> t: TestElem): REFANY =
  VAR arg := NEW(REF ARRAY OF TEXT, 0);
  BEGIN
    RETURN arg;
  END NewTextArrayEmpty;

PROCEDURE TextArrayEmptyEqual(<*UNUSED*> t: TestElem; r: REFANY) : BOOLEAN =
  VAR res: REF ARRAY OF TEXT := r;
  BEGIN
    IF res = NIL THEN
      IO.Put ("Empty array of TEXT converted to NIL" );
      RETURN FALSE;  
    ELSIF NUMBER(res^) = 0
    THEN RETURN TRUE;  
    ELSE RETURN FALSE;
    END; 
  END TextArrayEmptyEqual;

(* Array of TEXT containing no WIDECHARs. *) 

TYPE
  TextArrayTest = TestElem OBJECT OVERRIDES
    new := NewTextArray;
    equal := TextArrayEqual;
  END;

PROCEDURE NewTextArray(<*UNUSED*> t: TestElem): REFANY =
  VAR arg := NEW(REF ARRAY OF TEXT, 3);
  BEGIN
    arg^[0] := SampleText1;
    arg^[1] := SampleText2;
    arg^[2] := SampleText3;
    RETURN arg;
  END NewTextArray;

PROCEDURE TextArrayEqual(<*UNUSED*> t: TestElem; r: REFANY) : BOOLEAN =
  VAR res: REF ARRAY OF TEXT := r;
  BEGIN
    RETURN NUMBER(res^) = 3 AND
              Text.Equal(res^[0], SampleText1) AND
              Text.Equal(res^[1], SampleText2) AND
              Text.Equal(res^[2], SampleText3);
  END TextArrayEqual;

(* Array of TEXT containing WIDECHARs. *) 

TYPE
  WideTextArrayTest = TestElem OBJECT OVERRIDES
    new := NewTextWideArray;
    equal := TextArrayWideEqual;
  END;

PROCEDURE NewTextWideArray(<*UNUSED*> t: TestElem): REFANY =
  VAR arg := NEW(REF ARRAY OF TEXT, 3);
  BEGIN
    arg^[0] := MapText(SampleTextWide1);
    arg^[1] := MapText(SampleTextWide2);
    arg^[2] := MapText(SampleTextWide3);
    RETURN arg;
  END NewTextWideArray;

PROCEDURE TextArrayWideEqual(<*UNUSED*> t: TestElem; r: REFANY) : BOOLEAN =
  VAR res: REF ARRAY OF TEXT := r;
  BEGIN
    RETURN NUMBER(res^) = 3 AND
              RecdTextEqual(res^[0], SampleTextWide1) AND
              RecdTextEqual(res^[1], SampleTextWide2) AND
              RecdTextEqual(res^[2], SampleTextWide3);
  END TextArrayWideEqual;

(* Array of TEXT containing no WIDECHARs in a record. *) 

TYPE
  TextArrayRef = REF RECORD a: REF ARRAY OF TEXT; END;

TYPE
  TextArrayInRecTest = TestElem OBJECT OVERRIDES
    new := NewTextArrayInRec;
    equal := TextArrayInRecEqual;
  END;

PROCEDURE NewTextArrayInRec(<*UNUSED*> t: TestElem): REFANY =
  VAR arg := NEW(TextArrayRef, a := NEW(REF ARRAY OF TEXT, 3));
  BEGIN
    arg.a[0] := SampleText1;
    arg.a[1] := SampleText2;
    arg.a[2] := SampleText3;
    RETURN arg;
  END NewTextArrayInRec;

PROCEDURE TextArrayInRecEqual(<*UNUSED*> t: TestElem; r: REFANY) : BOOLEAN =
  VAR res: TextArrayRef := r;
  BEGIN
    RETURN NUMBER(res.a^) = 3 AND
              Text.Equal(res.a[0], SampleText1) AND
              Text.Equal(res.a[1], SampleText2) AND
              Text.Equal(res.a[2], SampleText3);
  END TextArrayInRecEqual;

(* Array of TEXT containing WIDECHARs, in a record. *) 

TYPE
  WideTextArrayInRecTest = TestElem OBJECT OVERRIDES
    new := NewTextWideArrayInRec;
    equal := TextArrayWideInRecEqual;
  END;

PROCEDURE NewTextWideArrayInRec(<*UNUSED*> t: TestElem): REFANY =
  VAR arg := NEW(TextArrayRef, a := NEW(REF ARRAY OF TEXT, 3));
  BEGIN
    arg.a[0] := MapText(SampleTextWide1);
    arg.a[1] := MapText(SampleTextWide2);
    arg.a[2] := MapText(SampleTextWide3);
    RETURN arg;
  END NewTextWideArrayInRec;

PROCEDURE TextArrayWideInRecEqual(<*UNUSED*> t: TestElem; r: REFANY) : BOOLEAN =
  VAR res: TextArrayRef := r;
  BEGIN
    RETURN NUMBER(res.a^) = 3 AND
              RecdTextEqual(res.a[0], SampleTextWide1) AND
              RecdTextEqual(res.a[1], SampleTextWide2) AND
              RecdTextEqual(res.a[2], SampleTextWide3);
  END TextArrayWideInRecEqual;


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
    IF TYPECODE(res) # TYPECODE(arg) THEN
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

PROCEDURE DisplayProgInfo() = 
  BEGIN 
    IO.Put("Test program for netobj, using pickles.\n"); 
    IO.Put("Compiled with:\n"); 
    IO.Put("BITSIZE(INTEGER) = " & Fmt.Int(BITSIZE(INTEGER)) & "\n"); 
    IO.Put("BITSIZE(WIDECHAR) = " & Fmt.Int(BITSIZE(WIDECHAR)) & "\n"); 
  END DisplayProgInfo; 

PROCEDURE Work () = 

  VAR netobjdErrStr: AtomList.T; 

VAR i32bs : INTEGER := BYTESIZE(StubLib.Int32); 
(* ^For checking in m3gdb.  
    Stublib.Int32 is BITS 32 FOR [-16_7FFFFFFF-1..16_7FFFFFFF].
    Modula-3 requires that the BITS 32 FOR ... be respected only when the base
    type is used for a field or array element.  We have code that depends on 
    it's being respected also for scalars.  Our compiler seems to give scalars 
    the smallest of 8, 16, 32, or 64 that will hold the base type's value range.
    This is OK for this case.  Not so for general bit counts.  Even so, it's
    depending on inside knowledge of the compiler's habits. 
*) 

  BEGIN
    IF Params.Count <= 1 THEN
    (* Run as a server. *) 
      TRY
        NetObj.Export (TestObjName, NEW(T), NIL);
        IO.Put("Exported target via netobjd\n");
      EXCEPT
      | NetObj.Error (a1) =>
        netobjdErrStr := a1; 
        TRY 
          NetObj.Export (TestObjName, NEW(T), TCPNetObj.Listen(MyIPPort));
          IO.Put("Exported target at private port\n");
        EXCEPT
        | NetObj.Error(a2) => 
          IO.Put("Got NetObj.Error (");
          IO.Put(ErrStr(a1)); 
          IO.Put(") while trying to export via netobjd\n");
          IO.Put("Got NetObj.Error (");
          IO.Put(ErrStr(a2)); 
          IO.Put(") while trying to export on private port\n");
          RETURN; 
        END; 
      END;
      IO.Put("Test server running, type <CR> to terminate\n");
      TRY EVAL IO.GetLine() EXCEPT IO.Error => END;
    ELSE (* Run as a client. *) 
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
          | NetObj.Error (a1)=>
              netobjdErrStr := a1; 
              agent := TCPNetObj.Locate(IP.Endpoint{addr, MyIPPort});
              TRY 
                t := NetObj.Import(TestObjName, agent);
                IO.Put("Located target at private port\n");
              EXCEPT
              | NetObj.Error(a2) => 
                IO.Put("Got NetObj.Error (");
                IO.Put(ErrStr(a1)); 
                IO.Put(") while trying to locate target via netobjd\n");
                IO.Put("Got NetObj.Error (");
                IO.Put(ErrStr(a2)); 
                IO.Put(") while trying to locate target at private port\n");
                RETURN; 
              END; 
          END;
          TRY
            TestIt(t, NEW(TextTest, name := "text"));
            TestIt(t, NEW(TwoTextTest, name := "two text"));
            TestIt(t, NEW(WideTextTest, name := "wide text"));
            TestIt(t, NEW(TwoWideTextTest, name := "two wide text"));

            TestIt(t, NEW(TextArrayInRecTest, name := "text array in record"));
            TestIt(t, NEW(WideTextArrayInRecTest, name := "wide text array in record"));
            TestIt(t, NEW(TextArrayNilTest, name := "NIL array of text")); 
            TestIt(t, NEW(TextArrayEmptyTest, name := "empty array of text"));
            TestIt(t, NEW(TextArrayTest, name := "text array"));
            TestIt(t, NEW(WideTextArrayTest, name := "wide text array"));

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
  END Work;   

BEGIN 
  DisplayProgInfo();
  Work();
END PickleTestWide.

