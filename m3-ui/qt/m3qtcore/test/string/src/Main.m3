MODULE Main;

IMPORT IO,Fmt,Text,Thread,RTCollector;

FROM QtNamespace IMPORT AspectRatioMode,ConnectionType,Initialization;
IMPORT QtByteArray;

IMPORT QtString;

VAR
  testCount : INTEGER;

PROCEDURE CheckText(x : TEXT; value : TEXT; test : TEXT) =
BEGIN
  IO.Put(test);
  IO.Put(" value is ");
  IO.Put(x);
  IO.Put(" should be ");
  IO.Put(value);
  IF Text.Compare(x,value) # 0 THEN IO.Put(" Problem "); END;
  IO.Put("\n");
END CheckText;

PROCEDURE CheckInt(x : INTEGER; value : INTEGER; test : TEXT) =
BEGIN
  IO.Put(test);
  IO.Put(" value is ");
  IO.PutInt(x);
  IO.Put(" should be ");
  IO.PutInt(value);
  IF x # value THEN IO.Put(" Problem "); END;
  IO.Put("\n");
END CheckInt;

PROCEDURE CheckBool(x : BOOLEAN; value : BOOLEAN; test : TEXT) =
BEGIN
  IO.Put(test);
  IO.Put(" value is ");
  IF x THEN IO.Put("True"); ELSE IO.Put("False"); END;
  IO.Put(" should be ");
  IF value THEN IO.Put("True"); ELSE IO.Put("False"); END;
  IF x # value THEN IO.Put(" Problem "); END;
  IO.Put("\n");
END CheckBool;


PROCEDURE TestQString() =
VAR
  str : QtString.T;
  ba : QtByteArray.T;
  txt,txt2 : TEXT;
BEGIN
(*
  str := NEW(QtString.T).init_1("test");
*)

  str := NEW(QtString.T).initQString("test");

  CheckInt(str.size(), 4, "qstring size");
  CheckInt(str.count(), 4, "qstring count");
  CheckInt(str.length(), 4, "qstring length");

  ba := str.toUtf8();
  CheckText(ba.data(),"test","ba.data toutf8");

  ba := str.toLocal8Bit();
  CheckText(ba.data(),"test","ba.data tolocal8bit");

  str := NEW(QtString.T).initQString("testcounttest");
  CheckInt(str.count1("test"), 2, "qstring count overlap");
(*
  str := NEW(QtString.T).init_2(5,Initialization.Uninitialized);
problems with this test segv
  ba := NEW(QtByteArray.T).init_1("stringtest");
  str := NEW(QtString.T).init_1(ba);
*)
END TestQString;

BEGIN


(*
  RTCollector.Disable();
*)

(*  RTCollector.DisableMotion();*)

  testCount := 5;

  FOR i := 1 TO testCount DO
    IO.Put("\n\niteration " & Fmt.Int(i) & "\n\n");

    TestQString();

  END;

  IO.Put("pausing for 1 secs\n");
  Thread.Pause(1.0D0);

(*  RTCollector.EnableMotion();
  RTCollector.Collect();*)

(*
  RTCollector.Enable();

  IO.Put("pausing for 2 secs\n");
  Thread.Pause(2.0D0);
*)

END Main.
