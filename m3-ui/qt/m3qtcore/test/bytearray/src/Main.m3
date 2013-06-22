MODULE Main;

IMPORT IO,Fmt,Text,Thread,RTCollector;

FROM QtNamespace IMPORT Initialization;
IMPORT QtByteArray;

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

PROCEDURE TestByteArray() =
VAR
  ret : INTEGER;
  txt : TEXT;
  ba,ba2  : QtByteArray.T;

BEGIN

  IO.Put("Start ByteArray tests \n");

  ba := NEW(QtByteArray.T).init_0();
  txt := ba.data1();
  CheckText(txt,"","init 0");
  CheckBool(ba.isEmpty(),TRUE,"ba isempty");

  ba := NEW(QtByteArray.T).init_1("long string");
  txt := ba.data1();
  CheckText(txt,"long string","init 1");

  ba := NEW(QtByteArray.T).init_2("len4",3);
  txt := ba.data1();
  CheckText(txt,"len","init 2");

  ba := NEW(QtByteArray.T).init_3(4,'A');
  txt := ba.data1();
  CheckText(txt,"AAAA","init 3");

  ba := NEW(QtByteArray.T).init_4(2,Initialization.Uninitialized);
  txt := ba.data1();
  CheckText(txt,"??","init 4 uninitialised garbage out");

  ba := NEW(QtByteArray.T).init_1("string1");
  txt := ba.data1();
  CheckText(txt,"string1","init 1");
  ba2 := NEW(QtByteArray.T).init_5(ba);
  txt := ba2.data1();
  CheckText(txt,"string1","init 5");

  ba := NEW(QtByteArray.T).init_1("123456");
  CheckInt(ba.size(), 6, "txt size");
  ba.resize(10);
  CheckInt(ba.size(), 10, "resize to 10");

  ba := NEW(QtByteArray.T).init_1("abcdef");
  ba := ba.toUpper();
  txt := ba.data1();
  CheckText(txt,"ABCDEF","toupper");
  CheckInt(ba.capacity(), 6, "capacity");

  ba := NEW(QtByteArray.T).init_1("abcdef");
  ba.truncate(3);
  txt := ba.data1();
  CheckText(txt,"abc","truncate");

  ba := NEW(QtByteArray.T).init_0();
  EVAL ba.fill('Z',4);
  txt := ba.data1();
  CheckText(txt,"ZZZZ","fill");

  ba := NEW(QtByteArray.T).init_1("abcdef");
  ret := ba.count1("test");
  txt := ba.data1();
  CheckText(txt,"abcdef","count");
  CheckInt(ret, 6, "count1");

END TestByteArray;


BEGIN

(*
  RTCollector.Disable();
*)

(*  RTCollector.DisableMotion();*)

  testCount := 5;

  FOR i := 1 TO testCount DO
    IO.Put("\n\niteration " & Fmt.Int(i) & "\n\n");

    TestByteArray();

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
