MODULE Main;

(*
  Basic test can we allocate an object. refer to
  the Namespace
*)

IMPORT IO,Fmt,Thread,RTCollector;

IMPORT QtNamespace AS NS;
IMPORT QtObject;

VAR
  testCount : INTEGER;
  parentObj : QtObject.T;

  rm : NS.AspectRatioMode;
  ct : NS.ConnectionType;
  i  : NS.Initialization;

PROCEDURE TestObject() =
VAR
  ret : INTEGER;
  str : TEXT;
  obj1 := NEW(QtObject.QObject).init_1();
  obj2 := NEW(QtObject.QObject);
BEGIN

  obj2 := NEW(QtObject.QObject).init_0(parentObj);

  IO.Put("setting object name\n");

  obj1.setObjectName("object1");
  obj2.setObjectName("object2");

  IO.Put("object name set \n");

  IO.Put("getting object name\n");
  str := obj1.objectName();
  IO.Put("object name is " & str & "\n");

END TestObject;

PROCEDURE TestObjectCrash() =
VAR
  parentObj : QtObject.T;
  obj1 : QtObject.T;
(* if you have a parent object on the stack with its child you are asking for trouble. Qt deletes all its
children when a parent is deleted. Later when we collect
the child its a segv *)
BEGIN
  parentObj := NEW(QtObject.QObject).init_1();
  obj1 := NEW(QtObject.QObject).init_0(parentObj);
  parentObj.setObjectName("parentobject");
  obj1.setObjectName("object1");
END TestObjectCrash;


BEGIN

(*
  RTCollector.Disable();
*)
(*
  RTCollector.DisableMotion();
*)


  parentObj := NEW(QtObject.QObject).init_1();

  testCount := 500;

  FOR i := 1 TO testCount DO

    IO.Put("\n\niteration " & Fmt.Int(i) & "\n\n");
    TestObject();
(*
  TestObjectCrash();
*)
  END;

  IO.Put("pausing for 1 secs\n");
  Thread.Pause(1.0D0);

  RTCollector.Collect();


(*
  RTCollector.EnableMotion();
*)
(*
  RTCollector.Enable();
*)

END Main.
