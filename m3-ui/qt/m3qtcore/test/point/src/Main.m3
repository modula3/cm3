MODULE Main;

IMPORT IO,Fmt,Thread;

IMPORT QtPoint;
FROM QtNamespace IMPORT AspectRatioMode;

VAR
  testCount : INTEGER;

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


PROCEDURE TestPoint() =
VAR
  ret : INTEGER;
  pt1,pt2 : QtPoint.T;
  rx,ry : UNTRACED REF INTEGER;
BEGIN

  IO.Put("Empty constructor\n");
  pt1 := NEW(QtPoint.T).init_0();

  IO.Put("Normal constructor\n");
  pt1 := NEW(QtPoint.T).init_1(12,15);
  CheckInt(pt1.x(), 12, "pt1 x");
  CheckInt(pt1.y(), 15, "pt1 y");

  IO.Put("Set X Y\n");
  pt1.setX(-31);
  pt1.setY(-37);

  CheckInt(pt1.x(), -31, "pt1 x");
  CheckInt(pt1.y(), -37, "pt1 y");

  IO.Put("Plus Equals\n");
  pt1 := NEW(QtPoint.T).init_1(10,20);
  pt2 := NEW(QtPoint.T).init_1(5,5);
  pt1 := pt1.PlusEqual(pt2);

  CheckInt(pt1.x(), 15, "pt1 x");
  CheckInt(pt1.y(), 25, "pt1 y");

  IO.Put("Multiply Equals\n");
  pt1 := NEW(QtPoint.T).init_1(10,20);
  pt1 := pt1.MultiplyEqual(2.0D0);

  CheckInt(pt1.x(), 20, "pt1 x");
  CheckInt(pt1.y(), 40, "pt1 y");

  IO.Put("Divide Equals\n");
  pt1 := NEW(QtPoint.T).init_1(10,20);
  pt1 := pt1.DivideEqual(2.2D0);

  CheckInt(pt1.x(), 5, "pt1 x");
  CheckInt(pt1.y(), 9, "pt1 y");

  IO.Put("manhattan length\n");
  pt1 := NEW(QtPoint.T).init_1(10,20);
  ret := pt1.manhattanLength();

  CheckInt(ret, 30, "manhattan length");

  IO.Put("Set X Y\n");
  rx := pt1.rx();
  ry := pt1.ry();
  CheckInt(rx^, 10, "pt1 x");
  CheckInt(ry^, 20, "pt1 y");

END TestPoint;

BEGIN

  testCount := 5;

  FOR i := 1 TO testCount DO

    IO.Put("\n\niteration " & Fmt.Int(i) & "\n\n");

    TestPoint();

  END;

  IO.Put("pausing for 1 secs\n");
  Thread.Pause(1.0D0);

END Main.
