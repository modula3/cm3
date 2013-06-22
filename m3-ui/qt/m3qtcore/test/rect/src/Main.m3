MODULE Main;

IMPORT IO,Fmt,Thread;

IMPORT QtRect,QtPoint,QtSize;

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


PROCEDURE TestRect() =
VAR
  ret : INTEGER;
  r1 : QtRect.T;
  pt1,pt2 : QtPoint.T;
  size1 : QtSize.T;
BEGIN

  IO.Put("Empty constructor\n");
  r1 := NEW(QtRect.T).init_0();
  CheckBool(r1.isValid(), TRUE, "R1 isvalid");
  CheckBool(r1.isNull(), FALSE, "R1 isNull");
  CheckBool(r1.isEmpty(), FALSE, "R1 isEmpty");

  IO.Put("Normal constructor left top width height\n");
  r1 := NEW(QtRect.T).init(1,2,3,4);
  CheckBool(r1.isValid(), TRUE, "R1 isvalid");
  CheckBool(r1.isNull(), FALSE, "R1 isNull");
  CheckBool(r1.isEmpty(), FALSE, "R1 isEmpty");
  CheckInt(r1.left(), 1, "r1 left");
  CheckInt(r1.top(), 2, "r1 top");
  CheckInt(r1.right(), 3, "r1 right");
  CheckInt(r1.bottom(), 5, "r1 bottom");

  IO.Put("Normal constructor 2 points\n");
  pt1 := NEW(QtPoint.T).init_1(10,20);
  pt2 := NEW(QtPoint.T).init_1(5,5);
  r1 := NEW(QtRect.T).init_1(pt1,pt2);

  IO.Put("Normal constructor point and size\n");
  pt1 := NEW(QtPoint.T).init_1(10,20);
  size1 := NEW(QtSize.T).init_1(8,12);
  r1 := NEW(QtRect.T).init_2(pt1,size1);

  IO.Put("Set X Y\n");
  r1.setX(-31);
  r1.setY(-37);

  CheckInt(r1.x(), -31, "r1 x");
  CheckInt(r1.y(), -37, "r1 y");

  IO.Put("Empty constructor\n");
  r1 := NEW(QtRect.T).init_0();
  r1.setLeft(4);
  r1.setRight(4);
  r1.setTop(4);
  r1.setBottom(4);
  CheckBool(r1.isValid(), TRUE, "R1 isvalid");
  CheckBool(r1.isNull(), FALSE, "R1 isNull");
  CheckBool(r1.isEmpty(), FALSE, "R1 isEmpty");
  CheckInt(r1.left(), 4, "r1 left");
  CheckInt(r1.top(), 4, "r1 top");
  CheckInt(r1.right(), 4, "r1 right");
  CheckInt(r1.bottom(), 4, "r1 bottom");


  pt1 := r1.topLeft();
  pt1 := r1.bottomRight();
  pt1 := r1.topRight();
  pt1 := r1.bottomLeft();

  r1.moveLeft(6);
  r1.moveRight(6);
  r1.moveTop(6);
  r1.moveBottom(6);

  ret := r1.width();
  ret := r1.height();
  size1 := r1.size();

END TestRect;

BEGIN

  testCount := 5;

  FOR i := 1 TO testCount DO

    IO.Put("\n\niteration " & Fmt.Int(i) & "\n\n");

    TestRect();

  END;

  IO.Put("pausing for 1 secs\n");
  Thread.Pause(1.0D0);

END Main.
