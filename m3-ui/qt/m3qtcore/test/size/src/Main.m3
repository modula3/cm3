MODULE Main;

IMPORT IO,Fmt,Thread;

IMPORT QtSize;
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

PROCEDURE CheckLong(x : LONGREAL; value : LONGREAL; test : TEXT) =
BEGIN
  IO.Put(test);
  IO.Put(" value is ");
  IO.Put(Fmt.LongReal(x));
  IO.Put(" should be ");
  IO.Put(Fmt.LongReal(value));
  IF x # value THEN IO.Put(" Problem "); END;

  IO.Put("\n");
END CheckLong;

PROCEDURE TestSize() =
VAR
  ret : INTEGER;
  size1,size2,size3 : QtSize.QSize;

BEGIN

  IO.Put("Empty constructor\n");

  size1 := NEW(QtSize.T).init_0();
  CheckBool(size1.isValid(), FALSE, "Size1 isvalid");
  CheckBool(size1.isNull(), FALSE, "Size1 isNull");
  CheckBool(size1.isEmpty(), TRUE, "Size1 isEmpty");

  IO.Put("Positive width height\n");
  size1.setWidth(20);
  size1.setHeight(30);
  CheckInt(size1.width(), 20, "Size1 width");
  CheckInt(size1.height(), 30, "Size1 height");
  CheckBool(size1.isValid(), TRUE, "Size1 isvalid");
  CheckBool(size1.isNull(), FALSE, "Size1 isNull");
  CheckBool(size1.isEmpty(), FALSE, "Size1 isEmpty");

  IO.Put("Zero width height IsNull true\n");
  size1.setWidth(0);
  size1.setHeight(0);
  CheckInt(size1.width(), 0, "Size1 width");
  CheckInt(size1.height(), 0, "Size1 height");
  CheckBool(size1.isValid(), TRUE, "Size1 isvalid");
  CheckBool(size1.isNull(), TRUE, "Size1 isNull");
  CheckBool(size1.isEmpty(), TRUE, "Size1 isEmpty");

  IO.Put("Zero width positive height IsEmpty true\n");
  size1.setWidth(0);
  size1.setHeight(1);
  CheckInt(size1.width(), 0, "Size1 width");
  CheckInt(size1.height(), 1, "Size1 height");
  CheckBool(size1.isValid(), TRUE, "Size1 isvalid");
  CheckBool(size1.isNull(), FALSE, "Size1 isNull");
  CheckBool(size1.isEmpty(), TRUE, "Size1 isEmpty");

  IO.Put("Positive width negatvie height IsEmpty true\n");
  size1.setWidth(1);
  size1.setHeight(-1);
  CheckInt(size1.width(), 1, "Size1 width");
  CheckInt(size1.height(), -1, "Size1 height");
  CheckBool(size1.isValid(), FALSE, "Size1 isvalid");
  CheckBool(size1.isNull(), FALSE, "Size1 isNull");
  CheckBool(size1.isEmpty(), TRUE, "Size1 isEmpty");

  IO.Put("Positive constructor\n");
  size1 := NEW(QtSize.T).init_1(8,12);

  CheckInt(size1.width(), 8, "Size1 width");
  CheckInt(size1.height(), 12, "Size1 height");
  CheckBool(size1.isValid(), TRUE, "Size1 isvalid");
  CheckBool(size1.isNull(), FALSE, "Size1 isNull");
  CheckBool(size1.isEmpty(), FALSE, "Size1 isEmpty");

  IO.Put("Transpose\n");
  size2 := NEW(QtSize.T).init_1(100,200);
  size2.transpose();

  CheckInt(size2.width(), 200, "Size2 width");
  CheckInt(size2.height(), 100, "Size2 height");
  CheckBool(size2.isValid(), TRUE, "Size2 isvalid");
  CheckBool(size2.isNull(), FALSE, "Size2 isNull");
  CheckBool(size2.isEmpty(), FALSE, "Size2 isEmpty");

  IO.Put("Expanding\n");
  size1 := NEW(QtSize.QSize).init_1(20,10);
  size2 := NEW(QtSize.QSize).init_1(10,20);

  size3 := size1.expandedTo(size2);

  CheckInt(size3.width(), 20, "Size3 width");
  CheckInt(size3.height(), 20, "Size3 height");

  IO.Put("Bounding\n");
  size1 := NEW(QtSize.QSize).init_1(20,10);
  size2 := NEW(QtSize.QSize).init_1(10,20);

  size3 := size1.boundedTo(size2);

  CheckInt(size3.width(), 10, "Size3 width");
  CheckInt(size3.height(), 10, "Size3 height");

  IO.Put("scaling keep aspect ratio \n");
  size1 := NEW(QtSize.QSize).init_1(10,12);

  size1.scale(60,60,AspectRatioMode.KeepAspectRatio);

  CheckInt(size1.width(), 50, "Size1 width");
  CheckInt(size1.height(), 60, "Size1 height");
  CheckBool(size1.isValid(), TRUE, "Size1 isvalid");

  IO.Put("scaling ignore aspect ratio \n");
  size1 := NEW(QtSize.QSize).init_1(10,12);
  size1.scale(60,60,AspectRatioMode.IgnoreAspectRatio);

  CheckInt(size1.width(), 60, "Size1 width");
  CheckInt(size1.height(), 60, "Size1 height");
  CheckBool(size1.isValid(), TRUE, "Size1 isvalid");

  IO.Put("scaling with object keep aspect ratio \n");
  size1 := NEW(QtSize.QSize).init_1(5,12);
  size2 := NEW(QtSize.QSize).init_1(100,120);

  size1.scale1(size2,AspectRatioMode.KeepAspectRatio);

  CheckInt(size1.width(), 50, "Size1 width");
  CheckInt(size1.height(), 120, "Size1 height");
  CheckBool(size1.isValid(), TRUE, "Size1 isvalid");

  IO.Put("plus an object \n");
  size1 := NEW(QtSize.QSize).init_1(5,12);
  size2 := NEW(QtSize.QSize).init_1(100,120);

  size1 := size1.PlusEqual(size2);

  CheckInt(size1.width(), 105, "Size1 width");
  CheckInt(size1.height(), 132, "Size1 height");
  CheckBool(size1.isValid(), TRUE, "Size1 isvalid");

  IO.Put("minus an object \n");
  size1 := NEW(QtSize.QSize).init_1(50,120);
  size2 := NEW(QtSize.QSize).init_1(30,80);

  size1 := size1.MinusEqual(size2);

  CheckInt(size1.width(), 20, "Size1 width");
  CheckInt(size1.height(), 40, "Size1 height");
  CheckBool(size1.isValid(), TRUE, "Size1 isvalid");

  IO.Put("times an object \n");
  size1 := NEW(QtSize.QSize).init_1(50,120);

  size1 := size1.MultiplyEqual(2.0D0);

  CheckInt(size1.width(), 100, "Size1 width");
  CheckInt(size1.height(), 240, "Size1 height");
  CheckBool(size1.isValid(), TRUE, "Size1 isvalid");

END TestSize;

PROCEDURE TestSizeF() =
VAR
  ret : INTEGER;
  size1,size2,size3 : QtSize.QSizeF;

BEGIN

  IO.Put("Empty constructor for sizef \n");

  size1 := NEW(QtSize.QSizeF).init_0();
  CheckBool(size1.isValid(), FALSE, "Size1 isvalid");
  CheckBool(size1.isNull(), FALSE, "Size1 isNull");
  CheckBool(size1.isEmpty(), TRUE, "Size1 isEmpty");

  IO.Put("Positive width height\n");
  size1.setWidth(20.0D0);
  size1.setHeight(30.0D0);
  CheckLong(size1.width(), 20.0D0, "Size1 width");
  CheckLong(size1.height(), 30.0D0, "Size1 height");
  CheckBool(size1.isValid(), TRUE, "Size1 isvalid");
  CheckBool(size1.isNull(), FALSE, "Size1 isNull");
  CheckBool(size1.isEmpty(), FALSE, "Size1 isEmpty");

END TestSizeF;

BEGIN

  testCount := 5;

  FOR i := 1 TO testCount DO

    IO.Put("\n\niteration " & Fmt.Int(i) & "\n\n");

    TestSize();
    TestSizeF();

  END;

  IO.Put("pausing for 1 secs\n");
  Thread.Pause(1.0D0);

END Main.
