MODULE Main;

IMPORT IO,Thread;

IMPORT QtLabel;
IMPORT QtCheckBox;
FROM QtDateTime IMPORT QDate;
IMPORT QtCalendarWidget;
IMPORT QtMainWindow;
IMPORT QtApplication;
IMPORT QtDynamic;


VAR
  ret : BOOLEAN;
  appret : INTEGER;

  argc : INTEGER;
  argv : ARRAY[0..0] OF TEXT;
  mainWin : QtMainWindow.T;
  label : QtLabel.T;
  app : QtApplication.T;
  cb : QtCheckBox.T;
  cal : QtCalendarWidget.T;

  dyn1,dyn2,dyn3 : QtDynamic.DynamicQObject;

PROCEDURE Callback1(obj : ROOT; args : ADDRESS) =
VAR
 i : INTEGER;
BEGIN

  IO.Put("In M3 callback ");
  i := QtDynamic.ConvertInt(args,1);
  IO.PutInt(i);
  IO.Put("\n");

END Callback1;


PROCEDURE Callback2(obj : ROOT; args : ADDRESS) =
VAR
  month,year : INTEGER;
BEGIN

  IO.Put("In M3 callback2 ");

  year := QtDynamic.ConvertInt(args,1);
  month := QtDynamic.ConvertInt(args,2);
  IO.PutInt(month); IO.Put(" "); IO.PutInt(year);

  IO.Put("\n");

END Callback2;

PROCEDURE Callback3(obj : ROOT; args : ADDRESS) =
VAR
  date := NEW(QDate);
  doy : INTEGER;
BEGIN

  IO.Put("In M3 callback3 ");

(*
test new conversion rtns dont work th cxxobj is a function of it being a class not
that it descends from qobject !!!
*)
  date.cxxObj := QtDynamic.ConvertObj(args,1);
  doy := date.dayOfYear();

  IO.Put ("day of year ");
  IO.PutInt(doy);

  IO.Put("\n");

END Callback3;

BEGIN

  argc := 1;
  argv[0] := "test window";

  app := NEW(QtApplication.T).init_1(argc,argv);
  mainWin := NEW(QtMainWindow.T).init_2();
  label := NEW(QtLabel.T).init_4("Hello Modula 3",mainWin);

  cb := NEW(QtCheckBox.T).init_2("Click Me",mainWin);
  cb.move(40,40);

  cal := NEW(QtCalendarWidget.T).init_0(mainWin);
  cal.move(100,100);
  cal.resize(220,200);

  dyn1 := NEW(QtDynamic.DynamicQObject).init_0(Callback1,NIL);

  dyn2 := NEW(QtDynamic.DynamicQObject).init_0(Callback2,NIL);
  dyn3 := NEW(QtDynamic.DynamicQObject).init_0(Callback3,NIL);


  ret := dyn1.connectDynamicSlot(cb, "clicked(bool)", "slot1()");
  IF ret THEN
    IO.Put("Slot1 connected\n");
  ELSE
    IO.Put("Slot1 not connected\n");
  END;


  ret := dyn2.connectDynamicSlot(cal, "currentPageChanged(int,int)", "slot2()");
  IF ret THEN
    IO.Put("Slot2 connected\n");
  ELSE
    IO.Put("Slot2 not connected\n");
  END;

  ret := dyn3.connectDynamicSlot(cal, "clicked(QDate)", "slot3()");
  IF ret THEN
    IO.Put("Slot3 connected\n");
  ELSE
    IO.Put("Slot3 not connected\n");
  END;

  mainWin.setObjectName("mainwin");
  mainWin.resize(400,300);
  mainWin.show();

  IO.Put("before exec\n");
  appret :=  QtApplication.Exec();

  IO.Put("pausing for 1 secs\n");
  Thread.Pause(1.0D0);

END Main.
