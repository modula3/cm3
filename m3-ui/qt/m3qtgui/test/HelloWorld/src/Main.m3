MODULE Main;

IMPORT IO,Fmt,Thread,RTCollector;

FROM QtNamespace IMPORT AspectRatioMode,ConnectionType,Initialization;

IMPORT QtApplication;
IMPORT QtMainWindow;
IMPORT QtLabel;
IMPORT QtCheckBox;

VAR
  ret : INTEGER;
  argc : INTEGER;
  argv : ARRAY[0..0] OF TEXT;
  app : QtApplication.T;
  mainWin : QtMainWindow.T;
  label : QtLabel.T;
  auto_conn : ConnectionType; (*.AutoConnection;*)

BEGIN

  argc := 1;
  argv[0] := "test window";

  app := NEW(QtApplication.T).init_1(argc,argv);

  mainWin := NEW(QtMainWindow.T).init_2();

  label := NEW(QtLabel.T).init_4("Hello Modula 3",mainWin);
  label.move(100,100);

  QtApplication.AboutQt();

  mainWin.setObjectName("mainwin");
  mainWin.resize(400,300);
  mainWin.show();

  ret :=  QtApplication.Exec();

  IO.Put("pausing for 1 sec\n");
  Thread.Pause(1.0D0);

END Main.
