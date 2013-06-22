MODULE Main;

IMPORT IO,Fmt,Thread;
IMPORT QtApplication;
FROM Window IMPORT Window;

VAR
  argc : INTEGER;
  argv : ARRAY[0..0] OF TEXT;
  app : QtApplication.T;
  window : Window;

BEGIN

  argc := 1;
  argv[0] := "File Browse";

  app := NEW(QtApplication.T).init_1(argc,argv);
  window := NEW(Window).init();
  window.show();

  EVAL QtApplication.Exec();

END Main.
