MODULE Main;

IMPORT IO,Fmt,Thread;
IMPORT QtApplication;
FROM Window IMPORT Window;

VAR
  argc : INTEGER;
  argv : ARRAY[0..1] OF TEXT;
  app : QtApplication.T;
  window : Window;

BEGIN

  argv[0] := "progname";
  argv[1] := "parm1";
  argc := NUMBER(argv);

  app := NEW(QtApplication.T).init_1(argc,argv);
  window := NEW(Window).init();
  window.show();

  EVAL QtApplication.Exec();

END Main.