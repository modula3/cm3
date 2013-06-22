MODULE Main;

IMPORT IO,Fmt,Thread;
IMPORT QtApplication;

IMPORT QtBoxLayout;
IMPORT QtLayout;
IMPORT QtPushButton;
IMPORT QtSizePolicy;
IMPORT QtSize;

FROM QtWidget IMPORT QWidget;

VAR

  argc : INTEGER;
  argv : ARRAY[0..1] OF TEXT;
  app : QtApplication.T;

  window : QWidget;

  vboxLayout : QtBoxLayout.QVBoxLayout;

  settings : QtPushButton.T;
  accounts : QtPushButton.T;
  loans : QtPushButton.T;
  cash : QtPushButton.T;
  debts : QtPushButton.T;

  minSize : QtSize.T;

BEGIN

  argv[0] := "progname";
  argv[1] := "parm1";
  argc := NUMBER(argv);

  app := NEW(QtApplication.T).init_1(argc,argv);

  window := NEW(QWidget).init_widget();

  vboxLayout := NEW(QtBoxLayout.QVBoxLayout).init_0();
  vboxLayout.setSpacing(1);
  settings := NEW(QtPushButton.T).init_2("Settings",window);
  settings.setSizePolicy1(QtSizePolicy.Expanding,QtSizePolicy.Expanding);
  accounts := NEW(QtPushButton.T).init_2("Accounts",window);
  accounts.setSizePolicy1(QtSizePolicy.Expanding,QtSizePolicy.Expanding);
  loans := NEW(QtPushButton.T).init_2("Loans",window);
  loans.setSizePolicy1(QtSizePolicy.Expanding,QtSizePolicy.Expanding);
  cash := NEW(QtPushButton.T).init_2("Cash",window);
  cash.setSizePolicy1(QtSizePolicy.Expanding,QtSizePolicy.Expanding);
  debts := NEW(QtPushButton.T).init_2("Debts",window);
  debts.setSizePolicy1(QtSizePolicy.Expanding,QtSizePolicy.Expanding);

  vboxLayout.addWidget2(settings);
  vboxLayout.addWidget2(accounts);
  vboxLayout.addWidget2(loans);
  vboxLayout.addWidget2(cash);
  vboxLayout.addWidget2(debts);

  window.setLayout(vboxLayout);
  minSize := vboxLayout.minimumSize();

  IO.Put(" Min size width " & Fmt.Int(minSize.width()) &
         " Min size height " & Fmt.Int(minSize.height()) & "\n");

  window.show();

  EVAL QtApplication.Exec();

END Main.
