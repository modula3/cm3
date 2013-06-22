UNSAFE MODULE Window;
(*
this not working since cant set the layout into a main window
althogh the filebrowse app seems to work
check it out again with another test app
*)
IMPORT IO;

FROM QtNamespace IMPORT ConnectionType;
FROM QtWidget IMPORT QWidget;

IMPORT QtMainWindow;
IMPORT QtLabel;

IMPORT QtBoxLayout;
IMPORT QtLayout;
IMPORT QtPushButton;
IMPORT QtSizePolicy;

REVEAL Window = WindowPublic BRANDED OBJECT

  mainWin : QtMainWindow.T;
  label : QtLabel.T;
  auto_conn : ConnectionType; (*.AutoConnection;*)


  settings : QtPushButton.T;
  accounts : QtPushButton.T;
  loans : QtPushButton.T;
  cash : QtPushButton.T;
  debts : QtPushButton.T;

OVERRIDES
  init := New_Window;
END;


PROCEDURE New_Window(self : Window; parent : QWidget) : Window =
VAR
  vboxLayout : QtBoxLayout.QVBoxLayout;
BEGIN

  EVAL self.init_0(parent);
(*
  self.label := NEW(QtLabel.T).init_4("Hello Modula 3",self);
  self.label.move(100,100);
*)
  vboxLayout := NEW(QtBoxLayout.QVBoxLayout).init_0();
  vboxLayout.setSpacing(1);

  self.settings := NEW(QtPushButton.T).init_2("Settings",parent);
  self.settings.setSizePolicy1(QtSizePolicy.Expanding,QtSizePolicy.Expanding);
  self.accounts := NEW(QtPushButton.T).init_2("Accounts",parent);
  self.accounts.setSizePolicy1(QtSizePolicy.Expanding,QtSizePolicy.Expanding);

  vboxLayout.addWidget2(self.settings);
  vboxLayout.addWidget2(self.accounts);

  self.setLayout(vboxLayout);

  self.setObjectName("mainwin");
  (*
  self.setWindowTitle("Menus");
  *)
  self.resize(400, 300);

  IO.Put("Window created\n");

  RETURN self;

END New_Window;


BEGIN
END Window.
