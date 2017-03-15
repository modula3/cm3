UNSAFE MODULE Window;

IMPORT IO,Fmt;

FROM QtNamespace IMPORT ConnectionType;
FROM QtWidget IMPORT QWidget;

IMPORT QtMainWindow;
IMPORT QtBoxLayout;
IMPORT QtPushButton;
IMPORT QtSizePolicy;
IMPORT QtSize;

REVEAL 
  Window = WindowPublic BRANDED OBJECT
    mainWin : QtMainWindow.T;
    settings,accounts,loans,cash,debts : QtPushButton.T;
    minSize : QtSize.T;
  OVERRIDES
    init := New_Window;
  END;

PROCEDURE New_Window(self : Window; <*UNUSED*>parent : QWidget) : Window =
  VAR
    vboxLayout : QtBoxLayout.QVBoxLayout;
  BEGIN

    EVAL self.init_2();

    vboxLayout := NEW(QtBoxLayout.QVBoxLayout).init_0();
    vboxLayout.setSpacing(1);

    self.settings := NEW(QtPushButton.T).init_3("Settings");
    self.settings.setSizePolicy1(QtSizePolicy.Expanding,QtSizePolicy.Expanding);
    self.accounts := NEW(QtPushButton.T).init_3("Accounts");
    self.accounts.setSizePolicy1(QtSizePolicy.Expanding,QtSizePolicy.Expanding);
    self.loans := NEW(QtPushButton.T).init_3("Loans");
    self.loans.setSizePolicy1(QtSizePolicy.Expanding,QtSizePolicy.Expanding);
    self.cash := NEW(QtPushButton.T).init_3("Cash");
    self.cash.setSizePolicy1(QtSizePolicy.Expanding,QtSizePolicy.Expanding);
    self.debts := NEW(QtPushButton.T).init_3("Debts");
    self.debts.setSizePolicy1(QtSizePolicy.Expanding,QtSizePolicy.Expanding);
  
    vboxLayout.addWidget2(self.settings);
    vboxLayout.addWidget2(self.accounts);
    vboxLayout.addWidget2(self.loans);
    vboxLayout.addWidget2(self.cash);
    vboxLayout.addWidget2(self.debts);

    self.setLayout(vboxLayout);
    self.minSize := vboxLayout.minimumSize();

    self.setObjectName("buttonwin");

    IO.Put(" Min size width " & Fmt.Int(self.minSize.width()) &
           " Min size height " & Fmt.Int(self.minSize.height()) & "\n");
         
    IO.Put("Window created\n");

    RETURN self;

  END New_Window;

BEGIN
END Window.
