UNSAFE MODULE Window;

IMPORT IO,Word,Fmt;

FROM QtWidget IMPORT QWidget;

IMPORT QtLabel;
IMPORT QtAction;
IMPORT QtMenuBar;
IMPORT QtMenu;

IMPORT QtPushButton;
IMPORT QtGroupBox;
IMPORT QtTabWidget;
IMPORT QtBoxLayout;
IMPORT QtDialogButtonBox;
FROM QtDialogButtonBox IMPORT Ok,Cancel;

REVEAL Window = WindowPublic BRANDED OBJECT

  menuBar : QtMenuBar.T;
  fileMenu : QtMenu.T;
  openAction : QtAction.T;
  horizontalGroupBox : QtGroupBox.T;
  buttons : ARRAY[0..3] OF QtPushButton.T;

  tabWidget : QtTabWidget.T;

  tabLabel1 : QtLabel.T;
  tabLabel2 : QtLabel.T;
  tabLabel3 : QtLabel.T;
  tabLabel4 : QtLabel.T;

  buttonBox : QtDialogButtonBox.T;

METHODS

  createTabs() := CreateTabs;
  createMenu() := CreateMenu;
  createHorizGroupBox() := CreateHorizGroupBox;

OVERRIDES
  init := New_Window;
END;

PROCEDURE CreateTabs(self : Window) =
BEGIN

  self.tabWidget := NEW(QtTabWidget.T).init_0(self);
  self.tabWidget.resize(300,300);

  self.tabLabel1 := NEW(QtLabel.T).init_4("Tab1 Content",self);
  self.tabLabel2 := NEW(QtLabel.T).init_4("Tab2 Content",self);
  self.tabLabel3 := NEW(QtLabel.T).init_4("Tab3 Content",self);
  self.tabLabel4 := NEW(QtLabel.T).init_4("Tab4 Content",self);

  EVAL self.tabWidget.addTab(self.tabLabel1,"Tab1");
  EVAL self.tabWidget.addTab(self.tabLabel2,"Tab2");
  EVAL self.tabWidget.addTab(self.tabLabel3,"Tab3");
  EVAL self.tabWidget.addTab(self.tabLabel4,"Tab4");

END CreateTabs;

PROCEDURE CreateMenu(self : Window) =
BEGIN

  self.menuBar := NEW(QtMenuBar.T).init_1();

  self.fileMenu := NEW(QtMenu.T).init_2("&File",self);
  self.openAction := self.fileMenu.addAction1("E&xit");

  EVAL self.menuBar.addMenu(self.fileMenu);

END CreateMenu;

PROCEDURE CreateHorizGroupBox(self : Window) =
VAR
  layout : QtBoxLayout.QHBoxLayout;
BEGIN

  self.horizontalGroupBox := NEW(QtGroupBox.T).init_2("Horizontal layout",self);

  layout := NEW(QtBoxLayout.QHBoxLayout).init_0();

  FOR i := 0 TO 3 DO
    self.buttons[i] := NEW(QtPushButton.T).init_3("Button " & Fmt.Int(i));
    layout.addWidget2(self.buttons[i]);
  END;

  self.horizontalGroupBox.setLayout(layout);

END CreateHorizGroupBox;

PROCEDURE New_Window(self : Window; <*UNUSED *>parent : QWidget) : Window =
VAR
  mainLayout : QtBoxLayout.QVBoxLayout;
BEGIN

  EVAL self.init_2();

  self.createMenu();
  self.createTabs();
  self.createHorizGroupBox();

(*
  self.buttonBox := NEW(QtDialogButtonBox.T).init_4(Word.Or(Ok,Cancel),Horizontal,self);
*)

  self.buttonBox := NEW(QtDialogButtonBox.T).init_0(self);
  EVAL self.buttonBox.addButton1("Button 1",3);
  EVAL self.buttonBox.addButton1("Button 2",3);

  EVAL self.buttonBox.addButton2(Ok);
  EVAL self.buttonBox.addButton2(Cancel);

  self.buttonBox.setStandardButtons(Word.Or(Ok,Cancel));

  mainLayout := NEW(QtBoxLayout.QVBoxLayout).init_1(self);

  mainLayout.setMenuBar(self.menuBar);
  mainLayout.addWidget2(self.horizontalGroupBox);
  mainLayout.addWidget2(self.tabWidget);
  mainLayout.addWidget2(self.buttonBox);

  self.setLayout(mainLayout);

  self.setObjectName("mainwin");
  self.setWindowTitle("Tabs");
  self.resize(400, 300);

  IO.Put("Window created\n");

  RETURN self;

END New_Window;


BEGIN
END Window.
