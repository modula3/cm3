UNSAFE MODULE Window;

IMPORT IO;

FROM QtNamespace IMPORT ConnectionType;
FROM QtWidget IMPORT QWidget;

IMPORT QtMainWindow;
IMPORT QtLabel;
IMPORT QtAction;
IMPORT QtMenuBar;
IMPORT QtMenu;
IMPORT QtIcon;
IMPORT QtToolBar;
IMPORT QtKeySequence;
IMPORT QtStatusBar;
IMPORT QtActionGroup;


REVEAL Window = WindowPublic BRANDED OBJECT

  mainWin : QtMainWindow.T;
  label : QtLabel.T;
  menuBar : QtMenuBar.T;
  file : QtMenu.T;
  help : QtMenu.T;
  edit : QtMenu.T;
  quit : QtAction.T;
  open : QtAction.T;
  close : QtAction.T;
  about,opt0,opt1,opt2 : QtAction.T;
  icon1,icon2 : QtIcon.T;
  tbar : QtToolBar.T;
  statusBar : QtStatusBar.T;
  group : QtActionGroup.T;
  auto_conn : ConnectionType; (*.AutoConnection;*)


OVERRIDES
  init := New_Window;
END;


PROCEDURE New_Window(self : Window; <*UNUSED *>parent : QWidget) : Window =
BEGIN

  EVAL self.init_2();

  self.label := NEW(QtLabel.T).init_4("Hello Modula 3",self);
  self.label.move(100,100);

  (* some icons for menu items ie actions *)
  self.icon1 := NEW(QtIcon.T).init_3("vlc.png");
  self.icon2 := NEW(QtIcon.T).init_3("rosegarden.png");

  self.quit := NEW(QtAction.T).init_1("&Quit",self);
  self.quit.setCheckable(TRUE);
  self.quit.setShortcut(NEW(QtKeySequence.T).init_1("CTRL+Q"));
  self.open := NEW(QtAction.T).init_2(self.icon1,"&Open",self);
  self.close := NEW(QtAction.T).init_2(self.icon2,"&Close",self);
  self.about := NEW(QtAction.T).init_1("&About",self);


  (* need an actiongroup for menu radio buttons ie mutually
     exclusive menu options *)
  self.group := NEW(QtActionGroup.T).init_0(self);

  self.opt0 := NEW(QtAction.T).init_1("Option Number 0",self);
  self.opt1 := NEW(QtAction.T).init_1("Option Number 1",self);
  self.opt2 := NEW(QtAction.T).init_1("Option 2",self);
  self.opt2.setShortcut(NEW(QtKeySequence.T).init_1("Ctrl+J"));

  (* radios dont show up until setCheckable is set *)
  self.opt1.setCheckable(TRUE);
  self.opt2.setCheckable(TRUE);
  (* init one of the radios *)
  self.opt1.setChecked(TRUE);

  EVAL self.group.addAction(self.opt1);
  EVAL self.group.addAction(self.opt2);


  self.menuBar := NEW(QtMenuBar.T).init_1();
  self.file := self.menuBar.addMenu1("&File");
  self.file.addAction(self.open);
  self.file.addAction(self.close);
  EVAL self.file.addSeparator();
  self.file.addAction(self.quit);

  self.help := self.menuBar.addMenu1("&Help");
  self.help.addAction(self.opt0);

  (* submenu *)
  self.edit := self.help.addMenu1("Edit");

  self.edit.addAction(self.opt1);
  self.edit.addAction(self.opt2);

  (* last help menu item *)
  self.help.addAction(self.about);

  (* toolbar under meni *)
  self.tbar := NEW(QtToolBar.T).init_1("title");
  self.tbar.addAction(self.open);
  self.tbar.addAction(self.close);

  (* statusbar on bottom *)
  self.statusBar := NEW(QtStatusBar.T).init_1();
  self.statusBar.showMessage1("Ready");

  (* add all the bars *)
  self.setMenuBar(self.menuBar);
  self.setStatusBar(self.statusBar);
  self.addToolBar1(self.tbar);


  self.setObjectName("mainwin");
  self.setWindowTitle("Menus");
  self.resize(400, 300);

  IO.Put("Window created\n");

  RETURN self;

END New_Window;


BEGIN
END Window.
