UNSAFE MODULE Window;

IMPORT IO;

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
IMPORT QtDynamic;
IMPORT QtPushButton;

(*
  in the callback if we specify the sender in the 
  new dynamic object as the window which allows access
  to the other fields of the window which is pretty 
  helpful however if we specify the sender as the real
  object say the push button then we cant specify the window as well.
*)

REVEAL
  Window =
    WindowPublic BRANDED OBJECT

      mainWin                              : QtMainWindow.T;
      label                                : QtLabel.T;
      menuBar                              : QtMenuBar.T;
      file, help, edit                     : QtMenu.T;
      quit, open, close                    : QtAction.T;
      about, opt0, opt1, opt2              : QtAction.T;
      cut, copy, paste                     : QtAction.T;
      icon1, icon2                         : QtIcon.T;
      tbar                                 : QtToolBar.T;
      statusBar                            : QtStatusBar.T;
      group                                : QtActionGroup.T;
      pb                                   : QtPushButton.T;
      dynClick, dynHover, dynTst, dynClick2: QtDynamic.DynamicQObject;
    METHODS
      (* slots *)
      clicked (args: ADDRESS) := MenuClicked;
      hovered (args: ADDRESS) := MenuHovered;

    OVERRIDES
      init := NewWindow;
    END;


PROCEDURE NewWindow (self: Window; <* UNUSED *> parent: QWidget): Window =
  VAR ret: BOOLEAN;
  BEGIN

    EVAL self.init_2();

    self.label := NEW(QtLabel.T).init_4("Qt Modula 3 Menu test", self);
    self.label.move(100, 100);
    self.label.setWordWrap(TRUE);
    IO.Put("label " & self.label.text() & "\n");

    self.pb := NEW(QtPushButton.T).init_2("Push Me", self);
    self.pb.move(200, 200);

    (* some icons for menu items ie actions *)
    self.icon1 := NEW(QtIcon.T).init_3("vlc.png");
    self.icon2 := NEW(QtIcon.T).init_3("rosegarden.png");

    self.quit := NEW(QtAction.T).init_1("&Quit", self);
    self.quit.setShortcut(NEW(QtKeySequence.T).init_1("CTRL+Q"));
    self.open := NEW(QtAction.T).init_2(self.icon1, "&Open", self);
    self.close := NEW(QtAction.T).init_2(self.icon2, "&Close", self);
    self.about := NEW(QtAction.T).init_1("&About", self);

    self.cut := NEW(QtAction.T).init_1("&Cut", self);
    self.copy := NEW(QtAction.T).init_1("C&opy", self);
    self.paste := NEW(QtAction.T).init_1("P&aste", self);
    self.cut.setShortcut(NEW(QtKeySequence.T).init_1("Ctrl+x"));
    self.copy.setShortcut(NEW(QtKeySequence.T).init_1("Ctrl+c"));
    self.paste.setShortcut(NEW(QtKeySequence.T).init_1("Ctrl+v"));

    (* need an actiongroup for menu radio buttons ie mutually exclusive
       menu options *)
    self.group := NEW(QtActionGroup.T).init_0(self);

    self.opt0 := NEW(QtAction.T).init_1("Report Bug", self);
    self.opt0.setCheckable(TRUE);
    self.opt1 := NEW(QtAction.T).init_1("Long Help", self);
    self.opt2 := NEW(QtAction.T).init_1("Short Help", self);
    self.opt2.setShortcut(NEW(QtKeySequence.T).init_1("Ctrl+J"));

    (* radios dont show up until setCheckable is set *)
    self.opt1.setCheckable(TRUE);
    self.opt2.setCheckable(TRUE);
    (* init one of the radios *)
    self.opt1.setChecked(TRUE);

    EVAL self.group.addAction(self.opt1);
    EVAL self.group.addAction(self.opt2);

    (* The main menu bar *)
    self.menuBar := NEW(QtMenuBar.T).init_1();
    (* add the File menu *)
    self.file := self.menuBar.addMenu1("&File");
    (* add the open close and quit menu items *)
    self.file.addAction(self.open);
    self.file.addAction(self.close);
    EVAL self.file.addSeparator();
    self.file.addAction(self.quit);

    (* add the Edit menu *)
    self.edit := self.menuBar.addMenu1("&Edit");
    (* add the cut copy and paste menu items *)
    self.edit.addAction(self.cut);
    self.edit.addAction(self.copy);
    self.edit.addAction(self.paste);

    (* add the Help menu *)
    self.help := self.menuBar.addMenu1("&Help");
    (* add checkbox menu item *)
    self.help.addAction(self.opt0);

    (* add Display submenu *)
    self.edit := self.help.addMenu1("Display");

    (* add Display submenu items *)
    self.edit.addAction(self.opt1);
    self.edit.addAction(self.opt2);

    (* add Help menut item *)
    self.help.addAction(self.about);

    (* toolbar under menubar with shortcut buttons *)
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
    self.setWindowTitle("M3 Menus Test");
    self.resize(400, 300);

    (*
      self.dynClick := NEW(QtDynamic.DynamicQObject).init_0(MenuClicked,self);
      ret := self.dynClick.connectDynamicSlot(self.quit, "triggered()", "trigger()");
      *)
    self.dynClick :=
      NEW(QtDynamic.DynamicQObject).init_0(MenuClicked, self);
    ret :=
      self.dynClick.connectDynamicSlot(self.pb, "clicked()", "name()");
    CheckReturn(ret, "dynClick and pb");

    self.dynClick2 :=
      NEW(QtDynamic.DynamicQObject).init_0(MenuTst, self); (* second slot
                                                              connection *)
    ret :=
      self.dynClick2.connectDynamicSlot(self.pb, "clicked()", "click()");
    CheckReturn(ret, "dynClick2 and pb");

    ret := self.dynClick.connectDynamicSlot(
             self.open, "activated()", "nothing()");
    CheckReturn(ret, "dynClick and open");


    self.dynHover :=
      NEW(QtDynamic.DynamicQObject).init_0(MenuHovered, self);
    ret :=
      self.dynHover.connectDynamicSlot(self.close, "hovered()", "hover()");
    CheckReturn(ret, "dynHover and close");

    self.dynTst := NEW(QtDynamic.DynamicQObject).init_0(MenuTst, self);
    ret :=
      self.dynTst.connectDynamicSignal("clicked()", self.pb, "click()");
    CheckReturn(ret, "dynTst and pb");

    IO.Put("Window created\n");
    
    RETURN self;

  END NewWindow;

PROCEDURE MenuClicked (self: ROOT; <*UNUSED*>args: ADDRESS) =
  VAR
    w  : Window;
  BEGIN

    IO.Put("In M3 callback ButtonClicked\n");
    w := NARROW(self, Window);
    w.statusBar.showMessage1("Button Clicked");
  END MenuClicked;

PROCEDURE MenuHovered (self: ROOT; <*UNUSED*>args: ADDRESS) =
  VAR
    w   : Window;
    ret : BOOLEAN;
    data: REF INTEGER;
  BEGIN
    data := NEW(REF INTEGER);
    data^ := 23;

    IO.Put("In M3 callback MenuHovered\n");
    <* ASSERT self # NIL *>

    w := NARROW(self, Window);
    ret :=
      w.dynTst.emitDynamicSignal("clicked()", LOOPHOLE(data, REF ADDRESS));
    CheckReturn(ret, "dynTst signal sent");

  END MenuHovered;

PROCEDURE MenuTst (self: ROOT; <*UNUSED*>args: ADDRESS) =
  BEGIN

    IO.Put("In M3 callback MenuTst\n");
    <* ASSERT self # NIL *>

  END MenuTst;

PROCEDURE CheckReturn (ret: BOOLEAN; msg: TEXT) =
  BEGIN
    IF ret THEN
      IO.Put(msg & " connected\n");
    ELSE
      IO.Put(msg & " NOT connected\n");
    END;
  END CheckReturn;

BEGIN
END Window.
