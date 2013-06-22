UNSAFE MODULE Window;

IMPORT IO,Text;

FROM QtWidget IMPORT QWidget;
FROM QtLabel IMPORT QLabel;
FROM QtBoxLayout IMPORT QHBoxLayout;
FROM QtComboBox IMPORT QComboBox;
FROM QtPushButton IMPORT QPushButton;
FROM QtTableWidget IMPORT QTableWidget;
FROM QtGridLayout IMPORT QGridLayout;
FROM QtAbstractItemView IMPORT SelectionBehavior;
IMPORT QtFileDialog;
IMPORT QtSizePolicy;

IMPORT QtHeaderView;
FROM QtStringList IMPORT QStringList;

IMPORT QtDynamic;

VAR
  thisWindow : Window;

PROCEDURE UpdateComboBox(comboBox : QComboBox) =
BEGIN
  IF (comboBox.findText1(comboBox.currentText()) = -1) THEN
    comboBox.addItem1(comboBox.currentText());
  END
END UpdateComboBox;

REVEAL Window = WindowPublic BRANDED OBJECT

(*
fixme implement these to complete the test
    QStringList findFiles(const QStringList &files, const QString &text);
    void showFiles(const QStringList &files);

*)

  fileLabel : QLabel;
  fileComboBox : QComboBox;
  textComboBox : QComboBox;
  directoryComboBox : QComboBox;

  textLabel : QLabel;
  directoryLabel : QLabel;
  filesFoundLabel : QLabel;
  browseButton : QPushButton;
  findButton : QPushButton;
  filesTable : QTableWidget;

(*
  currentDir : QDir;
*)
  dynBrowse,dynFind : QtDynamic.DynamicQObject;

METHODS

  createButton(label : TEXT) : QPushButton := CreateButton;
  createComboBox(label : TEXT) : QComboBox := CreateComboBox;
  createFilesTable() := CreateFilesTable;

OVERRIDES
  init := New_Window;
END;

PROCEDURE Browse(args,obj : ADDRESS) =
VAR
  dir : TEXT;
BEGIN

  IO.Put("In M3 callback Browse ");
  IO.Put("\n");

(*
  if the sigslots dyn object were modified so that it used a method sig
  then the first parm is an object maybe its the sender??
  then this proc would be a method and we wouldnt have to use the thisWindow
  kludge but use the self parm passed in in the callback.
*)

  (* if dont use m3 File operations to get current dir
  dir := QtFileDialog.GetExistingDirectory4();
  *)
  dir := QtFileDialog.GetExistingDirectory1(thisWindow,"Find Files","/usr/bin");

  IF NOT Text.Empty(dir) THEN
    IF thisWindow.directoryComboBox.findText1(dir) = -1 THEN
      thisWindow.directoryComboBox.addItem1(dir);
      thisWindow.directoryComboBox.setCurrentIndex(thisWindow.directoryComboBox.findText1(dir));
    END;
  END;

END Browse;

PROCEDURE Find(args,obj : ADDRESS) =
BEGIN

  IO.Put("In M3 callback Find ");
  IO.Put("\n");

END Find;

PROCEDURE New_Window(self : Window; parent : QWidget) : Window =
VAR
  buttonsLayout : QHBoxLayout;
  mainLayout : QGridLayout;
  ret : BOOLEAN;
BEGIN

  EVAL self.init_2();

(* functions to be created *)

  self.dynBrowse := NEW(QtDynamic.DynamicQObject).init_0(Browse,NIL);
  self.dynFind := NEW(QtDynamic.DynamicQObject).init_0(Find,NIL);

  self.browseButton := self.createButton("&Browse");
  ret := self.dynBrowse.connectDynamicSlot(self.browseButton, "clicked()", "browse()");
  IF ret THEN
    IO.Put("Browse Slot connected\n");
  ELSE
    IO.Put("Browse Slot not connected\n");
  END;

  self.findButton := self.createButton("&Find");
  ret := self.dynFind.connectDynamicSlot(self.findButton, "clicked()", "find()");
  IF ret THEN
    IO.Put("Find Slot connected\n");
  ELSE
    IO.Put("Find Slot not connected\n");
  END;

  self.fileComboBox := self.createComboBox("*");
  self.textComboBox := self.createComboBox("");
  self.directoryComboBox := self.createComboBox(""); (*QDir::currentPath());*)

  (*test adding items *)
  self.fileComboBox.addItem1("item 1");
  self.fileComboBox.addItem1("item 2");
  self.fileComboBox.addItem1("item 3");

  self.fileLabel := NEW(QLabel).init_5("Named:");
  self.textLabel := NEW(QLabel).init_5("Containing text:");
  self.directoryLabel := NEW(QLabel).init_5("In directory:");
  self.filesFoundLabel := NEW(QLabel).init_2();

  self.createFilesTable();

  buttonsLayout := NEW(QHBoxLayout).init_0();
  buttonsLayout.addStretch1();
  buttonsLayout.addWidget2(self.findButton);

  mainLayout := NEW(QGridLayout).init_1();
  mainLayout.addWidget2(self.fileLabel, 0, 0);
  mainLayout.addWidget4(self.fileComboBox, 0, 1, 1, 2);
  mainLayout.addWidget2(self.textLabel, 1, 0);
  mainLayout.addWidget4(self.textComboBox, 1, 1, 1, 2);
  mainLayout.addWidget2(self.directoryLabel, 2, 0);
  mainLayout.addWidget2(self.directoryComboBox, 2, 1);
  mainLayout.addWidget2(self.browseButton, 2, 2);
  mainLayout.addWidget4(self.filesTable, 3, 0, 1, 3);
  mainLayout.addWidget4(self.filesFoundLabel, 4, 0, 1, 3);
  mainLayout.addLayout3(buttonsLayout, 5, 0, 1, 3);


  self.setLayout(mainLayout);

  self.setWindowTitle("Find Files");
  self.resize(700, 300);

(* kludge because the browse and find callbacks are not methods yet *)

  thisWindow := self;

  RETURN self;

END New_Window;

PROCEDURE CreateButton(self : Window; label : TEXT) : QPushButton =
VAR
  button : QPushButton;
BEGIN
  button := NEW(QPushButton).init_3(label);
  RETURN button;
END CreateButton;

PROCEDURE CreateComboBox(<* UNUSED *>self : Window; text : TEXT) : QComboBox =
VAR
  cb : QComboBox;
BEGIN
  cb := NEW(QComboBox).init_1();
  cb.setEditable(TRUE);
  cb.addItem1(text);
  cb.setSizePolicy1(QtSizePolicy.Expanding,QtSizePolicy.Preferred);

  RETURN cb;
END CreateComboBox;


PROCEDURE CreateFilesTable(self : Window) =
VAR
  labels := NEW(QStringList).init_0();
BEGIN

  self.filesTable := NEW(QTableWidget).init_3(0, 2);
  self.filesTable.setSelectionBehavior(SelectionBehavior.SelectRows);

  labels.append("File Name");
  labels.append("Size");
  self.filesTable.setHorizontalHeaderLabels(labels);
  self.filesTable.horizontalHeader().setResizeMode1(0, QtHeaderView.Stretch);
  self.filesTable.verticalHeader().hide();
  self.filesTable.setShowGrid(FALSE);

(*
    connect(filesTable, SIGNAL(cellActivated(int,int)),
            this, SLOT(openFileOfItem(int,int)));
*)


END CreateFilesTable;


BEGIN
END Window.
