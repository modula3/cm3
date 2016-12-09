UNSAFE MODULE Window;

IMPORT FS, File, Process, Text, Rd, Atom, Word, OSError;
IMPORT RefSeq,TextExtras,FmtTime;

FROM QtNamespace IMPORT AlignRight, AlignVCenter, ItemIsEditable,ItemIsEnabled,NoItemFlags,ItemFlags;
FROM QtWidget IMPORT QWidget;
FROM QtLabel IMPORT QLabel;
FROM QtBoxLayout IMPORT QHBoxLayout;
FROM QtComboBox IMPORT QComboBox;
FROM QtPushButton IMPORT QPushButton;
FROM QtTableWidget IMPORT QTableWidget, QTableWidgetItem;
FROM QtGridLayout IMPORT QGridLayout;
FROM QtProgressDialog IMPORT QProgressDialog;
FROM QtAbstractItemView IMPORT SelectionBehavior;
FROM QtCoreApplication IMPORT ProcessEvents1;
FROM QtStringList IMPORT QStringList;
IMPORT QtFileDialog;
IMPORT QtSizePolicy;
IMPORT QtHeaderView;
IMPORT QtDynamic;

IMPORT IO,Fmt;

<*FATAL OSError.E *>

TYPE
  FileRec = RECORD
    name,size,modTime : TEXT;
  END;
  RefFile = REF FileRec;
  
REVEAL
  Window =
    WindowPublic BRANDED OBJECT
      currentDir : TEXT;
      fileLabel        : QLabel;
      
      fileComboBox     : QComboBox;
      textComboBox     : QComboBox;
      directoryComboBox: QComboBox;

      textLabel      : QLabel;
      directoryLabel : QLabel;
      filesFoundLabel: QLabel;
      
      browseButton   : QPushButton;
      findButton     : QPushButton;
      filesTable     : QTableWidget;
      
      dynBrowse, dynFind, dynOpen: QtDynamic.DynamicQObject;
    METHODS
      createButton     (label: TEXT): QPushButton := CreateButton;
      createComboBox   (label: TEXT): QComboBox   := CreateComboBox;
      createFilesTable ()                         := CreateFilesTable;

      findFiles (files: RefSeq.T; text: TEXT): RefSeq.T := FindFiles;
      showFiles (files: RefSeq.T)                          := ShowFiles;

      (* slots *)
      browse   (args: ADDRESS) := Browse;
      find     (args: ADDRESS) := Find;
      openFile (args: ADDRESS) := OpenFile;

    OVERRIDES
      init := NewWindow;
    END;

PROCEDURE UpdateComboBox (comboBox: QComboBox) =
  BEGIN
    IF Text.Length(comboBox.currentText()) > 0 AND
       comboBox.findText1(comboBox.currentText()) = -1 THEN
      comboBox.addItem1(comboBox.currentText());
    END
  END UpdateComboBox;

PROCEDURE Browse (self: ROOT; <* UNUSED *> args: ADDRESS) =
  VAR
    dir: TEXT;
    w  : Window;
  BEGIN
    <* ASSERT self # NIL *>
    IO.Put("In M3 callback Browse \n");

    w := NARROW(self, Window);

    (* if dont use m3 File operations to get current dir *)
    dir := QtFileDialog.GetExistingDirectory1(w, "Find Files", "/usr/bin");

    IF NOT Text.Empty(dir) THEN
      IF w.directoryComboBox.findText1(dir) = -1 THEN
        w.directoryComboBox.addItem1(dir);
        w.directoryComboBox.setCurrentIndex(
          w.directoryComboBox.findText1(dir));
      END;
    END;

    w.currentDir := dir;

  END Browse;

PROCEDURE Find (self: ROOT; <* UNUSED *> args: ADDRESS) =
  VAR
    fileName, fileSize, fileType, modTime, text, path : TEXT;
    stat : File.Status;    
    files : RefSeq.T;
    file : RefFile;
    w : Window;
  BEGIN
    <* ASSERT self # NIL *>
    IO.Put("In M3 callback Find \n");

    w := NARROW(self, Window);
    w.filesTable.setRowCount(0);

    fileName := w.fileComboBox.currentText();
    text := w.textComboBox.currentText();
    path := w.directoryComboBox.currentText();

    IO.Put("filter " & fileName & "\n");
    IO.Put("text " & text & "\n");
    IO.Put("path " & path & "\n");

    UpdateComboBox(w.fileComboBox);
    UpdateComboBox(w.textComboBox);
    UpdateComboBox(w.directoryComboBox);

    IF Text.Length(fileName) = 0 THEN fileName := "*"; END;

    VAR
      iter : FS.Iterator;
      name: TEXT;
      size : LONGINT;
    BEGIN
      files := NEW(RefSeq.T).init();
      
      TRY
        TRY
          iter := FS.Iterate(path);

          WHILE iter.nextWithStatus(name,stat) DO
            fileType := Atom.ToText(stat.type);
            IF Text.Compare(fileType,"RegularFile") = 0 THEN
              modTime := FmtTime.Short(stat.modificationTime)          ;
              size := (stat.size + 1023L) DIV 1024L;

              fileSize := Fmt.LongInt(size) & " KB";          
              file := NEW(RefFile, name := name, size := fileSize, modTime := modTime);
              files.addhi(file);
            END;
          END;
        EXCEPT
        | OSError.E => RETURN;
        END;        
      FINALLY
        iter.close();
      END
    END;

    IF Text.Length(text) > 0 THEN
IO.Put(">>>" & text & "<<<<\n");
      files := w.findFiles(files, text); 
    END;

    w.showFiles(files);

  END Find;

PROCEDURE OpenFile (self: ROOT; args: ADDRESS) =
  VAR
    fileName: TEXT;
    row, col: INTEGER;
    w       : Window;
    item    : QTableWidgetItem;
  BEGIN
    <* ASSERT self # NIL *>
    IO.Put("In M3 callback OpenFile \n");

    w := NARROW(self, Window);
    row := QtDynamic.ConvertInt(args, 1);
    col := QtDynamic.ConvertInt(args, 2);

    IO.Put("row " & Fmt.Int(row) & " col " & Fmt.Int(col) & "\n");

    item := w.filesTable.item(row, 0);
    fileName := item.text();

    IO.Put("fileName to open " & fileName & "\n");
  END OpenFile;

PROCEDURE FindFiles (self: Window; files: RefSeq.T; text: TEXT):
  RefSeq.T =
  VAR
    fileCount     : INTEGER;
    index : CARDINAL;
    file : RefFile;
    rd : Rd.T;
    line : TEXT;
    finished : BOOLEAN;
    progressDialog: QProgressDialog;
    foundFiles := NEW(RefSeq.T).init();
  BEGIN

    IO.Put("Find files containing " & text & "\n");

    progressDialog := NEW(QProgressDialog).init_1(self);
    progressDialog.setCancelButtonText("&Cancel");
    progressDialog.setRange(0, files.size());
    progressDialog.setWindowTitle("Searching Files");

    fileCount := files.size();

    FOR i := 0 TO fileCount - 1 DO
      file := files.get(i);
      
      progressDialog.setValue(i);
      progressDialog.setLabelText("Searching file number " & Fmt.Int(i+1) & " of " & Fmt.Int(fileCount) & "...");

      ProcessEvents1();
      rd := IO.OpenRead(file.name);
      finished := FALSE;
      WHILE NOT IO.EOF(rd) AND NOT finished DO
        line := IO.GetLine(rd);
        index := 0;
        IF Text.Length(line) > 0 AND TextExtras.FindSub(line,text,index) THEN
          foundFiles.addhi(file);
          finished := TRUE;
        END;
        IF progressDialog.wasCanceled() THEN EXIT; END;
      END;
      IF progressDialog.wasCanceled() THEN EXIT; END
    END;

    progressDialog.cancel();
    
    RETURN foundFiles;
  END FindFiles;

PROCEDURE ShowFiles (self: Window; files: RefSeq.T) =
  VAR
    fileCount := files.size();
    fileNameItem, sizeItem, modTimeItem : QTableWidgetItem;
    row : INTEGER;
    f : RefFile;
    flags : ItemFlags;
  BEGIN
    IO.Put("file count " & Fmt.Int(fileCount) & "\n");

    FOR i := 0 TO fileCount - 1 DO
      f := files.remlo();
      
      fileNameItem := NEW(QTableWidgetItem).init_3(f.name);
      sizeItem := NEW(QTableWidgetItem).init_3(f.size);
      sizeItem.setTextAlignment(Word.Or(AlignRight, AlignVCenter));
      modTimeItem := NEW(QTableWidgetItem).init_3(f.modTime);
      
      (* get the size flags *)
      flags := sizeItem.flags();

      (* turn off editability
      flags := Word.And(flags,Word.Not(ItemIsEditable));
      *)
      (* set editable only flag *) 
      flags := Word.Or(ItemIsEnabled,ItemIsEditable);
      sizeItem.setFlags(flags);

      (* these 2 disabled readonly *)
      fileNameItem.setFlags(NoItemFlags);
      modTimeItem.setFlags(NoItemFlags);
      
      row := self.filesTable.rowCount();

      self.filesTable.insertRow(row);
      self.filesTable.setItem(row, 0, fileNameItem);
      self.filesTable.setItem(row, 1, modTimeItem);
      self.filesTable.setItem(row, 2, sizeItem);

    END;

    self.filesFoundLabel.setText(
      "Found " & Fmt.Int(fileCount) & " files. Click to open it");
  END ShowFiles;

PROCEDURE NewWindow (self: Window; <* UNUSED *> parent: QWidget): Window =
  VAR
    buttonsLayout: QHBoxLayout;
    mainLayout   : QGridLayout;
    ret          : BOOLEAN;
  BEGIN

    EVAL self.init_2();

    (* slots *)

    self.dynBrowse := NEW(QtDynamic.DynamicQObject).init_0(Browse, self);
    self.dynFind := NEW(QtDynamic.DynamicQObject).init_0(Find, self);
    self.dynOpen := NEW(QtDynamic.DynamicQObject).init_0(OpenFile, self);

    self.browseButton := self.createButton("&Browse");
    ret := self.dynBrowse.connectDynamicSlot(
             self.browseButton, "clicked()", "browse()");
    IF ret THEN
      IO.Put("Browse Slot connected\n");
    ELSE
      IO.Put("Browse Slot not connected\n");
    END;

    self.findButton := self.createButton("&Find");
    ret := self.dynFind.connectDynamicSlot(
             self.findButton, "clicked()", "find()");
    IF ret THEN
      IO.Put("Find Slot connected\n");
    ELSE
      IO.Put("Find Slot not connected\n");
    END;

    self.fileComboBox := self.createComboBox("*");
    self.textComboBox := self.createComboBox("");
    self.directoryComboBox :=
      self.createComboBox(Process.GetWorkingDirectory());

    (*add a few items *)
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

    RETURN self;

  END NewWindow;

PROCEDURE CreateButton (<* UNUSED *> self: Window; label: TEXT):
  QPushButton =
  VAR button: QPushButton;
  BEGIN
    button := NEW(QPushButton).init_3(label);
    RETURN button;
  END CreateButton;

PROCEDURE CreateComboBox (<* UNUSED *> self: Window; text: TEXT):
  QComboBox =
  VAR cb: QComboBox;
  BEGIN
    cb := NEW(QComboBox).init_1();
    cb.setEditable(TRUE);
    cb.addItem1(text);
    cb.setSizePolicy1(QtSizePolicy.Expanding, QtSizePolicy.Preferred);
    RETURN cb;
  END CreateComboBox;

PROCEDURE CreateFilesTable (self: Window) =
  VAR
    labels          := NEW(QStringList).init_0();
    ret   : BOOLEAN;
  BEGIN

    self.filesTable := NEW(QTableWidget).init_3(0, 3);
    self.filesTable.setSelectionBehavior(SelectionBehavior.SelectRows);

    labels.append("File Name");
    labels.append("Mod Time");
    labels.append("Size");
    self.filesTable.setHorizontalHeaderLabels(labels);
    self.filesTable.horizontalHeader().setResizeMode1(
      0, QtHeaderView.Stretch);
    self.filesTable.verticalHeader().hide();
    self.filesTable.setShowGrid(TRUE);

    self.filesTable.verticalHeader().setResizeMode1(0, QtHeaderView.Fixed);
    self.filesTable.verticalHeader().setDefaultSectionSize(16);

    ret := self.dynOpen.connectDynamicSlot(
             self.filesTable, "cellActivated(int,int)", "openFile()");
    IF ret THEN
      IO.Put("OpenFile Slot connected\n");
    ELSE
      IO.Put("OpenFile Slot not connected\n");
    END;

  END CreateFilesTable;

BEGIN
END Window.
