(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Dec 27 08:08:29 PST 1994 by kalsow     *)
(*      modified on Thu Nov  3 15:08:20 PST 1994 by isard      *)

MODULE Main;

IMPORT FileRd, Wr, Lex, Text, TextF, IO, Fmt, WinBase, Time, FS;
IMPORT WinDef, WinUser, Process, Rd, OSError, Stdio, Thread, FloatMode;
IMPORT M3ID, M3Loader, M3LoaderDebug AS Debug;

PROCEDURE error (msg: TEXT) =
  <* FATAL Thread.Alerted, Wr.Failure *>
  BEGIN
    Wr.PutText(Stdio.stderr, msg & "\n");
  END error;

PROCEDURE close_file (file: FileRd.T; name: TEXT) =
  BEGIN
    TRY
      Rd.Close(file);
    EXCEPT
      Rd.Failure, Thread.Alerted =>
        error("Error closing " & name);
    END
  END close_file;

PROCEDURE read_list (filename: TEXT): BOOLEAN =
  VAR
    filerd    : FileRd.T;
    objname,
    exename,
    subsystem : TEXT;
    newset    := ObjSet { table := NEW(REF ARRAY OF ObjInstance, 32) };
    item,
    stamp,
    place     : INTEGER;
    success   := TRUE;
  BEGIN
    TRY
      filerd := FileRd.Open(filename);
    EXCEPT
      OSError.E =>
        error("Can't open file " & filename);
        RETURN FALSE;
    END;

    TRY
      exename := Lex.Scan(filerd);
      Lex.Skip(filerd);
    EXCEPT
      Thread.Alerted, Rd.Failure =>
        error("Error reading file " & filename);
        close_file(filerd, filename);
        RETURN FALSE;
    END;

    IF Text.Equal(Text.Sub(exename, 0, 5), "-out:") THEN
      exename := Text.Sub(exename, 5);
    ELSE
      error("Unrecognised target name " & exename);
      close_file(filerd, filename);
      RETURN FALSE;
    END;

    IF Text.Equal(Text.Sub(exename, Text.Length(exename)-4), ".exe") THEN
      exename := Text.Sub(exename, 0, Text.Length(exename)-4);
    END;

    newset.exename := exename;

    TRY
      subsystem := Lex.Scan(filerd);
    EXCEPT
      Thread.Alerted, Rd.Failure =>
        error("Error reading file " & filename);
        close_file(filerd, filename);
        RETURN FALSE;
    END;

    IF Text.Equal(subsystem, "-subsystem:console") THEN
      newset.console := TRUE;
      Debug.Txt("Subsystem:console\n");
    ELSIF Text.Equal(subsystem, "-subsystem:windows") THEN
      newset.console := FALSE;
      Debug.Txt("Subsystem:windows\n");
    ELSE
      error("Unknown subsystem " & subsystem);
      close_file(filerd, filename);
      RETURN FALSE;
    END;

    item := 0;

    REPEAT
      TRY
        Lex.Skip(filerd);
        objname := Lex.Scan(filerd);
      EXCEPT
        Thread.Alerted, Rd.Failure =>
          error("Error reading file " & filename);
          close_file(filerd, filename);
          RETURN FALSE;
      END;
      IF Text.Length(objname) > 0 THEN
        Debug.Txt(objname);
        Debug.Txt(" ");
        TRY
          stamp := Lex.Int(filerd);
        EXCEPT
          FloatMode.Trap, Lex.Error, Thread.Alerted, Rd.Failure =>
            error("Error reading file " & filename);
            close_file(filerd, filename);
            RETURN FALSE;
        END;
        Debug.Int(stamp);
        Debug.NL();
        IF item = NUMBER(newset.table^) THEN
          expand_table(newset);
        END;
        newset.table[item] := ObjInstance { name := M3ID.Add(objname),
                                            stamp := stamp};
        INC(item);
      END
    UNTIL Text.Length(objname) = 0;

    close_file(filerd, filename);

    newset.size := item;

    FOR item := 0 TO set.size-1 DO
      place := in_table(newset, set.table[item].name);
      IF place # -1 THEN
        IF newset.table[place].stamp > set.table[item].stamp THEN
          unload(set, item);
        END
      ELSE
        unload(set, item);
      END
    END;

    item := 0;
    WHILE item < newset.size DO
      place := in_table(set, newset.table[item].name);
      IF place # -1 THEN
        IF newset.table[item].stamp > set.table[place].stamp THEN
          IF NOT load(newset, item) THEN
            remove_from_set(newset, item);
            DEC(item);
            success := FALSE;
          END
        ELSE
          newset.table[item] := set.table[place];
        END
      ELSE
        IF NOT load(newset, item) THEN
          remove_from_set(newset, item);
          DEC(item);
          success := FALSE;
        END
      END;
      INC(item);
    END;

    set := newset;

    RETURN success;
  END read_list;

PROCEDURE remove_from_set(VAR set: ObjSet; item: INTEGER) =
  BEGIN
    SUBARRAY(set.table^, item, NUMBER(set.table^)-item-1) :=
      SUBARRAY(set.table^, item+1, NUMBER(set.table^)-item-1);
    DEC(set.size);
  END remove_from_set;

PROCEDURE expand_table(VAR set: ObjSet) =
  VAR
    newtable := NEW(REF ARRAY OF ObjInstance, NUMBER(set.table^)*2);
  BEGIN
    SUBARRAY(newtable^, 0, NUMBER(set.table^)) := set.table^;
    set.table := newtable;
  END expand_table;

PROCEDURE suffix_in_table (set: ObjSet; suff: TEXT): INTEGER =
  VAR
    suflen  := Text.Length(suff);
    nametxt : TEXT;
  BEGIN
    FOR i := 0 TO set.size-1 DO
      nametxt := M3ID.ToText(set.table[i].name);
      IF suflen <= Text.Length(nametxt) AND
         Text.Equal(suff, Text.Sub(nametxt, Text.Length(nametxt)-suflen)) THEN
        RETURN i;
      END
    END;
    RETURN -1;
  END suffix_in_table;

PROCEDURE in_table (set: ObjSet; name: M3ID.T): INTEGER =
  BEGIN
    FOR i := 0 TO set.size-1 DO
      IF set.table[i].name = name THEN
        RETURN i;
      END
    END;
    RETURN -1;
  END in_table;

PROCEDURE unload (set: ObjSet; place: INTEGER) =
  BEGIN
    IF set.table[place].lib # NIL THEN
      loader.unload_lib(set.table[place].lib);
    ELSIF set.table[place].obj # NIL THEN
      loader.unload_obj(set.table[place].obj);
    ELSE
      error("Unload failed: place " & Fmt.Int(place));
    END
  END unload;

PROCEDURE load (set: ObjSet; place: INTEGER): BOOLEAN =
  VAR
    nametxt := M3ID.ToText(set.table[place].name);
  BEGIN
    TRY
      IF Text.Length(nametxt) > 4 AND
         Text.Equal(Text.Sub(nametxt, Text.Length(nametxt)-4), ".lib") THEN
        set.table[place].lib := loader.load_lib(nametxt);
      ELSE
        set.table[place].obj := loader.load_obj(nametxt);
      END
    EXCEPT
      M3Loader.LoadError =>
        RETURN FALSE;
    END;

    RETURN TRUE;
  END load;

TYPE
  ObjSet = RECORD
    console := TRUE;
    exename : TEXT := NIL;
    table   : REF ARRAY OF ObjInstance;
    size    := 0;
  END;

TYPE
  ObjInstance = RECORD
    name    : M3ID.T;
    stamp   : INTEGER;
    lib     : M3Loader.LibModule := NIL;
    obj     : M3Loader.ObjModule := NIL;
  END;

TYPE
  CmdFunc = RECORD
    name : TEXT;
    args : TEXT;
    desc : TEXT;
    func : PROCEDURE (args: TEXT);
  END;

CONST
  filelist = ".M3LINK";

VAR
  loader     := M3Loader.New();
  set        : ObjSet;
  cmd,
  cwd        : TEXT := "";
  conswind   : WinDef.HWND := NIL;
  start_time : Time.T;

CONST
  cmdtable = ARRAY OF CmdFunc {
    CmdFunc { "help", "", "List Commands", help },
    CmdFunc { "quit", "", "Quit Loader", quit },
    CmdFunc { "args", "<arg-list>", "Set command line", set_args },
    CmdFunc { "cd", "<path>", "Set Working Directory", set_cwd },
    CmdFunc { "debug", "", "Start windbg on the Loader", debug },
    CmdFunc { "show", "", "Show all symbols", show },
    CmdFunc { "unload", "<name>", "Unload a module or library", cmdunload },
    CmdFunc { "modules", "", "List loaded modules", modules },
    CmdFunc { "go", "", "Relink any new modules, then run", go } };

PROCEDURE to_lower (word: TEXT): TEXT =
  BEGIN
    FOR i := FIRST(word^) TO LAST(word^) DO
      IF word[i] >= 'A' AND word[i] <='Z' THEN
        word[i] := VAL(ORD(word[i]) + (ORD('a') - ORD('A')), CHAR);
      END
    END;
    RETURN word;
  END to_lower;

PROCEDURE interpret (line: TEXT): BOOLEAN =
  VAR
    head,
    tail  : TEXT;
    space : INTEGER;
    found : BOOLEAN;
  BEGIN
    space := Text.FindChar(line, ' ');
    IF space = -1 THEN
      head := to_lower(line);
      tail := "";
    ELSE
      head := to_lower(Text.Sub(line, 0, space));
      tail := Text.Sub(line, space+1);
    END;

    found := FALSE;
    FOR i := FIRST(cmdtable) TO LAST(cmdtable) DO
      IF Text.Equal(head, cmdtable[i].name) THEN
        cmdtable[i].func(tail);
        found := TRUE;
      END
    END;
    IF NOT found THEN
      IO.Put("Unknown command: '" & head & "'\n");
    END;

    RETURN Text.Equal(head, "quit");
  END interpret;

PROCEDURE cmdunload (args: TEXT) =
  VAR
    place := suffix_in_table(set, args);
  BEGIN
    IF place = -1 THEN
      error("Can't find " & args & " to unload");
    ELSE
      unload(set, place);
      remove_from_set(set, place);
    END
  END cmdunload;

PROCEDURE modules (<* UNUSED *> args: TEXT) =
  BEGIN
    FOR i := 0 TO set.size-1 DO
      IO.Put(M3ID.ToText(set.table[i].name));
      IO.Put("\n");
    END
  END modules;

CONST
  DescrColumn = 20;

PROCEDURE help (<* UNUSED *> args: TEXT) =
  BEGIN
    FOR i := FIRST(cmdtable) TO LAST(cmdtable) DO
      IO.Put(cmdtable[i].name & " " & cmdtable[i].args);
      FOR spaces := 1 TO DescrColumn-Text.Length(cmdtable[i].name)-
                                     Text.Length(cmdtable[i].args) DO
        IO.Put(" ");
      END;
      IO.Put(cmdtable[i].desc);
      IO.Put("\n");
    END
  END help;

PROCEDURE quit (<* UNUSED *> args: TEXT) =
  BEGIN
  END quit;

PROCEDURE set_args (args: TEXT) =
  BEGIN
    cmd := args;
  END set_args;

PROCEDURE set_cwd (args: TEXT) =
  BEGIN
    TRY
      IF FS.Status(args).type # FS.DirectoryFileType THEN
        error(args & " is not a directory, using \"" & cwd & "\"");
        RETURN;
      END
    EXCEPT
      OSError.E =>
        error("Can't find " & args & ", using \"" & cwd & "\"");
        RETURN;
    END;
    cwd := args;
    IO.Put("The process will start up with working directory:\n");
    IO.Put(cwd);
    IO.Put("\n");
  END set_cwd;

PROCEDURE show (<* UNUSED *> args: TEXT) =
  BEGIN
    loader.show_stuff();
  END show;

PROCEDURE go (<* UNUSED *> args: TEXT) =
  BEGIN
    conswind := WinUser.GetForegroundWindow();
    start_time := Time.Now();
    IF NOT read_list(filelist) THEN
      IO.Put("Error reading .M3LINK, not starting process\n");
      RETURN;
    END;
    IO.Put("Starting process\n");
    loader.call(cmd, cwd, set.exename, set.console, start_time);
    EVAL WinUser.SetForegroundWindow(conswind);
  END go;

PROCEDURE debug (<* UNUSED *> args: TEXT) =
  VAR
    params : ARRAY [0 .. 1] OF TEXT;
  BEGIN
    params[0] := "-p";
    params[1] := Fmt.Int(WinBase.GetCurrentProcessId());

    IO.Put("Attaching windbg to the loader process.\n");
    IO.Put("Choose 'Run/Go' from the windbg menu to restart the loader.\n");
    IO.Put("Quitting windbg will kill the loader.\n");
    TRY
      EVAL Process.Create("windbg", params);
    EXCEPT
      OSError.E =>
        error("Error starting windbg");
    END
  END debug;

BEGIN
  IO.Put("M3:TNG Loader Version 1.00\n");
  TRY
    REPEAT
      IO.Put("> ");
    UNTIL interpret(IO.GetLine());
  EXCEPT
    IO.Error =>
  END;

  loader.shutdown();
END Main.
