(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Feb 22 08:50:13 PST 1995 by kalsow     *)

UNSAFE MODULE Utils;

IMPORT File, FileWr, Wr, Thread, Fmt, Process, TextIntTbl, M3toC;
IMPORT Stdio, OSError, ETimer, FS, RegularFile, Time, Text;
IMPORT Msg, Arg, M3Timers, CoffTime, M3File, Pathname;
IMPORT WCharr, Mx, MxConfig; 
FROM Ctypes IMPORT const_char_star, char_star, int;

(*--------------------------------------------------------------- writers ---*)

PROCEDURE OpenWriter (name: TEXT;  fatal: BOOLEAN): Wr.T =
  VAR wr: Wr.T;
  BEGIN
    TRY
      wr := FileWr.Open (name);
    EXCEPT OSError.E (args) =>
      IF (fatal)
        THEN Msg.FatalError (args, "unable to open file for writing: ", name);
        ELSE Msg.Error      (args, "unable to open file for writing: ", name);
      END;
      wr := NIL;
    END;
    RETURN wr;
  END OpenWriter;

PROCEDURE AppendWriter (name: TEXT;  fatal: BOOLEAN): Wr.T =
  VAR wr: Wr.T;
  BEGIN
    TRY
      wr := FileWr.OpenAppend (name);
    EXCEPT OSError.E (args) =>
      IF (fatal)
        THEN Msg.FatalError (args, "unable to open file for writing: ", name);
        ELSE Msg.Error      (args, "unable to open file for writing: ", name);
      END;
      wr := NIL;
    END;
    RETURN wr;
  END AppendWriter;

PROCEDURE FlushWriter (wr: Wr.T;  name: TEXT) =
  BEGIN
    IF (wr = NIL) THEN RETURN END;
    TRY
      Wr.Flush (wr);
    EXCEPT
    | Wr.Failure (args) =>
        Msg.FatalError (args, "unable to flush output file: ", name);
    | Thread.Alerted =>
        Msg.FatalError (NIL, "unable to flush output file: ", name);
    END;
  END FlushWriter;

PROCEDURE CloseWriter (wr: Wr.T;  name: TEXT) =
  BEGIN
    IF (wr = NIL) THEN RETURN END;
    TRY
      Wr.Close (wr);
    EXCEPT
    | Wr.Failure (args) =>
        Msg.FatalError (args, "unable to close output file: ", name);
    | Thread.Alerted =>
        Msg.FatalError (NIL, "unable to close output file: ", name);
    END;
  END CloseWriter;

PROCEDURE WriteFile (file: TEXT;  proc: Emitter;  append := FALSE) =
  VAR wr: Wr.T;
  BEGIN
    IF (append)
      THEN wr := AppendWriter (file, fatal := TRUE);
      ELSE wr := OpenWriter (file, fatal := TRUE);
    END;
    TRY
      TRY
        proc (wr);
      EXCEPT
      | Wr.Failure (ec) =>
          Msg.FatalError (ec, "write failed on ", file);
      | Thread.Alerted =>
          Msg.FatalError (NIL, "interrupted while writing ", file);
      END;
    FINALLY
      CloseWriter (wr, file);
    END;
  END WriteFile;

(*--------------------------------------------------------------- readers ---*)

PROCEDURE OpenReader (name: TEXT;  fatal: BOOLEAN): File.T =
  VAR rd: File.T;
  BEGIN
    TRY
      rd := FS.OpenFileReadonly (name);
    EXCEPT OSError.E (args) =>
      IF (fatal)
        THEN Msg.FatalError (args, "unable to open file for reading: ", name);
        ELSE Msg.Error      (args, "unable to open file for reading: ", name);
      END;
      rd := NIL;
    END;
    RETURN rd;
  END OpenReader;

PROCEDURE CloseReader (rd: File.T;  name: TEXT) =
  BEGIN
    IF (rd = NIL) THEN RETURN END;
    TRY
      rd.close ();
    EXCEPT OSError.E (args) =>
      Msg.FatalError (args, "unable to close input file: ", name);
    END;
  END CloseReader;

PROCEDURE RewindReader (rd: File.T;  name: TEXT) =
  VAR f: RegularFile.T := rd;
  BEGIN
    IF (rd = NIL) THEN RETURN END;
    TRY
      EVAL f.seek (RegularFile.Origin.Beginning, 0);
    EXCEPT OSError.E (args) =>
      Msg.FatalError (args, "unable to rewind input file: ", name);
    END;
  END RewindReader;

(*------------------------------------------------------- temporary files ---*)

VAR
  tmp_files := NEW (TextIntTbl.Default).init (100);

PROCEDURE OpenTempFile (root: TEXT;  VAR file: TEXT): Wr.T =
  VAR seq := 0;  wr: Wr.T;
  BEGIN
    file := root;
    WHILE (ModificationTime (file) # NO_TIME) DO
      INC (seq);
      file := root & "_" & Fmt.Int (seq);
    END;
    wr := OpenWriter (file, fatal := TRUE);
    EVAL tmp_files.put (file, 0);
    RETURN wr;
  END OpenTempFile;

PROCEDURE NoteTempFile (name: TEXT) =
  BEGIN
    EVAL tmp_files.put (name, 0);
  END NoteTempFile;

PROCEDURE RemoveTempFiles () =
  VAR
    iter := tmp_files.iterate ();
    name : TEXT;
    void : INTEGER;
  BEGIN
    WHILE iter.next (name, void) DO  Remove (name);  END;
  END RemoveTempFiles;

(*------------------------------------------------------- file operations ---*)

PROCEDURE Remove (file: TEXT) =
  VAR void: INTEGER;
  BEGIN
    IF (file = NIL) THEN RETURN END;
    ETimer.Push (M3Timers.remove);
    Msg.Commands ("rm ", file);
    TRY
      FS.DeleteFile (file);
    EXCEPT OSError.E =>
      (* ignore the failure *)
    END;
    EVAL tmp_files.delete (file, void);
    ETimer.Pop ();
  END Remove;

PROCEDURE Copy (old, new: TEXT) =
  BEGIN
    Msg.Commands ("copy ", old, " -> ", new);
    TRY
      M3File.Copy (old, new);
    EXCEPT OSError.E (ec) =>
      Msg.FatalError (ec, "unable to copy: ", old, " -> ", new);
    END;
  END Copy;

PROCEDURE IsEqual (a, b: TEXT): BOOLEAN =
  BEGIN
    Msg.Commands ("compare ", a, " ", b);
    TRY
      RETURN M3File.IsEqual (a, b);
    EXCEPT OSError.E (ec) =>
      Msg.FatalError (ec, "unable to compare: ", a, " == ", b);
    END;
    RETURN FALSE;
  END IsEqual;

PROCEDURE IsDir(fn : TEXT) : BOOLEAN =
  VAR s : File.Status;
  BEGIN
    TRY
      s := FS.Status(fn);
    EXCEPT ELSE
      RETURN FALSE;
    END;
    RETURN s.type = FS.DirectoryFileType;
  END IsDir;

PROCEDURE IsFile(fn : TEXT) : BOOLEAN =
  VAR s : File.Status;
  BEGIN
    TRY
      s := FS.Status(fn);
    EXCEPT ELSE
      RETURN FALSE;
    END;
    RETURN s.type = RegularFile.FileType;
  END IsFile;

(*------------------------------------------------------------ file times ---*)

VAR
  file_times := NEW (TextIntTbl.Default).init ();
  max_file_time := NO_TIME;

PROCEDURE NoteLocalFileTimes () =
  VAR
    iter : FS.Iterator;
    file : TEXT;
    stat : File.Status;
    time : INTEGER;
    done := FALSE;
  BEGIN
    TRY iter := FS.Iterate (".");
    EXCEPT OSError.E => (*** silently assume the files are missing... ***)
      RETURN;
    END;

    REPEAT
      TRY
        WHILE iter.nextWithStatus (file, stat) DO
          time := M3Time (stat.modificationTime);
          max_file_time := MAX (max_file_time, time);
          EVAL file_times.put (file, time);
        END;
        done := TRUE;
      EXCEPT OSError.E =>
      END;
    UNTIL done;

    iter.close ();
  END NoteLocalFileTimes;

PROCEDURE LocalModTime (file: TEXT): INTEGER =
  VAR t := NO_TIME;
  BEGIN
    IF (file # NIL) THEN EVAL file_times.get (file, t) END;
    RETURN t;
  END LocalModTime;

PROCEDURE ModificationTime (file: TEXT): INTEGER =
  VAR t: INTEGER;
  BEGIN
    IF (file = NIL) OR NOT file_times.get (file, t) THEN
      t := NoteModification (file);
    END;
    RETURN t;
  END ModificationTime;

PROCEDURE NoteModification (file: TEXT): INTEGER =
  VAR s: File.Status;  t: INTEGER;
  BEGIN
    IF (file = NIL) THEN RETURN NO_TIME; END;
    TRY
      s := FS.Status (file);
      t := M3Time (s.modificationTime);
    EXCEPT OSError.E =>
      t := NO_TIME;
    END;
    max_file_time := MAX (max_file_time, t);
    EVAL file_times.put (file, t);
    RETURN t;
  END NoteModification;

PROCEDURE NoteNewFile (file: TEXT) =
  (* This hack with "max_file_time" is necessary for systems with
     remote filesystems and unsynchronized clocks.  We need to believe
     that any new file is at least as recent as the local time
     and the most recent observed file time. *)
  VAR t := M3Time (Time.Now ());
  BEGIN
    max_file_time := MAX (max_file_time, t);
    EVAL file_times.put (file, max_file_time);
  END NoteNewFile;

PROCEDURE M3Time (t: Time.T): INTEGER =
  CONST Year  = 365.25d0 * 24.0d0 * 3600.0d0;
  CONST Epoch = CoffTime.EpochAdjust(*1970*) + 24.0d0 * Year;
  BEGIN
    RETURN ROUND (t - Epoch);
  END M3Time;

(*-------------------------- initializing range of WIDECHAR -----------------*)

VAR GWidechar16Seen, GWidecharUniSeen: BOOLEAN := FALSE; 

PROCEDURE NoteWidechar16 () =
  BEGIN 
    GWidecharUniSeen := FALSE; 
    GWidechar16Seen := TRUE; 
  END NoteWidechar16; 

PROCEDURE NoteWidecharUni () =
  BEGIN 
    GWidechar16Seen := FALSE; 
    GWidecharUniSeen := TRUE; 
  END NoteWidecharUni; 

PROCEDURE InitWidechar () =

  (* Because of the division of cm3 into packages, which cannot be
     cyclically dependent, we have to do this a bit kludgily.  Here,
     in package cm3, we initially have the necessary information.  It is
     needed in m3front and m3linker, which can't refer to cm3 in any way.
     So we push it down into global variables declared in m3front and m3linker, 
     where they can access it.
  *) 

  VAR Unicode_WIDECHAR_opt: TEXT;
  BEGIN
    IF GWidechar16Seen THEN (* Command line -widechar16. *) 
      WCharr.IsUnicode := FALSE; 
      Mx.UnicodeWideChar := FALSE; 
    ELSIF GWidecharUniSeen THEN (* Command line -widecharuni. *) 
      WCharr.IsUnicode := TRUE;
      Mx.UnicodeWideChar := TRUE; 
    ELSE 
      Unicode_WIDECHAR_opt := MxConfig.Get("Unicode_WIDECHAR");
      (* ^Possibly set in cm3.cfg, or just about any other Quake code. *)  
      IF Unicode_WIDECHAR_opt # NIL  
         AND NOT Text.Equal(Unicode_WIDECHAR_opt, "")
      THEN 
        WCharr.IsUnicode := TRUE;
        Mx.UnicodeWideChar := TRUE; 
      ELSE (* Default case: 16-bit WIDECHAR. *) 
        WCharr.IsUnicode := FALSE; 
        Mx.UnicodeWideChar := FALSE; 
      END; 
    END; 
  END InitWidechar; 

(*---------------------------------- process creation / command execution ---*)

PROCEDURE PrepArgs (program: TEXT;  args: Arg.List): REF ARRAY OF TEXT =
  VAR argv := NEW (REF ARRAY OF TEXT, args.cnt);  a := args.head;
  BEGIN
    IF (Msg.level >= Msg.Level.Commands) THEN Msg.Out (program); END;

    FOR i := 0 TO args.cnt-1 DO 
      argv[i] := a.arg; 
      IF (Msg.level >= Msg.Level.Commands) THEN Msg.Out (" ", argv [i]); END;
      a := a.next;
    END;

    IF (Msg.level >= Msg.Level.Commands) THEN Msg.Out (Wr.EOL); END;

    RETURN argv;
  END PrepArgs;

PROCEDURE Execute (program: TEXT;  args: Arg.List;
                   stdout: TEXT;  fatal: BOOLEAN): INTEGER =
  VAR
    p         : Process.T;
    my_stdin  : File.T;
    my_stdout : File.T;
    my_stderr : File.T;
    ec        : Process.ExitCode;
  BEGIN
    FlushWriter (Stdio.stdout, "<stdout>");
    FlushWriter (Stdio.stderr, "<stderr>");

    TRY
      Process.GetStandardFileHandles (my_stdin, my_stdout, my_stderr);
      IF (stdout # NIL) THEN my_stdout := OpenStdout (stdout); END;
      p := Process.Create (cmd := program, 
                           params := PrepArgs (program, args)^,
                           stdin := my_stdin, stdout := my_stdout,
                           stderr := my_stderr);
      IF (stdout # NIL) THEN CloseStdout (my_stdout, stdout); END;
      ec := Process.Wait (p);
    EXCEPT OSError.E (args) =>
      Msg.FatalError (args, "Process.Create(\""& program &"\") failed");
    END;

    IF (fatal) AND (ec # 0) THEN
      Msg.FatalError (NIL, "program \"", program, "\" failed, exit status = ",
                        Fmt.Int (ec));
    END;
    RETURN ec;
  END Execute;

PROCEDURE OpenStdout (nm: TEXT): File.T =
  VAR wr: File.T;
  BEGIN
    TRY
      wr := FS.OpenFile (nm);
    EXCEPT OSError.E (args) =>
      Msg.FatalError (args, "unable to open file for listing: ", nm);
      wr := NIL;
    END;
    RETURN wr;
  END OpenStdout;

PROCEDURE CloseStdout (wr: File.T;  name: TEXT) =
  BEGIN
    IF (wr = NIL) THEN RETURN END;
    TRY
      wr.close ();
    EXCEPT OSError.E (args) =>
      Msg.FatalError (args, "unable to close listing file: ", name);
    END;
  END CloseStdout;

(*---------------------------------------------------------------------------*)

PROCEDURE MakeRelative (VAR from: TEXT;  to: TEXT) =
  VAR
    from_arcs, to_arcs: Pathname.Arcs;
    from_len, to_len  : INTEGER;
    from_x, to_x      : TEXT;
    common            : INTEGER;
    new_arcs          : Pathname.Arcs;
  BEGIN
    IF NOT Pathname.Absolute (from) OR NOT Pathname.Absolute (to) THEN
      RETURN;  (* bail out *)
    END;

    TRY
      from_arcs := Pathname.Decompose (from);
      to_arcs   := Pathname.Decompose (to);
    EXCEPT Pathname.Invalid =>
      RETURN;  (* bail out *)
    END;

    from_len := from_arcs.size ();
    to_len := to_arcs.size ();
    common := 0;
    WHILE (common < from_len-1) AND (common < to_len-1) DO
      from_x := from_arcs.get (common);
      to_x := to_arcs.get (common);
      IF    (from_x = NIL) AND (to_x = NIL) THEN  (* they're common *)
      ELSIF (from_x = NIL) OR  (to_x = NIL) THEN  EXIT;  (* they're different *)
      ELSIF NOT Text.Equal (from_x, to_x)   THEN  EXIT;  (* they're different *)
      END;
      INC (common);
    END;
    IF (common <= 0) THEN RETURN END;

    new_arcs := NEW (Pathname.Arcs).init (from_len);
    new_arcs.addhi (NIL); (* make a relative path *)
    FOR i := common TO to_len-2   DO new_arcs.addhi (Pathname.Parent);   END;
    FOR i := common TO from_len-1 DO new_arcs.addhi (from_arcs.get (i)); END;

    TRY
      from_x := Pathname.Compose (new_arcs);
    EXCEPT Pathname.Invalid =>
      RETURN; (* bail out *)
    END;

    Msg.Verbose ("\nREWRITE: ",  from,    " -> ",  to,
                 "\n         " & from_x & " -> " & to);
    from := from_x;
  END MakeRelative;

(*---------------------------------------------------------------------------*)

PROCEDURE SymbolicOrHardLink (link: PROCEDURE(name1, name2: const_char_star):int; s_for_sym, from, to: TEXT) =
  VAR s_from, s_to: char_star;
  BEGIN
    Remove (to);
    Msg.Commands ("ln ", s_for_sym, from, " ", to);
    s_from := M3toC.SharedTtoS (from);
    s_to   := M3toC.SharedTtoS (to);
    EVAL link(s_from, s_to);
    M3toC.FreeSharedS (from, s_from);
    M3toC.FreeSharedS (to, s_to);
  END SymbolicOrHardLink;

(*---------------------------------------------------------------------------*)

BEGIN
END Utils.
