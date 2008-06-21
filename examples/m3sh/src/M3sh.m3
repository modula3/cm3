

(* M3sh is a simple shell utility which uses the safe, portable
   operating system interfaces. By using the portable interfaces,
   this program works on both Unix and Win32 platforms. *)

MODULE M3sh EXPORTS Main;
IMPORT Pathname, FS, IO, OSError;
IMPORT Stdio, RegularFile, Pipe, Process, Thread, Env, Params;
IMPORT FileWr, FileRd, Rd, Lex, Wr, Text, Atom, AtomList;
IMPORT TextRd, TextSeq;

(*------------------------------------------------- shell commands ------*)

(* "Command" designates a "name" and an "action" procedure.  *)

TYPE
  Command = RECORD 
    name: TEXT;
    action: PROCEDURE (cmd: TEXT; 
                     READONLY args: ARRAY OF TEXT): TEXT RAISES {OSError.E};
  END (* RECORD *);

(* "Commands" is an array of pre-defined "Command" designations
   for built-in shell commands, such as "cd". To allow aliasing of actions, 
   multiple names may correspond to the same action. *)

CONST
  Commands = ARRAY OF Command { Command {"exit", exit},
                                Command {"quit", exit},
                                Command {"bye",  exit},
                                Command {"cd", chdir},
                                Command {"chdir", chdir},
                                Command {"dir", dir},
                                Command {"ls", dir},
                                Command {"pwd", pwd},
                                Command {"directory", dir},
                                Command {"type", type}, 
                                Command {"cat",  type},
                                Command {"exec", exec},
                                Command {"bg",   background},
                                Command {"help", help}};

PROCEDURE ShellCommand(cmd: TEXT; READONLY args: ARRAY OF TEXT): TEXT RAISES {OSError.E} =
(* Executes the shell command "cmd" with arguments "args". *)
  BEGIN

    (* Check to see if "cmd" is a built-in. *)
    FOR i := FIRST(Commands) TO LAST(Commands) DO
      IF Text.Equal (cmd, Commands[i].name) THEN
        RETURN Commands[i].action (cmd, args);
      END;
    END;

    (* If cmd is not a built-in, then try to execute it. *)
    RETURN Execute (cmd, args);

  END ShellCommand;


PROCEDURE Execute(cmd: TEXT; READONLY args: ARRAY OF TEXT): TEXT RAISES {OSError.E} =
(* Run an external command, returning the result as a text string. 
  See the <<a href=/intf/Process>>Process<</a>> interface for more information. *)
  VAR hrChild, hwChild, hrSelf, hwSelf: Pipe.T;
  VAR result: TEXT := "";
  BEGIN
  
    ArgCount(args, 0);

    WITH full = FindExecutable(cmd) DO
      IF full # NIL THEN cmd := full; END;
    END;

    Pipe.Open(hr := hrChild, hw := hwSelf);
    Pipe.Open(hr := hrSelf, hw := hwChild);

    TRY 
      WITH p = Process.Create (cmd, args, stdin := hrChild, 
                            stdout := hwChild, stderr := NIL) DO
        TRY
          TRY hrChild.close(); hwChild.close()
          EXCEPT OSError.E => (* skip *)
          END;
 
         (* Here is the actual writing and reading, which is conveniently
            performed using I/O streams. *)

          WITH wr = NEW(FileWr.T).init(hwSelf),
               rd = NEW(FileRd.T).init(hrSelf) DO
         
          TRY Wr.Close(wr)
          EXCEPT Wr.Failure, Thread.Alerted => (*SKIP*)
          END;

          result := Rd.GetText(rd, LAST(INTEGER));
         
          TRY Rd.Close(rd)
          EXCEPT Rd.Failure, Thread.Alerted => (*SKIP*)
          END
        END;
     
      (* If not in "background", wait for the process to finish. *)
      FINALLY EVAL Process.Wait(p);
      END
    END
    EXCEPT
    | Rd.Failure, Thread.Alerted => Error ("exec failed");
    END; 
    RETURN result; 
  END Execute;

PROCEDURE ArgCount(READONLY args: ARRAY OF TEXT; 
                  lo: CARDINAL; 
                   hi: CARDINAL := LAST(INTEGER)) RAISES {OSError.E} =
(*  Check the number of arguments and raise "OSError.E"
    if the wrong number of arguments problem. *)
  BEGIN
    IF NUMBER(args) < lo THEN Error ("Too few args");
    ELSIF NUMBER(args) > hi THEN Error ("Too many args");
    END;
  END ArgCount;


PROCEDURE Error (name: TEXT) RAISES {OSError.E} =
(*  Raise "OSError.E", passing it two parameters 
    the returned error as a shell error parameter to the extension. *)
  VAR
    err := AtomList.List2(Atom.FromText(name), Atom.FromText("m3sh error"));
  BEGIN
    RAISE OSError.E (err);
  END Error;

(*-------------------------------------------------------- builtin commands ----*)

PROCEDURE pwd(<*UNUSED*>cmd: TEXT; READONLY args: ARRAY OF TEXT): TEXT RAISES {OSError.E} =
(* Working Directory *)
  BEGIN
   ArgCount(args, 0, 0);
   RETURN Process.GetWorkingDirectory();
  END pwd;

PROCEDURE dir(<*UNUSED*>cmd: TEXT; READONLY args: ARRAY OF TEXT): TEXT RAISES {OSError.E} =
(* Directory contents *)
  VAR
    dir: Pathname.T := "."; 
    result: TEXT := "";
    name: TEXT;
    iter: FS.Iterator;
  BEGIN
    ArgCount(args, lo := 0, hi := 1);
    IF NUMBER(args) > 0 THEN dir := args[0] END;
    IF NOT IsDirectory (dir) THEN Error (dir & " is not a directory"); END;
  
    IO.Put ("Directory listing for " & FS.GetAbsolutePathname(dir) & "\n");
    iter := FS.Iterate (dir);
    WHILE iter.next (name) DO
      result := result & "  " & name & "\n";
    END;
    RETURN result;
  END dir;

PROCEDURE chdir(<*UNUSED*>cmd: TEXT; READONLY args: ARRAY OF TEXT): TEXT RAISES {OSError.E} =
(* Change Directory *)
  BEGIN
    ArgCount(args, 1, 1);
    IF NOT IsDirectory (args[0]) THEN
      Error (args[0] & " is not a directory\n");
    END;
    Process.SetWorkingDirectory(args[0]);
    RETURN NIL;
  END chdir;

PROCEDURE type(cmd: TEXT; READONLY args: ARRAY OF TEXT): TEXT RAISES {OSError.E} =
(* Display the contents of a file or directory. 
   If "arg[0]" is a file, return its contents.
   If "arg[0]" is a directory, prints its directory listing. *)
  VAR
    rd: Rd.T;
  BEGIN
    ArgCount(args, 1, 1);
    IF IsDirectory (args[0]) THEN
      IO.Put (args[0] & " is a directory\n");
      RETURN dir (cmd, args);
    ELSE
        TRY
          rd := FileRd.Open(args[0]);
          TRY RETURN Rd.GetText(rd, LAST(INTEGER)); FINALLY Rd.Close(rd); END;
        EXCEPT
        | Rd.Failure, Thread.Alerted =>  Error ("type could not read a file\n");
        END;
    END;
    <* ASSERT FALSE *> 
  END type;

PROCEDURE exit(<*UNUSED*>cmd: TEXT; READONLY args: ARRAY OF TEXT): TEXT RAISES {OSError.E} =
(* Exit the shell *)
  BEGIN
    ArgCount(args, 0, 0);
    IO.Put ("Goodbye!\n");
    Process.Exit(0);
    <* ASSERT FALSE *>
  END exit;

PROCEDURE exec(<*UNUSED*>cmd: TEXT; READONLY args: ARRAY OF TEXT): TEXT RAISES {OSError.E} =
(* Bypass built-in commands. *)
  BEGIN
    ArgCount(args, 1);
    IO.Put ("The command is " & args[0] & "\n");
    RETURN Execute (args[0], SUBARRAY(args,1, NUMBER(args)-1));
  END exec;

PROCEDURE help(<*UNUSED*>cmd: TEXT; READONLY args: ARRAY OF TEXT): TEXT RAISES {OSError.E} =
(* Print a help message. *)
  BEGIN
    ArgCount (args, 0,0);
    RETURN HelpfulInfo();
  END help;

PROCEDURE background(<*UNUSED*>cmd: TEXT; READONLY args: ARRAY OF TEXT): TEXT RAISES {OSError.E} =
(* Run a command in the background. *)
  VAR
    background_closure: BgClosure;
  BEGIN
    ArgCount(args, 1);
    background_closure := NEW(BgClosure,
                              cmd := args[0],
                              args := NEW(REF ARRAY OF TEXT, NUMBER(args)-1));
    background_closure.args^ := SUBARRAY(args, 1, NUMBER(args)-1);
    EVAL Thread.Fork (background_closure); (* We forget the background results... *)
    RETURN NIL;
  END background;

(* Closure for the background thread. *)

TYPE
  BgClosure = Thread.Closure OBJECT
    cmd: TEXT; args: REF ARRAY OF TEXT;
  OVERRIDES
    apply := BackgroundApply;
  END;

PROCEDURE BackgroundApply (cl: BgClosure): REFANY = 
  BEGIN
    TRY
      RETURN Execute (cl.cmd, cl.args^);
    EXCEPT OSError.E => (* do nothing *)
    END;
    RETURN NIL;
  END BackgroundApply;

(*------------------------------------------ "PATH" navigation ---- *)


PROCEDURE FindExecutable (file: TEXT): TEXT =
(* Finds an executable program found by searching the directories
   contained in the "PATH" environment variable. "PATH" variable
   is looked up using the <<a href=/intf/Env>>Env<</a>> interface. *)

  VAR path := Env.Get ("PATH");

  CONST UnixExts = ARRAY OF TEXT { NIL };
  CONST WinExts = ARRAY OF TEXT { NIL, "exe", "com", "cmd", "bat" };
 
(* To look up the separator for "PATH", we need to find out 
   what sort of system we are running. To do so, we check
   to see if "Pathname" uses "/" or "\". *)
  VAR on_unix: BOOLEAN := Text.Equal(Pathname.Join("","",NIL),"/");
  BEGIN
    IF on_unix
      THEN RETURN SearchPath (file, path, ':', UnixExts);
      ELSE RETURN SearchPath (file, path, ';', WinExts);
    END;
  END FindExecutable;

PROCEDURE IsFile (file: TEXT): BOOLEAN =
(* Return "TRUE" if the name corresponds to a file. *)
  BEGIN
    TRY
      WITH stat = FS.Status (file) DO
        RETURN stat.type = RegularFile.FileType;
      END
    EXCEPT
    | OSError.E => RETURN FALSE;
    END
  END IsFile;

PROCEDURE IsDirectory (file: TEXT): BOOLEAN =
(* Returns "TRUE" if the name corresponds to a directory. *)
  BEGIN
    TRY
      WITH stat = FS.Status (file) DO
        RETURN stat.type = FS.DirectoryFileType;
      END
    EXCEPT
    | OSError.E => RETURN FALSE;
    END
  END IsDirectory;


PROCEDURE SearchPath (file, path: TEXT;   sep: CHAR;
                      READONLY exts: ARRAY OF  TEXT): TEXT =
(* Searches the items passed in as part of "path" for the 
   file "file". *)

  VAR dir, fn: TEXT;  s0, s1, len: INTEGER;  no_ext: BOOLEAN;
  BEGIN
    IF IsFile (file) THEN RETURN file; END;

    no_ext := Text.Equal (file, Pathname.Base (file));

    (* First try the file without looking at the path *)
    IF no_ext THEN
      FOR i := FIRST (exts) TO LAST (exts) DO
        fn := Pathname.Join (NIL, file, exts[i]);
        IF IsFile (fn) THEN RETURN fn; END;
      END;
    END;

    IF path = NIL THEN RETURN NIL; END;
    IF Pathname.Absolute (file) THEN RETURN NIL; END;

    (* Try the search path *)
    len := Text.Length (path);  s0 := 0;
    WHILE (s0 < len) DO
      s1 := Text.FindChar (path, sep, s0);
      IF (s1 < 0) THEN s1 := len; END;
      IF (s0 < s1) THEN
        dir := Text.Sub (path, s0, s1 - s0);
        IF no_ext THEN
          FOR i := FIRST (exts) TO LAST (exts) DO
            fn := Pathname.Join (dir, file, exts[i]);
            IF IsFile (fn) THEN RETURN fn; END;
          END;
        ELSE
          fn := Pathname.Join (dir, file, NIL);
          IF IsFile (fn) THEN RETURN fn; END;
        END;
      END;
      s0 := s1 + 1;
    END;

    (* failed *)
    RETURN NIL;
  END SearchPath;

(*-------------------------------------------- the main portion ---- *)

PROCEDURE HelpfulInfo(): TEXT =
  CONST
    Msg = "m3sh:     a simple portable shell for POSIX and Win32 written in Modula-3\n" &
          "syntax:   m3sh [-prompt string | -help]\n" &
          "commands:";
  VAR
    result := Msg;
  BEGIN
    FOR i := FIRST(Commands) TO LAST(Commands) DO
      result := result & " " & Commands[i].name;
    END;
    RETURN result & "\n";
  END HelpfulInfo;

VAR
  prompt: TEXT := "m3sh";

PROCEDURE ProcessCommand() RAISES {OSError.E, Rd.EndOfFile} =
(*        Processes a whole line. *)
  VAR
    cmdname: TEXT;                   (* name of the command *)
    cmdargs: REF ARRAY OF TEXT;      (* arguments of the command *)
    result: TEXT;                    (* result of the command *)

  BEGIN
    (* Echo the prompt. Get a command. 
       If the command is not null, then 
       execute it, and print its results on the screen. *)
    IO.Put(prompt & "> ");
    GetCommand(cmdname, cmdargs);
    IF cmdname = NIL THEN RETURN END;
    result := ShellCommand (cmdname, cmdargs^);
    IF result # NIL THEN IO.Put (result & "\n") END;
  END ProcessCommand;

PROCEDURE GetCommand(VAR name: TEXT; VAR args: REF ARRAY OF TEXT) 
          RAISES {Rd.EndOfFile} =
(* Read a command line; affect variables "name" and "args".
   Set "name" and "args" to NIL if there is no input in this line.
   Raise "Rd.EndOfFile" if the end of file is reached. *)

  VAR 
    cmd := NEW(TextSeq.T).init();
    rd: Rd.T;

  BEGIN
    name := NIL; args := NIL;

    TRY
      (* Read a line and map it to the reader "rd". *)
      rd := TextRd.New(Rd.GetLine(Stdio.stdin));

      (* Tokenize the line into a sequence of strings. *)
      TRY WHILE NOT Rd.EOF(rd) DO
            Lex.Skip(rd);
            cmd.addhi(Lex.Scan(rd)); END;
      EXCEPT Rd.Failure => (* do nothing *) 
      END;

      (* Turn the sequence into a (command, arguments)
         pair. *)
      IF cmd.size() = 0 THEN RETURN END;
      name := cmd.get(0);
      args := NEW(REF ARRAY OF TEXT, cmd.size()-1);

      FOR i := FIRST(args^) TO LAST(args^) DO
        args[i] := cmd.get(i+1);
      END; 
    EXCEPT
    | Rd.Failure, Thread.Alerted => IO.Put ("Problems in reading from input\n"); 
    END;
  END GetCommand;

PROCEDURE PrintError (al: AtomList.T) =
(* Print arguments to an "OSError.E". Used by the main
   shell loop to print out errors. *)
  BEGIN
    al := AtomList.Reverse(al);
    WHILE al # NIL DO
      IO.Put (Atom.ToText(al.head) & ". ");
      al := al.tail;
    END;
    IO.Put ("\n");
  END PrintError;

PROCEDURE ProcessParams() =
(* Checks the command-line parameters. *)
  BEGIN
    CASE Params.Count OF
    | 1 => RETURN;
    | 2 => IF Text.Equal(Params.Get(1), "-help")   THEN IO.Put (HelpfulInfo());   RETURN END;
    | 3 => IF Text.Equal(Params.Get(1), "-prompt") THEN prompt := Params.Get(2); RETURN END;
    ELSE (* skip *)
    END;
    IO.Put ("Incorrect or bad number of parameters. Try -help to get more info.\n");
    Process.Exit(10);
  END ProcessParams;

(* The main loop. *)

BEGIN
  ProcessParams();
  LOOP
    TRY 
      ProcessCommand(); 
    EXCEPT
    | OSError.E (e) =>    PrintError(e);
    | Rd.EndOfFile =>     EXIT;
    END;
  END;
END M3sh.

