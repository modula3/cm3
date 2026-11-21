MODULE CommandLoop;
IMPORT TextCommandQueueTbl;
IMPORT CitTextUtils AS TextUtils;
IMPORT TextTextListTbl;
IMPORT Text;
IMPORT FileRd;
IMPORT Term;
IMPORT Thread;
IMPORT FileWr;
IMPORT TextReader;
IMPORT Pathname;
IMPORT OSError;
IMPORT Cooker;
IMPORT RdList;
IMPORT Rd;
IMPORT TextList;
IMPORT Wr;

REVEAL
  T = Public BRANDED OBJECT
    prompt: TEXT;
    pre, post: Command;
    commands: TextCommandQueueTbl.T;
    prefixes: TextTextListTbl.T; (* prefix -> possible command names *)
    prev: TextList.T;            (* previously executed commands     *)
    term: Term.T;                (* terminal used in "run"           *)
    inputStack: RdList.T;        (* sourcefile readers               *)
    load: BuiltInCommand;
    extHelpCatalog: TextList.T;
  OVERRIDES
    init                := Init;
    putCommand          := PutCommand;
    run                 := Run;
    setPreStep          := SetPreStep;
    setPostStep         := SetPostStep;
  END;

TYPE
  BuiltInCommand = Command OBJECT loop: T; END;
  QuitCommand = Command BRANDED OBJECT END;


PROCEDURE Init(self: T;
               prompt := "> ";
               help := "help ?";
               quit := "quit ^D";
               input := "input source";
               save := "save") : T =
  BEGIN
    self.prompt         := prompt;
    self.extHelpCatalog := NIL;
    self.inputStack     := NIL;
    self.prev           := NIL;
    self.pre            := NIL;
    self.post           := NIL;
    self.commands       := NEW(TextCommandQueueTbl.Default).init();
    self.prefixes       := NEW(TextTextListTbl.Default).init();

    self.putCommand(help, NEW(BuiltInCommand,
                              loop := self,
                              simpleHelp := "-- display list of commands",
                              execute := DoHelp));

    self.putCommand(save,NEW(BuiltInCommand,
               loop := self,
               simpleHelp:="<filename> -- save previously-executed commands",
               execute := DoSave));

    self.load := NEW(BuiltInCommand,
                     loop := self,
                     simpleHelp:= "<filename> -- execute commands from a file",
                     execute := DoLoad);
    self.putCommand(input, self.load);

    self.putCommand(quit, NEW(QuitCommand,
                              simpleHelp := "-- exit the command loop"));

    RETURN self;
  END Init;

PROCEDURE PreRegister(prefixes: TextTextListTbl.T; prefix, name: TEXT) =
  VAR
    l: TextList.T := NIL;
  BEGIN
    EVAL prefixes.get(prefix, l);
    EVAL prefixes.put(prefix, TextList.Cons(name,l));
    IF Text.Length(prefix)#0 THEN
      PreRegister(prefixes, Text.Sub(prefix,0,Text.Length(prefix)-1), name);
    END;
  END PreRegister;

PROCEDURE PutCommand(self: T; names: TEXT; cmd: Command) =
  VAR
    cur := TextUtils.Shatter(names);
  BEGIN
    cmd.pre := self.pre;
    cmd.post := self.post;
    IF cmd.hasExtendedHelp THEN
      self.extHelpCatalog := TextList.Append(cur, self.extHelpCatalog);
    END;
    WHILE cur # NIL DO
      EVAL self.commands.put(cur.head, cmd);
      PreRegister(self.prefixes, cur.head, cur.head);
      cur := cur.tail;
    END;
  END PutCommand;


(*****************************************************************************
 *                                                                           *
 *               "Execute" methods for Built-in Commands                     *
 *                                                                           *
 *****************************************************************************)

PROCEDURE DoHelp(helpCmd: BuiltInCommand; args: TextList.T; term: Term.T)
  RAISES {Error} =
  VAR
    self := helpCmd.loop;
    name: TEXT;
    cmd: Command;
  BEGIN
    IF args.tail = NIL THEN
      WITH iter = self.commands.iterateQOrdered() DO
        WHILE iter.next(name, cmd) DO
          term.wr(name); term.wr(" ");
          IF cmd.simpleHelp = NIL THEN
            term.wr("-- no help provided.\n");
          END;
          term.wr(cmd.simpleHelp, TRUE);
        END;
      END;
      IF self.extHelpCatalog # NIL THEN
        term.wr("help <command> -- extended help is available for the following commands:", TRUE);
        term.wr(TextUtils.Assemble(self.extHelpCatalog), TRUE);
      END;
    ELSE
      name := args.tail.head;
      CASE Lookup(self, name, cmd) OF
      | LURes.WasPrefix, LURes.Found =>
        IF cmd.hasExtendedHelp THEN
          term.wr(cmd.extendedHelp(TextList.Cons(name, args.tail.tail)), TRUE);
        ELSE
          term.wr(name & " " & cmd.simpleHelp, TRUE);
          term.wr("(no extended help provided)", TRUE);
        END;
      | LURes.NotFound => RAISE Error("command not found");
      | LURes.Ambiguous => RAISE Error("command ambiguous");
      END;
    END;
  END DoHelp;

PROCEDURE DoSave(cmd: BuiltInCommand; args: TextList.T; <*UNUSED*>term: Term.T)
  RAISES {Error} =
  BEGIN
    IF TextList.Length(args) # 2 THEN
      RAISE Error("single argument expected.")
    ELSE
      TRY
        VAR
          fn := args.tail.head;
          wr := FileWr.Open(fn);
          cur := TextList.Reverse(cmd.loop.prev);
        BEGIN
          WHILE cur # NIL DO
            Wr.PutText(wr, cur.head);
            Wr.PutChar(wr, '\n');
            cur := cur.tail;
          END;
          Wr.Close(wr);
        END;
      EXCEPT OSError.E, Wr.Failure, Thread.Alerted =>
        RAISE Error("bad output file");
      END;
    END;
  END DoSave;

PROCEDURE DoLoad(cmd: BuiltInCommand; args: TextList.T; <*UNUSED*>term: Term.T)
  RAISES {Error} =
  BEGIN
    IF args.tail = NIL THEN RAISE Error("filename missing."); END;
    TRY
      cmd.loop.inputStack := RdList.Cons(FileRd.Open(args.tail.head),
                                         cmd.loop.inputStack);
    EXCEPT OSError.E =>
      RAISE Error("cannot open `"&args.tail.head&"'.");
    END;
  END DoLoad;


(*****************************************************************************
 *                                                                           *
 *                        Command-Table Utilities                            *
 *                                                                           *
 *****************************************************************************)


(* Lookup a command *)

TYPE
  LURes = {Found, WasPrefix, NotFound, Ambiguous};

PROCEDURE Lookup(self: T; VAR name: TEXT; VAR cmd: Command): LURes =
  VAR
    l: TextList.T;
  BEGIN
    IF self.commands.get(name, cmd) THEN
      RETURN LURes.Found;
    ELSIF self.prefixes.get(name, l) THEN
      name := l.head;
      IF NOT self.commands.get(name, cmd) THEN
        <* ASSERT FALSE *>
        (* how did the command end up in the prefix table? *)
      END;
      IF l.tail = NIL THEN
        RETURN LURes.WasPrefix;
      ELSE
        RETURN LURes.Ambiguous;
      END;
    ELSE
      RETURN LURes.NotFound;
    END;
  END Lookup;


(* Complete a command *)

TYPE
  StdCompleter = Cooker.Completer OBJECT
    loop: T;
  OVERRIDES
    do := Complete;
  END;

PROCEDURE Complete(comp: StdCompleter; VAR t: TEXT) =
  VAR
    p:=Text.FindChar(t, ' ');
    name := t;
    q: Command;
    term := comp.loop.term;
  BEGIN
    IF p#-1 THEN
      name := Text.Sub(t,0,p);
    END;
    CASE Lookup(comp.loop, name, q) OF
    | LURes.WasPrefix, LURes.Found =>
      IF p=-1 THEN
        t := name & " ";
      ELSE
        TRY
          q.complete(t);
        EXCEPT Error(e) =>
          term.wr("\n" & e & "\n");
        END;
      END;
    | LURes.NotFound => term.wr("\ncommand not found\n");
    | LURes.Ambiguous => term.wr("\ncommand ambiguous\n");
    END;
  END Complete;

PROCEDURE SetPreStep(self: T; cmd: Command := NIL) =
  BEGIN
    self.pre := cmd;
  END SetPreStep;

PROCEDURE SetPostStep(self: T; cmd: Command := NIL) =
  BEGIN
    self.post := cmd;
  END SetPostStep;



(*****************************************************************************
 *                                                                           *
 *                            Command Loop Main                              *
 *                                                                           *
 *****************************************************************************)


PROCEDURE Run(self: T; source: Pathname.T := NIL; term: Term.T := NIL) =
  CONST
    Comment = SET OF CHAR{'%','#'};
  VAR
    completer := NEW(StdCompleter, loop:=self);
    line: TEXT;
  BEGIN
    IF term = NIL THEN
      self.term := Term.Default();
    ELSE
      self.term := term;
    END;
    LOOP
      TRY
        IF source # NIL THEN
          DoLoad(self.load, TextList.List2("",source), self.term);
          source := NIL;
        ELSE
          IF self.inputStack = NIL THEN
            line := Cooker.Input(self.prompt, completer, self.prev,
                                 "", FALSE, TRUE);
          ELSE
            TRY
              line := Rd.GetLine(self.inputStack.head);
              IF Text.Equal(line, "") OR Text.GetChar(line,0) IN Comment THEN
                line := "";
              ELSE
                self.term.wr(self.prompt & line, TRUE, TRUE);
              END;
            EXCEPT
            | Rd.EndOfFile =>
              self.inputStack := self.inputStack.tail;
              line := "";
            | Rd.Failure, Thread.Alerted =>
              RAISE Error("bad input file");
            END;
          END;
          
          IF Text.Equal(line,"\003") THEN
            self.term.wr("quit", TRUE, TRUE);
          ELSIF Text.Equal(line,"\032") THEN
            self.term.wr("\nSuspended\njust kidding - I don't know how to do that.\n");
          ELSIF Text.Equal(line,"") THEN
          ELSE
            IF Text.Equal(line,"\004") THEN
              line := "^D";
              self.term.wr("", TRUE, TRUE);
            END;
            VAR
              tr := NEW(TextReader.T).init(line);
              portion: TEXT;
              args: TextList.T;
              cmd: Command;
            BEGIN
              WHILE tr.next(";",portion,TRUE) DO
                args := NEW(TextReader.T).init(portion).shatter(": ","",TRUE);
                IF args # NIL THEN
                  CASE Lookup(self, args.head, cmd) OF
                  | LURes.WasPrefix,LURes.Found =>
                    TYPECASE cmd OF QuitCommand =>
                      RETURN;
                    ELSE
                      Term.MakeRaw(FALSE); (* hack: ^C support *)
                      IF cmd.pre # NIL THEN
                        cmd.pre.execute(args, self.term);
                      END;
                      cmd.execute(args, self.term);
                      IF cmd.post # NIL THEN
                        cmd.post.execute(args, self.term);
                      END;
                      Term.MakeRaw(TRUE);
                    END;
                  | LURes.NotFound =>
                    RAISE Error("command not found; try `help'.");
                  | LURes.Ambiguous =>
                    RAISE Error("command ambiguous; try `help'.");
                  END;
                END;
              END;
            END;
            self.prev := TextList.Cons(line, self.prev);
          END;
        END;
      EXCEPT Error(e) =>
        IF self.inputStack # NIL THEN
          self.inputStack := self.inputStack.tail;
        END;
        self.term.wr("error: " & e, TRUE, TRUE);
      END;
    END;
  END Run;




BEGIN
END CommandLoop.
