MODULE ScalarVar;
FROM CommandLoop IMPORT Error;
IMPORT CommandLoop;
IMPORT Term;
IMPORT TextList;
IMPORT FmtScanVar;

TYPE
  Command = CommandLoop.Command OBJECT
    fs: FmtScanVar.T;
    chg: BOOLEAN;
    varName: TEXT;
  OVERRIDES
    execute := Execute;
  END;
    

PROCEDURE PutCommand(cl: CommandLoop.T;
                     fs: FmtScanVar.T;
                     typeName, name, desc: TEXT;
                     userChange: BOOLEAN) =
  VAR
    cmd := NEW(Command, fs:=fs, chg:=userChange, varName:=name);
  BEGIN
    IF desc = NIL THEN
      desc := typeName & " variable `" & name & "'";
    END;
    IF userChange THEN
      cmd.simpleHelp := "[<val>] -- display/set " & desc;
    ELSE
      cmd.simpleHelp := "-- display " & desc;
    END;
    cl.putCommand(name, cmd);
  END PutCommand;

PROCEDURE Execute(self: Command; args: TextList.T; term: Term.T)
  RAISES {Error} =
  BEGIN
    IF args.tail = NIL THEN
      (* view variable *)
      term.wr(self.varName & " = " & self.fs.fmt(), TRUE);
    ELSIF NOT self.chg THEN
      RAISE Error("Too many arguments.");
    ELSE
      (* set variable *)
      TRY
        self.fs.scan(args.tail.head);
      EXCEPT FmtScanVar.Error(msg) =>
        RAISE Error(msg);
      END;
    END;
  END Execute;

BEGIN
END ScalarVar.
