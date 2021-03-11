(* $Id$ *)

MODULE ReadLineUI;
IMPORT ReadLine, TextReader, Text, Rd, OSError;
IMPORT ReadLineHelpNode AS HelpNode, ReadLineHelp AS Help;
IMPORT ReadLineHelpClass;
IMPORT ReadLineError;
IMPORT FileRd;
IMPORT NetObj;
IMPORT Thread;

CONST TE = Text.Equal;

REVEAL
  T = Public BRANDED Brand OBJECT
    intf : ReadLine.Public := NIL;
    name : TEXT;
    help : Help.T := NIL;
    next : T := NIL;
  OVERRIDES
    init := Init;
    run := Run;
    getIntf := GetIntf;
    command := DefCommand;
    chain := Chain;
  END;

PROCEDURE Chain(t : T; next : T) = 
  BEGIN 
    t.next := next;
    IF    t.help = NIL THEN
      t.help := next.help
    ELSIF next.help # NIL THEN
    t.help := NEW(Help.T).init(
                    HelpNode.ExtendNode(
                       t.help.getRoot(), 
                       next.help.getRoot().children))
    END
  END Chain;

PROCEDURE MyHelp() : HelpNode.List = 
  BEGIN
    
    RETURN HelpNode.NewList(HelpNode.Arr {
      HelpNode.New("quit", "quit the interface", "Quit the interface"),
      HelpNode.New("log <filename>", "log output", "Log all output, except asynchronous output and tables to file <filename>.  Will prompt for tables and discard asynchronous output."),
      HelpNode.New("source <filename>", "source commands", "Source commands from file."),
      HelpNode.New("help", "help", "This help system"),
      HelpNode.New("setenv <var> <value>", "set UI variable", "Set a user-interface variable to the given value"),
      HelpNode.New("nolog", "turn off logging", "Stop logging") })
  END MyHelp;

PROCEDURE Init(t : T; name : TEXT; helpRoot : HelpNode.T) : T = 
  BEGIN 
    IF helpRoot # NIL THEN
      t.help := NEW(Help.T).init(HelpNode.ExtendNode(helpRoot, MyHelp() ))
    END;

    t.name := name; RETURN t 
  END Init;

PROCEDURE Run(t : T; intf : ReadLine.Public) RAISES { ReadLineError.E, 
                                                      NetObj.Error,
                                                      Thread.Alerted } = 
  VAR
    oldPrompt : TEXT;
  BEGIN
    TRY
      <* ASSERT t.intf = NIL *> (* only one run at a time, plz *)

      t.intf := intf;
      oldPrompt := intf.getPrompt();

      intf.setPrompt(t.name & "> ");
      LOOP
        TRY
          WITH line   = intf.readLine(),
               reader = NEW(TextReader.T).init(line),
               save   = reader.save() DO
            IF Text.Length(line) > 0 AND Text.GetChar(line,0) # '#' THEN
              TRY 
                IF NOT t.command(reader) THEN
                  IF t.next = NIL THEN
                    intf.display("? unknown command\n")
                  ELSE
                    reader.continue(save);
                    (* this is really kind of dumb... *)
                    <* ASSERT t.next.intf = NIL *>
                    t.next.intf := intf;
                    TRY
                      EVAL t.next.command(reader)
                    FINALLY
                      t.next.intf := NIL
                    END
                  END
                END;
                
                (* check that all was consumed *)
                
                VAR rem : TEXT; BEGIN
                  IF reader.next("",rem) THEN
                    intf.display("? junk at end of command ignored: \""&rem&"\"\n")
                  END
                END
              EXCEPT
                Error(err) => intf.display("?" & err & "\n")
              END
            END
          END
        EXCEPT
          Quit, Rd.EndOfFile => RETURN
        END
      END
    FINALLY
      IF oldPrompt # NIL THEN intf.setPrompt(oldPrompt) END;
      t.intf := NIL
    END
  END Run;

PROCEDURE GetIntf(t : T) : ReadLine.Public =
  BEGIN RETURN t.intf END GetIntf;

PROCEDURE DefCommand(t : T; r : TextReader.T) : BOOLEAN 
  RAISES { Error, Quit, NetObj.Error, Thread.Alerted } =
  BEGIN
    TRY
      WITH cont = r.save(),
           cmd = r.get() DO
        IF    TE(cmd, "quit") THEN RAISE Quit
        ELSIF t.help # NIL AND TE(cmd, "help") THEN 
          t.help.topCommand(r, t.getIntf())
        ELSIF TE(cmd, "log") THEN
          t.getIntf().startLogging(r.get())
        ELSIF TE(cmd, "setenv") THEN
          WITH var = r.get(),
               val = r.get() DO
            t.getIntf().setVar(var,val)
          END
        ELSIF TE(cmd, "nolog") THEN
          t.getIntf().stopLogging()
        ELSIF TE(cmd, "source") THEN
          t.getIntf().source(FileRd.Open(r.get()))
        ELSE
          r.continue(cont); RETURN FALSE
        END;
        RETURN TRUE
      END
    EXCEPT
      TextReader.NoMore => RAISE Error("not enough arguments")
    |
      OSError.E => RAISE Error("couldn't open that log/source file")
    |
      ReadLineError.E => RAISE Quit
    END
  END DefCommand;
  
BEGIN END ReadLineUI.
