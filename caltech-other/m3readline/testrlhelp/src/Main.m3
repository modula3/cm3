(* $Id$ *)

MODULE Main;
IMPORT ReadLine;
IMPORT Fmt;
IMPORT Thread, Rd, Debug, Wx, Text, FileRd, TextReader;
IMPORT Date, XTime AS Time;
IMPORT ReadLineHelpNode AS HelpNode;
IMPORT ReadLineHelp AS Help;
IMPORT ReadLineUI AS UserInterface;

CONST TE = Text.Equal;

TYPE 
  Closure = Thread.Closure OBJECT 
    rl : ReadLine.T; 
    quit := FALSE;
  OVERRIDES 
    apply := A 
  END;

VAR ct : Thread.T := NIL;
VAR quitClock := FALSE;

PROCEDURE StartClock(rl : ReadLine.T) =
  BEGIN
    IF ct # NIL THEN RETURN END;
    ct := Thread.Fork(NEW(Closure, rl := rl))
  END StartClock;

PROCEDURE StopClock() = 
  BEGIN 
    quitClock := TRUE;
    EVAL Thread.Join(ct);
    ct := NIL
  END StopClock;

PROCEDURE A(cl : Closure) : REFANY =
  BEGIN
    LOOP
      WITH d = Date.FromTime(Time.Now()) DO
        cl.rl.asyncDisplay(Fmt.F("%02s:%02s:%02s\n",
                                 Fmt.Int(d.hour),
                                 Fmt.Int(d.minute),
                                 Fmt.Int(d.second)));
        IF quitClock THEN quitClock := FALSE; RETURN NIL END
        
      END;
      Thread.Pause(1.0d0);
    END
  END A;


(**********************************************************************)

PROCEDURE InitHelp() : Help.T =
  VAR
    subSubValueTime := HelpNode.New("time","time of day","Well, what time do you think it is?  Tell me!");
    subSubValueZIP := HelpNode.New("zip","ZIP code","Your ZIP code according to postal regulations blah blah");

    valueList := HelpNode.NewList(HelpNode.Arr{ subSubValueTime,subSubValueZIP });
    
    subShow := HelpNode.New("show","show values", "Use the show command to show values", valueList);
    subSet := HelpNode.New("set","set values", "Use the set command to set values", valueList);

    subs := HelpNode.NewList(HelpNode.Arr { subShow, subSet });
    root := HelpNode.New("testhelp",NIL,
                         "This is the testhelp program.  It lets you test the help system!", subs);
  BEGIN
    RETURN NEW(Help.T).init(root)
  END InitHelp;

(**********************************************************************)

PROCEDURE Cmd (ui : UI; reader : TextReader.T) : BOOLEAN
  RAISES { UserInterface.Error, UserInterface.Quit } =
  BEGIN
    TRY
      WITH t = ui.getIntf(),
           cmd = reader.nextE(" ") DO
        IF    TE(cmd, "quit") THEN RAISE UserInterface.Quit             
        ELSIF TE(cmd, "clock") THEN StartClock(t)
        ELSIF TE(cmd, "noclock") THEN StopClock()
        ELSIF TE(cmd, "help") THEN 
          WITH op = t.getPrompt() DO
            TRY
              t.setPrompt("(help)");
              help.command(reader,t);
            FINALLY
              t.setPrompt(op)
            END
          END
        ELSIF TE(cmd, "cat") THEN
          WITH fn = reader.nextE(" "),
               rd = FileRd.Open(fn),
               wx = Wx.New() DO
            TRY
              LOOP
                WITH c = Rd.GetChar(rd) DO
                  IF c # ReadLine.Null THEN Wx.PutChar(wx, c) END
                END
              END
            EXCEPT
              Rd.EndOfFile => Rd.Close(rd)
            END;
            t.display(Wx.ToText(wx))
          END
        END
      END;
      RETURN TRUE
    EXCEPT
      TextReader.NoMore => RAISE UserInterface.Error("not enough arguments")
    END
  END Cmd;

TYPE 
  UI = UserInterface.T OBJECT OVERRIDES
    command := Cmd;
  END;

VAR
  cnt := 1;
  help := InitHelp();
BEGIN

  WITH t = NEW(ReadLine.T).init(),
       intf = NEW(UI).init("testhelp") DO
    t.startProc();
    t.display("Hello there.\n");
    intf.run(t);
  END
END Main.









