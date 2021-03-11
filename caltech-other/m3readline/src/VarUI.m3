(* $Id$ *)

MODULE VarUI;
IMPORT SortedTextRefTbl, ReadLineUI;
IMPORT Fmt;
IMPORT ReadLine, TextReader, ReadLineError;
IMPORT ReadLineHelpNode AS HelpNode;
IMPORT Text;
IMPORT VarProxyClass;
IMPORT Rd;
IMPORT NetObj;
IMPORT Thread;

CONST TE = Text.Equal;

REVEAL
  T = Public BRANDED Brand OBJECT
    tab : SortedTextRefTbl.T;
  OVERRIDES
    init := Init;
    addVar := AddVar;
    command := Command;
  END;

PROCEDURE MakeHelp() : HelpNode.T =
  CONST
    New = HelpNode.New;
    NewList = HelpNode.NewList;
  BEGIN
    WITH list = NewList(HelpNode.Arr {
      New("set <var> <value>", "set variable", "Set variable.  \"show\" lists all variables."),
      New("show [<var>]", "show variable", "Show variable. \"show\" lists all variables.")
      }),

         root = New(Brand, NIL, "", list) DO
      RETURN root
    END
  END MakeHelp;

PROCEDURE Init(t : T; myName : TEXT) : T =
  BEGIN
    EVAL ReadLineUI.T.init(t, myName, MakeHelp());
    t.tab := NEW(SortedTextRefTbl.Default).init();

    RETURN t
  END Init;

TYPE PRec = OBJECT varProxy : VarProxy; comment : TEXT END;

PROCEDURE AddVar(t : T; varName : TEXT; 
                 varProxy : VarProxy; 
                 comment : TEXT) : BOOLEAN =
  BEGIN 
    RETURN t.tab.put(varName,
                     NEW(PRec, varProxy := varProxy, comment := comment)) 
  END AddVar;

PROCEDURE Command(ui : T; r : TextReader.T) : BOOLEAN
  RAISES { ReadLineUI.Error, ReadLineUI.Quit } = 

  PROCEDURE Show(var : TEXT) RAISES { ReadLineUI.Error,
                                      ReadLineUI.Quit, 
                                      NotFound } =
    VAR n, x : TEXT; r : REFANY; BEGIN
      TRY
        IF var = NIL THEN
          WITH table = NEW(ReadLine.Table).init(3, fmt := "%-28s %28s   %-40s\n"),
               iter = ui.tab.iterateOrdered() DO
            WHILE iter.next(n,r) DO
              WITH pr = NARROW(r,PRec), p = pr.varProxy DO
                LOCK p.mu DO
                  IF p.mode = ProxyMode.ReadOnly THEN
                    x := " [read-only]"
                  ELSE
                    x := ""
                  END;
                  table.addRow(ARRAY OF TEXT { n & x, p.show(n), pr.comment })
                END
              END
            END;
            ui.getIntf().displayTbl(table)
          END
        ELSE
          WITH p = Lookup(var) DO 
            LOCK p.mu DO
              ui.getIntf().display(Fmt.F("%s   %s\n",var,p.show(var)))
            END
          END
        END
      EXCEPT
        Thread.Alerted, NetObj.Error, Rd.EndOfFile, ReadLineError.E => RAISE ReadLineUI.Quit
      END
    END Show;

  PROCEDURE Set(var : TEXT; to : TEXT) RAISES { NotFound,
                                                ReadLineUI.Error } =
    BEGIN
      WITH p = Lookup(var) DO 
        LOCK p.mu DO 
          IF p.mode = ProxyMode.ReadOnly THEN
            RAISE ReadLineUI.Error("Attempting to set value of read-only variable " & var)
          END;
          p.set(var,to) 
        END 
      END
    END Set;

  PROCEDURE Lookup(nam : TEXT) : VarProxy RAISES { NotFound } =
    VAR r : REFANY; BEGIN
      IF NOT ui.tab.get(nam,r) THEN RAISE NotFound(nam) END;
      RETURN NARROW(r,PRec).varProxy
    END Lookup;

  BEGIN
    TRY
      IF ReadLineUI.T.command(ui,r) THEN RETURN TRUE END;

      WITH cont = r.save(), 
           cmd = r.get() DO
        IF TE(cmd, "show") THEN 
          IF r.empty() THEN 
            Show(NIL) 
          ELSE
            TRY
              LOOP Show(r.get()) END
            EXCEPT
              TextReader.NoMore => (* skip *)
            END
          END
        ELSIF TE(cmd, "set") THEN
          WITH k1 = r.get(), k2 = r.get() DO Set(k1, k2) END
        ELSE
          r.continue(cont); RETURN FALSE
        END;
        RETURN TRUE
      END
    EXCEPT
      NotFound(nam) => RAISE ReadLineUI.Error("Variable \"" & nam & "\" not found")
    |
      TextReader.NoMore => RAISE ReadLineUI.Error("not enough arguments")
    | 
      NetObj.Error, Thread.Alerted => RAISE ReadLineUI.Quit
    END
  END Command;

EXCEPTION NotFound(TEXT);

BEGIN END VarUI.
