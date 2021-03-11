(* $Id$ *)

MODULE ReadLineHelp;
IMPORT ReadLineHelpClass;
IMPORT ReadLine, ReadLineError;
IMPORT ReadLineHelpNode AS Node;
IMPORT ReadLineUI AS UI;
IMPORT Text, TextReader;
IMPORT Fmt, Rd, TextUtils;
IMPORT NetObj;
IMPORT Thread;

CONST TE = Text.Equal;

REVEAL
  T = ReadLineHelpClass.Private BRANDED Brand OBJECT
    root : Node.T;
  OVERRIDES
    init := Init;
    command := Command;
    topCommand := TopCommand;
    getRoot := GetRoot;
  END;

PROCEDURE GetRoot(t : T) : Node.T = BEGIN RETURN t.root END GetRoot;

PROCEDURE Init(t : T; root : Node.T) : T =
  BEGIN t.root := root; RETURN t END Init;

EXCEPTION NotFound;

PROCEDURE Lookup(word : TEXT; where : Node.T) : Node.T RAISES { NotFound,
                                                                UI.Error } =
  VAR 
    p := where.children;
    found : Node.T := NIL;
    error : TEXT := NIL;
  BEGIN
    WHILE p # NIL DO
      IF TextUtils.HavePrefix(p.head.name, word) THEN 
        IF found = NIL THEN
          found := p.head
        ELSE
          (* duplicate, ambiguous *)
          IF error = NIL THEN 
            error := p.head.name
          ELSE
            error := error & ", " & p.head.name
          END
        END
      END;
      p := p.tail
    END;

    IF found = NIL THEN
      RAISE NotFound
    ELSE
      IF error # NIL THEN
        RAISE UI.Error("Multiple matches for \"" & word & "\": " & error)
      ELSE
        RETURN found
      END
    END
  END Lookup;

PROCEDURE UnNil(t : TEXT) : TEXT = 
  BEGIN IF t = NIL THEN RETURN "" ELSE RETURN t END END UnNil;

PROCEDURE Command(t : T; r : TextReader.T; rl : ReadLine.Public) 
  RAISES { UI.Error, ReadLineError.E, UI.Quit } =

  PROCEDURE Next(VAR word : TEXT) : BOOLEAN =
    CONST 
      Delims = " \t";
    BEGIN
      RETURN r.next(Delims, word, TRUE);
    END Next;

  PROCEDURE Display(node : Node.T) RAISES { ReadLineError.E, NetObj.Error, Thread.Alerted } =
    VAR
      d := "Help for \"";
    BEGIN
      d := d & node.name & "\"\n  ";

      d := d & node.long & "\n\n";

      IF node.children # NIL THEN
        IF node.children.tail = NIL THEN
          d := d & "Subtopic:\n";
        ELSE
          d := d & "Subtopics:\n";
        END;
        VAR p := node.children; BEGIN
          WHILE p # NIL DO
            d := d & Fmt.F("  %-15s  %s\n", p.head.name, UnNil(p.head.short));
            p := p.tail
          END
        END
      ELSE
        d := d & "(No subtopics)\n"
      END;
      
      rl.display(d)
    END Display;

  PROCEDURE FirstWord(of : TEXT) : TEXT =
    BEGIN
      WITH idx = Text.FindChar(of,' ') DO
        IF idx = -1 THEN 
          RETURN of
        ELSE
          RETURN Text.Sub(of,0,idx)
        END
      END
    END FirstWord;

  PROCEDURE Help(op : TEXT; where : Node.T) RAISES { NotFound,
                                                     ReadLineError.E,
                                                     UI.Error,
                                                     UI.Quit,
                                                     NetObj.Error,
                                                     Thread.Alerted } =
    VAR
      word : TEXT;
    BEGIN
      IF Next(word) THEN
        (* still not at end *)
        WITH nd = Lookup(word,where),
             np = op & " > " & FirstWord(nd.name) DO
          TRY
            Help(np, nd);
          FINALLY
            rl.setPrompt(op)
          END
        END
      ELSE
        (* at end *)
        rl.setPrompt(op & " > ");
        Display(where);

        TRY
          WITH line = rl.readLine() DO
            rl.setPrompt(op);
            IF NOT TE(line,"") THEN
              NEW(T).init(where).command(NEW(TextReader.T).init(line),
                                         rl)
            END
          END
        EXCEPT
          Rd.EndOfFile => RAISE UI.Quit
        END
      END
    END Help;

  VAR
    where := t.root;
    oldPrompt : TEXT;
    resetPrompt := TRUE;
  BEGIN
    TRY
      TRY
        TRY
          TRY
            oldPrompt := rl.getPrompt();
            Help(oldPrompt, where)
          EXCEPT
            NetObj.Error => RAISE UI.Quit
          END
        EXCEPT
          UI.Quit => resetPrompt := FALSE; RAISE UI.Quit
        |
          NotFound => RAISE UI.Error("no help for that")
        END
      FINALLY
        IF resetPrompt THEN rl.setPrompt(oldPrompt) END
      END
    EXCEPT
      NetObj.Error, Thread.Alerted => RAISE UI.Quit 
    END
  END Command;

PROCEDURE TopCommand(t : T; r : TextReader.T; rl : ReadLine.Public) 
  RAISES { UI.Error, ReadLineError.E, UI.Quit } =
  BEGIN
    TRY
      WITH op = rl.getPrompt() DO
        TRY
          rl.setPrompt("(help)");
          t.command(r,rl);
        FINALLY
          rl.setPrompt(op)
        END
      END
    EXCEPT
      NetObj.Error, Thread.Alerted => RAISE UI.Quit
    END
  END TopCommand;

BEGIN END ReadLineHelp.
