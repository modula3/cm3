(* $Id$ *)

MODULE Main;
IMPORT MagCell, MagSlasher;
IMPORT Params;
IMPORT TextMagLayerTbl;
IMPORT Process, OSError, Stdio, Rd, Conf, FileRd, Fmt, Thread, Wr;

VAR 
  cell : MagCell.T;
  layerDB : TextMagLayerTbl.T;
  src := Params.Get(1);
  rd : Rd.T;
BEGIN
  TRY
    rd := FileRd.Open("router.conf");
    layerDB := Conf.ReadConf(rd)
  EXCEPT
    OSError.E =>
      Process.Crash("Couldn't open config file!")
  |
    Thread.Alerted, Rd.Failure =>
      Process.Crash("Problems reading config file!")
  | 
    Conf.ParseError(lno) =>
      Process.Crash("Parse error reading config file on line "&Fmt.Int(lno)&
        "!")
  |   
    Conf.LayerNotFound(lname) =>
      Process.Crash("Layer \"" & lname & "\" in conf file not found!")
  END;

  TRY
    cell := NEW(MagCell.Labelled).lookup(src,layerDB)
  EXCEPT
    MagCell.NotFound(c) =>
      Process.Crash("Couldn't find mag cell \""&c&
        "\" while attempting to open \""&src&"\"!")
  | 
    MagCell.SyntaxError => 
      Process.Crash("Syntax error while reading mag cell \""&src&"!")
  |
    Thread.Alerted, Rd.Failure =>
      Process.Crash("I/O error reading mag cell \""&src&"!")
  END;

  VAR
    slasher := NEW(MagSlasher.T).init(cell);
  BEGIN
    TRY
      LOOP
        VAR
          line := Rd.GetLine(Stdio.stdin);
          slashed : TEXT;
          gotIt := slasher.slash(line, slashed);
        BEGIN
          IF gotIt THEN
            Wr.PutText(Stdio.stdout, line & " FOUND AS " & slashed & "\n")
          ELSE
            Wr.PutText(Stdio.stdout, line & " NOT FOUND\n")
          END;
          Wr.Flush(Stdio.stdout)
        END
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    |
      Wr.Failure, Thread.Alerted =>
        Process.Crash("I/O error writing standard output.")
    |
      Rd.Failure =>
        Process.Crash("I/O error reading from standard input.")
    END
  END
END Main.
