MODULE MagSlasherUtils;
IMPORT Conf;
IMPORT FileRd;
IMPORT Fmt;
IMPORT MagCell;
IMPORT OSError;
IMPORT Pathname;
IMPORT Process;
IMPORT Rd;
IMPORT TextMagLayerTbl;
IMPORT Thread;

PROCEDURE FromMagPath(path: Pathname.T): T =
  VAR 
    cell : MagCell.T;
    layerDB : TextMagLayerTbl.T;
    src := path;
    rd : Rd.T;
  BEGIN
      TRY
    rd := FileRd.Open("router.conf");
    layerDB := Conf.ReadConf(rd)
  EXCEPT
    OSError.E =>
      Process.Crash("MagSlasherUtils: Couldn't open config file!")
  |
    Thread.Alerted, Rd.Failure =>
      Process.Crash("MagSlasherUtils: Problems reading config file!")
  | 
    Conf.ParseError(lno) =>
      Process.Crash("MagSlasherUtils: Parse error reading config file on line "&Fmt.Int(lno)&
        "!")
  |   
    Conf.LayerNotFound(lname) =>
      Process.Crash("MagSlasherUtils: Layer \"" & lname & "\" in conf file not found!")
  END;

  TRY
    cell := NEW(MagCell.Labelled).lookup(src,layerDB)
  EXCEPT
    MagCell.NotFound(c) =>
      Process.Crash("MagSlasherUtils: Couldn't find mag cell \""&c&
        "\" while attempting to open \""&src&"\"!")
  | 
    MagCell.SyntaxError => 
      Process.Crash("MagSlasherUtils: Syntax error while reading mag cell \""&src&"!")
  |
    Thread.Alerted, Rd.Failure =>
      Process.Crash("MagSlasherUtils: I/O error reading mag cell \""&src&"!")
  END;

    RETURN NEW(T).init(cell);
  END FromMagPath;

BEGIN
END MagSlasherUtils.
