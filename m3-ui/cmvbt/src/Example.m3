
MODULE Example;
IMPORT TabVBT, Trestle, TextVBT, VBT;
IMPORT HVSplit, Axis;
IMPORT IPTypeinVBT, TextPort;
IMPORT FrameVBT, PaintOp, Shadow;
IMPORT TrestleComm;
IMPORT IO, Fmt;
IMPORT BorderedVBT, HoverVBT;
IMPORT GridSplit;
IMPORT FmtTime, Time (**, Text**);
IMPORT SortedTableVBT;
IMPORT TypeinVBT;
IMPORT PasswordFont;

PROCEDURE DoIt () =
  CONST
    names = ARRAY OF TEXT {"Active Text", "IP Address", "Passive Text"};
  VAR
    contents: ARRAY [FIRST(names)..LAST(names)] OF VBT.T;
  VAR
    lightgray := PaintOp.FromRGB (0.80, 0.80, 0.80);
    darkgray  := PaintOp.FromRGB (0.20, 0.20, 0.20);
    shadow := Shadow.New(light := lightgray, dark  := darkgray);
  VAR
    ipv, info, log, tabs, passwd, frames, hover, top: VBT.T;
  BEGIN
    info := TextVBT.New ("Welcome!");

    log := NewLogVBT (info);

    ipv := NewIPTypein (info, log);

    contents[0] := NewActiveVBT ("Some Active Text...", info, log);
    contents[1] := ipv;
    contents[2] := TextVBT.New ("Some Passive Text...");
    tabs := TabVBT.New (names, contents, shadow := shadow);

    passwd := NEW(TypeinVBT.T).init (font := PasswordFont.New ());

    frames := HVSplit.Cons (Axis.T.Ver, 
                FrameVBT.New(tabs, title := "Widget Factory", shadow := shadow),
                FrameVBT.New(passwd, "Password", shadow := shadow),
                FrameVBT.New(log, "Event Log", shadow := shadow),
                FrameVBT.New(info, "Info", shadow := shadow));

    hover := HoverVBT.New (frames);

    top   := BorderedVBT.New (hover, size := 10.0, op := PaintOp.Bg);
    TextPort.ChangeAllTextPorts (top, TextPort.Model.Emacs);

    Note (log, "Starting Time");
    TRY

      Trestle.Install (NEW(TypeinVBT.T).init(font := PasswordFont.New()));

      Trestle.Install (top);
    EXCEPT TrestleComm.Failure =>
      IO.Put ("Could not start a window.\n");
      RETURN;
    END;

    Trestle.AwaitDelete (top);

    TRY
      WITH ip = IPTypeinVBT.Get(ipv) DO
        IO.Put ("\n\nThe final IP address was: ");
        FOR i := FIRST(ip.a) TO LAST(ip.a) DO
          IO.Put (Fmt.Int (ip.a[i]));
          IF i # 3 THEN IO.Put (".") END;
        END;
      END;
      IO.Put ("\n");
    EXCEPT IPTypeinVBT.InvalidAddress =>
      IO.Put ("The IP address " & TextPort.GetText(ipv) & "is invalid.\n");
    END;
  END DoIt;

(*-------------------------------------------------a sorted table ----*)
  
TYPE
  LogVBT = SortedTableVBT.T OBJECT
    headings : ARRAY [0..1] OF TEXT;
    sortbycol: CARDINAL  := 0;
    count    : CARDINAL  := 0;
    info     : TextVBT.T := NIL;
  OVERRIDES
    content := ContentClick;
    heading := HeadingClick;
  END;

TYPE 
  LogEntry = TextVBT.T OBJECT
    count: CARDINAL; 
  END;

PROCEDURE NewLogVBT (info: TextVBT.T): LogVBT =
  VAR log := NEW (LogVBT, info := info, order := Order);
  BEGIN
    log.headings[0] := " Event ";
    log.headings[1] := " Time ";
    EVAL log.init (log.headings, ARRAY OF CARDINAL {100, 100});
    RETURN log;
  END NewLogVBT;

PROCEDURE Note (v: LogVBT;  msg: TEXT) = 
  VAR 
    v1 := TextVBT.New (msg);
    v2 := NEW (LogEntry, count := v.count).init(FmtTime.Long(Time.Now()));
  BEGIN
    EVAL v.insert_sorted (ARRAY OF VBT.T {v1, v2});
    INC (v.count);
  END Note;

PROCEDURE Order (<*UNUSED*> v: SortedTableVBT.T;
                 READONLY data: ARRAY OF VBT.T): INTEGER = 
  VAR e: LogEntry := data[1];
  BEGIN
    RETURN e.count;
  END Order;

PROCEDURE ContentClick (v: LogVBT;  row: CARDINAL;
                        <*UNUSED*> READONLY cd: VBT.MouseRec) =
  VAR data := NEW(REF ARRAY OF VBT.T, GridSplit.NumCols(v.contents()));
  BEGIN
    v.delete (row, data^);
  END ContentClick;

PROCEDURE HeadingClick (v: LogVBT;  col: CARDINAL;
                        <*UNUSED*> READONLY cd: VBT.MouseRec) =
  BEGIN
    TextVBT.Put (v.info, "Sort by " & v.headings[col]);
    v.sortbycol := col;
  END HeadingClick;

(*------------------------------------------------- active tab page ----*)

TYPE
  ActiveVBT = TextVBT.T OBJECT
    info : TextVBT.T;
    log  : LogVBT;
  END;

PROCEDURE NewActiveVBT (msg: TEXT;  info: TextVBT.T;  log: LogVBT): ActiveVBT = 
  VAR v := NEW (ActiveVBT, info := info, log := log);
  BEGIN
    EVAL v.init (msg);
    HoverVBT.Register (v, 0.5d0,  ActiveHover, NIL);
    RETURN v;
  END NewActiveVBT;

PROCEDURE ActiveHover (self: VBT.T;  <*UNUSED*> ref: REFANY) =
  VAR v: ActiveVBT := self;  msg := TextVBT.Get (v);
  BEGIN
    TextVBT.Put (v.info, "Pointing at \"" & msg & "\"...");
    Note (v.log, msg);
  END ActiveHover;

(*-------------------------------------------------  IP Typeins ----*)

TYPE
  IPTypein = IPTypeinVBT.T OBJECT
    info : TextVBT.T;
    log  : LogVBT;
  OVERRIDES
    returnAction := IPReturnAction;
  END;

PROCEDURE NewIPTypein (info: TextVBT.T;  log: LogVBT): IPTypein =
  VAR v := NEW (IPTypein, info := info, log := log);
  BEGIN
    EVAL v.init ();
    HoverVBT.Register (v, 1.0d0, IPHover, NIL);
    RETURN v;
  END NewIPTypein;

PROCEDURE IPHover (self: VBT.T;  <*UNUSED*> ref: REFANY) =
  VAR v: IPTypein := self;
  BEGIN
    TextVBT.Put (v.info, "Enter an IP Address.");
  END IPHover;

PROCEDURE IPReturnAction (v: IPTypein; <*UNUSED*> READONLY cd: VBT.KeyRec) = 
  VAR
    addr := TextPort.GetText (v);
    desc := "IP Address: " & addr;
  BEGIN
    TextVBT.Put (v.info, "New IP entered: " & addr);
    TRY 
      EVAL IPTypeinVBT.Get(v);
    EXCEPT
      IPTypeinVBT.InvalidAddress => desc := "Invalid " & desc;
    END;
    Note (v.log, desc);
  END IPReturnAction;

(*-------------------------------------------------  Main Body ----*)

BEGIN
  DoIt ();
END Example.
