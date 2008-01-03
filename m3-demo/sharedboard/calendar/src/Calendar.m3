(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

MODULE Calendar EXPORTS Main;

IMPORT Params, Process, NetObj, Thread, Trestle, VBT, IO, Atom, ItemSeq,
       Color, Stdio, TrestleComm, Fmt,
       Callback, ClientInfo, TextItem, RuleItem, Item, ItemClass, ItemFont,
       Board, BoardServer, PointR, RectR;

EXCEPTION Usage;

VAR boardName: TEXT;
    serverAddress: NetObj.Address;
    bs: BoardServer.T;
    board: Board.T;
    ci: ClientInfo.T;

CONST UsageMessage = "Usage: Calendar <board name> [<server name>] \n";


PROCEDURE ReadCommandLine () RAISES {Usage, NetObj.Invalid, NetObj.Error} =
<* FATAL Thread.Alerted *>
  BEGIN
    IF Params.Count < 2 THEN
      RAISE Usage;
    ELSE 
      boardName := Params.Get (1);
    END;
    IF Params.Count = 2 THEN
      serverAddress := NIL;
    ELSE
      TRY
        serverAddress := NetObj.Locate (Params.Get (2));
      EXCEPT
        NetObj.Error =>  IO.Put ("Error locating server daemon" 
          & Params.Get (2) & "\n", Stdio.stderr); 
        Process.Exit (1);
      END;
    END;
    TRY
      bs := NetObj.Import ("BoardServer", serverAddress);
    EXCEPT
      NetObj.Error =>  IO.Put ("Error importing board server \n",
                             Stdio.stderr); 
      Process.Exit (1);
    END;
    TRY 
      board := bs.create (boardName);
      ci := board.register (NEW (Callback.T));
    EXCEPT
      BoardServer.Failed (text) => IO.Put ("Can't create board: " 
        & text & "\n", Stdio.stderr); 
      Process.Exit (1);
    END;
  END ReadCommandLine;

CONST YearNameHt = 24.0;
      TotalHt = 700.0;
      TotalWd = 850.0;
      
VAR items := NEW (ItemSeq.T).init (500);
    font := ItemFont.FromName ("-*-helvetica-bold-r-*-*-*-10-*-*-*-*-iso8859-1");
    color1 := Color.Blue;
    color2 := Color.Green;

PROCEDURE Build () RAISES {NetObj.Error} =
<* FATAL Thread.Alerted *>
  BEGIN
    items.addhi (NEW (TextItem.T, text := "1993", 
                      rp :=PointR.T{h := 0.0, v := -5.0},
                      font := ItemFont.Scale (font, 1.0/YearNameHt),
                      color := color1));
    FOR j := 0 TO 3 DO
      FOR i := 0 TO 2 DO
        BuildMonth (i, j);
      END;
    END;

    VAR arr := NEW (Item.TArray, items.size()); BEGIN
      FOR i := FIRST(arr^) TO LAST(arr^) DO
        arr[i] := items.get (i);
        TYPECASE arr[i] OF
          TextItem.T (ti) => AdjustTextBox (ti);
        ELSE (*SKIP*)
        END;
        arr[i].move (PointR.T{h:=10.0, v:=YearNameHt+10.0});
     END;
      EVAL board.createItems (ci, arr);
    END;
    board.unregister (ci);
    TRY
      bs.close (boardName);
    EXCEPT
      BoardServer.Failed (text) => IO.Put (text & "\n", Stdio.stderr); 
      Process.Exit (1);
    END;
  END Build; 

CONST MonthHt = (TotalHt - YearNameHt)/3.0;
      MonthWd = TotalWd/4.0;
      MonthNameHt = 12.0;
      GutterWd = 20.0;
      GutterHt = 20.0;
      DayHt = (MonthHt-GutterHt-MonthNameHt-DayNameHt-DayNameGutterHt)/6.0;
      DayWd = (MonthWd-GutterWd)/7.0;
      DayNameHt = 8.0;
      DayNameGutterHt = 8.0;
      DateNameHt = 6.0;

CONST MonthName = ARRAY OF TEXT {"January", "February", "March", "April",
                                 "May", "June", "July", "August", 
                                 "September", "October", "November", "December"};
      MonthDays = ARRAY OF INTEGER {31,28,31,30,31,30,31,31,30,31,30,31};
      MonthOffsets = ARRAY OF INTEGER {5,1,1,4,6,2,4,0,3,5,1,3};
      (* the week day for the first of the month *)
      DayName = ARRAY OF TEXT {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};
      FullDayName = ARRAY OF TEXT {"Sunday", "Monday", "Tuesday", 
                                   "Wednesday", "Thursday", "Friday", 
                                   "Saturday"};


VAR monthFont := ItemFont.Scale (font, 1.0/MonthNameHt);
    dayFont := ItemFont.Scale (font, 1.0/DayNameHt);
    dateFont := ItemFont.Scale (font, 1.0/DateNameHt);
    smallFont := ItemFont.Scale (font, 2.0);

PROCEDURE BuildMonth (i, j: INTEGER) =
  VAR nw := PointR.T{FLOAT(j)*MonthWd, FLOAT(i)*MonthHt+MonthNameHt};
      num := i*4 + j;
  BEGIN
    items.addhi (NEW (TextItem.T, text := MonthName[num], 
                      rp :=nw,
                      font := monthFont,
                      color := color1));
    nw.v := nw.v + DayNameHt + DayNameGutterHt*0.75;
    FOR i := 0 TO 6 DO
      items.addhi (NEW (TextItem.T, text := DayName[i], 
                        rp :=PointR.T{nw.h+FLOAT(i)*DayWd, nw.v},
                        font := dayFont,
                        color := color1));
    END;
    nw.v := nw.v + DayNameGutterHt*0.25;
    FOR i := 0 TO 7 DO
      items.addhi (NEW (RuleItem.T, box := RectR.T{west:=nw.h+FLOAT (i)*DayWd,
                                            east:=nw.h+FLOAT(i)*DayWd+1.0,
                                            north:=nw.v,
                                            south:=nw.v+6.0*DayHt},
                        color := color2));
    END;
    FOR i := 0 TO 6 DO
      items.addhi (NEW (RuleItem.T, box := RectR.T{west:=nw.h,
                                            east:=nw.h+7.0*DayWd,
                                            north:=nw.v+FLOAT(i)*DayHt,
                                            south:=nw.v+FLOAT(i)*DayHt+1.0},
                        color := color2));
    END;
    FOR i := 0 TO MonthDays[num]-1 DO
      VAR m := i + MonthOffsets[num];
          week := m DIV 7;
          day := m MOD 7;
      BEGIN
        items.addhi (NEW (TextItem.T, text := Fmt.Int (i+1), 
                          rp := PointR.T{nw.h+FLOAT(day)*DayWd+2.0,
                                         nw.v+FLOAT(week)*DayHt+DateNameHt+1.0},
                          font := dateFont,
                          color := color1));
        items.addhi (NEW (TextItem.T, 
                          text := DayName[day] & ", " & MonthName[num]
                                      & " 1993",
                          rp := PointR.T{nw.h+FLOAT(day)*DayWd+2.0,
                                         nw.v+FLOAT(week)*DayHt+2.0},
                          font := smallFont,
                          color := color1));
      END;
    END;
  END BuildMonth;

PROCEDURE AdjustTextBox (it: TextItem.T) =
<* FATAL ItemFont.TooSmall, ItemFont.TooBig, ItemFont.Invisible *>
  VAR scale := 24.0/ItemFont.Size (it.font);
      font := ItemFont.ToFont (it.font, scale);
      bb := VBT.BoundingBox (dummyVBT, it.text, font);
      width := FLOAT (bb.east-bb.west)*1.125;
  BEGIN
    it.box := RectR.Add (RectR.T{west := FLOAT (bb.west)/scale,
                                 east := (FLOAT (bb.west)+width)/scale,
                                 north := FLOAT (bb.north)/scale,
                                 south := FLOAT (bb.south)/scale},
                         it.rp);
  END AdjustTextBox;


VAR dummyVBT := NEW (VBT.Leaf);
<* FATAL TrestleComm.Failure *>
BEGIN
  Trestle.Install (dummyVBT);
  TRY
    ReadCommandLine ();
    Build ();
  EXCEPT
    Usage, NetObj.Invalid => IO.Put (UsageMessage, Stdio.stderr);
      Process.Exit (1);
  | NetObj.Error (codes) => IO.Put ("NetObj Error: ", Stdio.stderr);
      WHILE codes # NIL DO
        IO.Put (Atom.ToText (codes.head) & " ", Stdio.stderr);
        codes := codes.tail;
      END;
      Process.Exit (1);
  END;
  Trestle.Delete (dummyVBT);
END Calendar.
