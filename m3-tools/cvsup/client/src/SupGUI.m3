(* Copyright 1996-2003 John D. Polstra.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgment:
 *      This product includes software developed by John D. Polstra.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $Id$ *)
<* PRAGMA LL *>

MODULE SupGUI;

IMPORT
  AutoRepeat, CVProto, Detailer, ErrMsg, EventSync, FileUpdater,
  FileWr, Fmt, FormsVBT, FSClient, IP, Logger, OSError, Pathname, Rd,
  Rsrc, SplitLogger, SupFile, SupGUIBundle, SupMisc, Text, TextPort,
  TextPortLogger, TextEditVBT, TextVBT, TextVBTLogger, Thread, Time,
  TokScan, TreeList, Trestle, TrestleComm, TypeinVBT, UnixMisc,
  Updater, VBT, Version, Wr;

CONST
  LogName = "cvsup.log";

<* FATAL FormsVBT.Unimplemented *>

(*****************************************************************************)
(* Main Controller. *)
(*****************************************************************************)

TYPE
  Controller = Thread.Closure OBJECT
    fv: FormsVBT.T;
    supFile: Pathname.T;
    config: FSClient.Configuration;
  METHODS
    init(fv: FormsVBT.T;
	 supFile: Pathname.T;
	 config: FSClient.Configuration): Controller := ControllerInit;
  OVERRIDES
    apply := ControllerApply;
  END;

PROCEDURE ControllerInit(self: Controller;
                         fv: FormsVBT.T;
			 supFile: Pathname.T;
			 config: FSClient.Configuration): Controller =
  BEGIN
    self.fv := fv;
    self.supFile := supFile;
    self.config := config;
    RETURN self;
  END ControllerInit;

PROCEDURE ControllerApply(self: Controller): REFANY =
  <* FATAL EventSync.Error, FormsVBT.Error, Thread.Alerted *>
  VAR
    totalStats: TotalStats;
    clock: Clock;
    listerVBT: TextVBT.T;
    detailerVBT: TextVBT.T;
    messagesVBT: TextPort.T;
    filterVBT: TypeinVBT.T;
    statusLogger: Logger.T;
    listerTrace: Logger.T;
    detailerTrace: Logger.T;
    messagesLogger: Logger.T;
    client: FSClient.T;
    clientThread: Thread.T;
    aborterThread: Thread.T;
    clientStatus: SupMisc.ThreadStatus;
    filterText := "";
    ts: TokScan.T;
  BEGIN
    listerVBT := FormsVBT.GetVBT(self.fv, "lister");
    detailerVBT := FormsVBT.GetVBT(self.fv, "detailer");
    messagesVBT :=
      NARROW(FormsVBT.GetVBT(self.fv, "messages"), TextEditVBT.T).tp;
    filterVBT :=
      NARROW(FormsVBT.GetVBT(self.fv, "filter"), TypeinVBT.T);

    (* Translate the command-line accept filter into a text string for
       the display. *)
    WITH accepts = self.config.override.accepts DO
      FOR i := 0 TO accepts.size()-1 DO
	IF i = 0 THEN
	  filterText := accepts.get(i);
	ELSE
	  filterText := filterText & " " & accepts.get(i);
	END;
      END;
    END;

    LOCK VBT.mu DO
      messagesVBT.setModel(TextPort.Model.Xterm);
      filterVBT.setModel(TextPort.Model.Xterm);
      FormsVBT.PutText(self.fv, "filter", filterText);
    END;

    listerTrace := NEW(TextVBTLogger.T).init(
      listerVBT, level := Logger.Priority.Info);
    detailerTrace := NEW(TextVBTLogger.T).init(
      detailerVBT, level := Logger.Priority.Info);

    statusLogger := NEW(TextVBTLogger.T).init(
      FormsVBT.GetVBT(self.fv, "status"), level := Logger.Priority.Info);
    messagesLogger := NEW(TextPortLogger.T).init(
      messagesVBT, level := Logger.Priority.Info);

    self.config.trace := NEW(SplitLogger.T).init(messagesLogger, statusLogger,
      SET OF Logger.Priority{Logger.Priority.Emerg..Logger.Priority.Warning},
      SET OF Logger.Priority{Logger.Priority.Emerg..Logger.Priority.Debug});
    self.config.listerTrace :=
      NEW(SplitLogger.T).init(messagesLogger, listerTrace);
    self.config.detailerTrace :=
      NEW(SplitLogger.T).init(messagesLogger, detailerTrace);
    self.config.updaterTrace := messagesLogger;

    (* Wait for the initial start command. *)
    IF EventSync.Wait(self.fv, "quit=0 start=1 filter=1") = 1 THEN  (* start *)
      LOOP
	(* Clear the trace and statistics displays. *)
	LOCK VBT.mu DO
	  TextVBT.Put(listerVBT, "");
	  TextVBT.Put(detailerVBT, "");
	  TextPort.SetText(messagesVBT, "");
	  ResetStatsDisplay(self);
	  VBT.Release(filterVBT, VBT.KBFocus);
	END;

	totalStats := NEW(TotalStats).init(self.fv);

	self.config.listerStats := NEW(ListerStats).init(self.fv, totalStats);
	self.config.detailerStats :=
	  NEW(DetailerStats).init(self.fv, totalStats);
	self.config.updaterStats := NEW(UpdaterStats).init(self.fv, totalStats);

	totalStats.ls := self.config.listerStats;
	totalStats.ds := self.config.detailerStats;
	totalStats.us := self.config.updaterStats;

	(* Build a filter list from the text in the TypeIn. *)
	WITH accepts = self.config.override.accepts DO
	  FOR i := 0 TO accepts.size()-1 DO EVAL accepts.remhi() END;
	  ts := TokScan.New(FormsVBT.GetText(self.fv, "filter"));
	  <* FATAL TokScan.Error *>
	  BEGIN
	    WHILE ts.next(filterText) DO
	      accepts.addhi(filterText);
	    END;
	  END;
	END;

	clock := NEW(Clock).init(self.fv, "clock", self.config.updaterStats);

	TRY
	  Msg(self, "Parsing supfile \"" & self.supFile & "\"");
	  self.config.collections := SupFile.Parse(self.supFile,
	    override := self.config.override,
	    mask := self.config.overrideMask);

	  client := NEW(FSClient.T).init(self.config);

	  (* Start the client. *)
	  clientThread := Thread.Fork(client);
	  AutoRepeat.Start(clock);
	  aborterThread :=
	    Thread.Fork(NEW(Aborter).init(self.fv, clientThread));

	  (* Wait for the client to finish. *)
	  clientStatus := Thread.Join(clientThread);
	  AutoRepeat.Stop(clock);
	  Thread.Alert(aborterThread);
	  EVAL Thread.Join(aborterThread);
	EXCEPT SupFile.Error(msg) =>
	  Msg(self, msg);
	END;

	CASE EventSync.Wait(self.fv, "quit=0 start=1 filter=1 save=2") OF
	| 0 => (* quit *)
	    EXIT;
	| 1 => (* start *)
	    (* Continue *)
	| 2 => (* save *)
	    TRY
	      SaveTrace(messagesVBT, LogName);
	      Msg(self, "Saved message window to \"" & LogName & "\"");
	    EXCEPT Error(msg) =>
	      Msg(self, msg);
	    END;
	    IF EventSync.Wait(self.fv, "quit=0 start=1 filter=1") = 0 THEN
	      (* quit *)
	      EXIT;
	    END;
	ELSE <* ASSERT FALSE *> END;
      END;
    END;

    LOCK VBT.mu DO Trestle.Delete(self.fv) END;
    RETURN NIL;
  END ControllerApply;

PROCEDURE Msg(self: Controller;
              msg: TEXT;
	      append := FALSE) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    LOCK VBT.mu DO
      FormsVBT.PutText(self.fv, "status", msg, append);
    END;
  END Msg;

PROCEDURE ResetStatsDisplay(self: Controller) =
  <* LL = VBT.mu *>
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  CONST
    UpdateRows = ARRAY OF TEXT{
      "edit", "co", "rsync", "app", "touch",
      "add", "del", "repl", "fixup", "other", "tot"
    };
    UpdateCols = ARRAY OF TEXT{
      "Count", "FileKB", "WireKB", "Compr"
    };
    CommRows = ARRAY OF TEXT{
      "lister", "detailer", "updater", "tot"
    };
    CommCols = ARRAY OF TEXT{
      "CommIn", "CommOut", "WireIn", "WireOut", "WireTot"
    };
  VAR
    name, value: TEXT;
  BEGIN
    FOR i := FIRST(UpdateRows) TO LAST(UpdateRows) DO
      FOR j := FIRST(UpdateCols) TO LAST(UpdateCols) DO
	name := UpdateRows[i] & UpdateCols[j];
	IF Text.Equal(UpdateCols[j], "Compr") THEN
	  value := "0.0";
	ELSE
	  value := "0";
	END;
	FormsVBT.PutText(self.fv, name, value);
      END;
    END;

    FOR i := FIRST(CommRows) TO LAST(CommRows) DO
      FOR j := FIRST(CommCols) TO LAST(CommCols) DO
	name := CommRows[i] & CommCols[j];
	IF Text.Equal(name, "listerCommIn")
	OR Text.Equal(name, "listerWireIn")
	OR Text.Equal(name, "updaterCommOut")
	OR Text.Equal(name, "updaterWireOut") THEN
	  value := "-";
	ELSE
	  value := "0";
	END;
	FormsVBT.PutText(self.fv, name, value);
      END;
    END;

    FormsVBT.PutText(self.fv, "clock", "00:00:00");
    FormsVBT.PutText(self.fv, "xferRate", "    0.0 KB/sec");
  END ResetStatsDisplay;

PROCEDURE SaveTrace(tp: TextPort.T; fileName: TEXT)
  RAISES {Error, Thread.Alerted} =
  CONST
    ChunkSize = 8 * 1024;
  VAR
    wr: Wr.T;
    pos, limit, len: CARDINAL;
  BEGIN
    TRY
      wr := FileWr.Open(fileName);
    EXCEPT OSError.E(list) =>
      RAISE Error("Cannot create \"" & fileName & "\": " &
	ErrMsg.StrError(list));
    END;
    TRY
      LOCK VBT.mu DO
	len := TextPort.Length(tp);
	pos := 0;
	WHILE pos < len DO
	  limit := MIN(pos+ChunkSize, len);
	  TRY
	    Wr.PutText(wr, TextPort.GetText(tp, pos, limit));
	  EXCEPT Wr.Failure(list) =>
	    RAISE Error("Write failure on \"" & fileName & "\": " &
	      ErrMsg.StrError(list));
	  END;
	  pos := limit;
	END;
      END;
    FINALLY
      TRY
	Wr.Close(wr);
      EXCEPT Wr.Failure(list) =>
	RAISE Error("Cannot close \"" & fileName & "\": " &
	  ErrMsg.StrError(list));
      END;
    END;
  END SaveTrace;

(*****************************************************************************)
(* Aborter. *)
(*****************************************************************************)

TYPE
  Aborter = Thread.Closure OBJECT
    fv: FormsVBT.T;
    clientThread: Thread.T;
  METHODS
    init(fv: FormsVBT.T;
	 clientThread: Thread.T): Aborter := AborterInit;
  OVERRIDES
    apply := AborterApply;
  END;

PROCEDURE AborterInit(self: Aborter;
                      fv: FormsVBT.T;
		      clientThread: Thread.T): Aborter =
  BEGIN
    self.fv := fv;
    self.clientThread := clientThread;
    RETURN self;
  END AborterInit;

PROCEDURE AborterApply(self: Aborter): REFANY =
  <* FATAL EventSync.Error, FormsVBT.Error *>
  BEGIN
    TRY
      EVAL EventSync.Wait(self.fv, "abort=0");
      Thread.Alert(self.clientThread);
    EXCEPT Thread.Alerted =>
      (* Just exit. *)
    END;
    RETURN NIL;
  END AborterApply;

(*****************************************************************************)
(* Lister Statistics Display. *)
(*****************************************************************************)

TYPE
  ListerStats = TreeList.Stats OBJECT
    fv: FormsVBT.T;
    ts: TotalStats;
  METHODS
    init(fv: FormsVBT.T; ts: TotalStats): ListerStats := LSInit;
  OVERRIDES
    update := LSUpdate;
  END;

PROCEDURE LSInit(self: ListerStats;
                 fv: FormsVBT.T;
		 ts: TotalStats): ListerStats =
  BEGIN
    EVAL TreeList.Stats.init(self);
    self.fv := fv;
    self.ts := ts;
    RETURN self;
  END LSInit;

PROCEDURE LSUpdate(self: ListerStats) =
  <* FATAL FormsVBT.Error *>
  BEGIN
    LOCK VBT.mu DO
      FormsVBT.PutText(self.fv, "listerCommOut",
	Fmt.LongReal(self.bytesOut / 1024.0d0, Fmt.Style.Fix, 0));
      FormsVBT.PutText(self.fv, "listerWireOut",
	Fmt.LongReal(self.wireBytesOut / 1024.0d0, Fmt.Style.Fix, 0));
    END;
    self.ts.update();
  END LSUpdate;

(*****************************************************************************)
(* Detailer Statistics Display. *)
(*****************************************************************************)

TYPE
  DetailerStats = Detailer.Stats OBJECT
    fv: FormsVBT.T;
    ts: TotalStats;
  METHODS
    init(fv: FormsVBT.T; ts: TotalStats): DetailerStats := DSInit;
  OVERRIDES
    update := DSUpdate;
  END;

PROCEDURE DSInit(self: DetailerStats;
                 fv: FormsVBT.T;
		 ts: TotalStats): DetailerStats =
  BEGIN
    EVAL Detailer.Stats.init(self);
    self.fv := fv;
    self.ts := ts;
    RETURN self;
  END DSInit;

PROCEDURE DSUpdate(self: DetailerStats) =
  <* FATAL FormsVBT.Error *>
  BEGIN
    LOCK VBT.mu DO
      FormsVBT.PutText(self.fv, "detailerCommIn",
	Fmt.LongReal(self.bytesIn / 1024.0d0, Fmt.Style.Fix, 0));
      FormsVBT.PutText(self.fv, "detailerWireIn",
	Fmt.LongReal(self.wireBytesIn / 1024.0d0, Fmt.Style.Fix, 0));
      FormsVBT.PutText(self.fv, "detailerCommOut",
	Fmt.LongReal(self.bytesOut / 1024.0d0, Fmt.Style.Fix, 0));
      FormsVBT.PutText(self.fv, "detailerWireOut",
	Fmt.LongReal(self.wireBytesOut / 1024.0d0, Fmt.Style.Fix, 0));
    END;
    self.ts.update();
  END DSUpdate;

(*****************************************************************************)
(* Updater Statistics Display. *)
(*****************************************************************************)

TYPE
  UpdaterStats = Updater.Stats OBJECT
    fv: FormsVBT.T;
    ts: TotalStats;
  METHODS
    init(fv: FormsVBT.T; ts: TotalStats): UpdaterStats := USInit;
  OVERRIDES
    start := USStart;
    update := USUpdate;
  END;

PROCEDURE USInit(self: UpdaterStats;
                 fv: FormsVBT.T;
		 ts: TotalStats): UpdaterStats =
  BEGIN
    EVAL Updater.Stats.init(self);
    self.fv := fv;
    self.ts := ts;
    RETURN self;
  END USInit;

PROCEDURE USStart(<*UNUSED*> self: UpdaterStats) =
  BEGIN
  END USStart;

PROCEDURE USUpdate(self: UpdaterStats; type: FileUpdater.UpdateType) =
  <* FATAL FormsVBT.Error *>
  VAR
    name: TEXT;
  BEGIN
    CASE type OF
    | FileUpdater.UpdateType.Edit =>     name := "edit";
    | FileUpdater.UpdateType.Checkout => name := "co";
    | FileUpdater.UpdateType.Rsync =>    name := "rsync";
    | FileUpdater.UpdateType.Append =>   name := "app";
    | FileUpdater.UpdateType.Touch =>    name := "touch";
    | FileUpdater.UpdateType.Create =>   name := "add";
    | FileUpdater.UpdateType.Delete =>   name := "del";
    | FileUpdater.UpdateType.Replace =>  name := "repl";
    | FileUpdater.UpdateType.Fixup =>    name := "fixup";
    | FileUpdater.UpdateType.Other =>    name := "other";
    ELSE
      name := NIL;
    END;

    LOCK VBT.mu DO
      IF name # NIL THEN
	WITH info = self.updateInfo[type] DO
	  FormsVBT.PutText(self.fv, name & "Count",
	    Fmt.Int(info.fileCount));
	  FormsVBT.PutText(self.fv, name & "WireKB",
	    Fmt.LongReal(info.wireBytes / 1024.0d0, Fmt.Style.Fix, 0));
	  FormsVBT.PutText(self.fv, name & "FileKB",
	    Fmt.LongReal(info.fileBytes / 1024.0d0, Fmt.Style.Fix, 0));
	  IF info.fileBytes # 0.0d0 THEN
	    FormsVBT.PutText(self.fv, name & "Compr",
	      Fmt.LongReal(100.0d0 *
		(info.fileBytes - info.wireBytes) / info.fileBytes,
		style := Fmt.Style.Fix, prec := 1));
	  END;
	END;
      END;
      WITH info = self.totals DO
	FormsVBT.PutText(self.fv, "totCount",
	  Fmt.Int(info.fileCount));
	FormsVBT.PutText(self.fv, "totWireKB",
	  Fmt.LongReal(info.wireBytes / 1024.0d0, Fmt.Style.Fix, 0));
	FormsVBT.PutText(self.fv, "totFileKB",
	  Fmt.LongReal(info.fileBytes / 1024.0d0, Fmt.Style.Fix, 0));
	IF info.fileBytes # 0.0d0 THEN
	  FormsVBT.PutText(self.fv, "totCompr",
	    Fmt.LongReal(100.0d0 *
	      (info.fileBytes - info.wireBytes) / info.fileBytes,
	      style := Fmt.Style.Fix, prec := 1));
	END;

	FormsVBT.PutText(self.fv, "updaterCommIn",
	  Fmt.LongReal(info.commBytes / 1024.0d0, Fmt.Style.Fix, 0));
	FormsVBT.PutText(self.fv, "updaterWireIn",
	  Fmt.LongReal(info.wireBytes / 1024.0d0, Fmt.Style.Fix, 0));
      END;
    END;
    self.ts.update();
  END USUpdate;

(*****************************************************************************)
(* Total Statistics Display. *)
(*****************************************************************************)

TYPE
  TotalStats = MUTEX OBJECT
    fv: FormsVBT.T;
    ls: ListerStats := NIL;
    ds: DetailerStats := NIL;
    us: UpdaterStats := NIL;
  METHODS
    init(fv: FormsVBT.T): TotalStats := TSInit;
    update() := TSUpdate;
  END;

PROCEDURE TSInit(self: TotalStats;
                 fv: FormsVBT.T): TotalStats =
  BEGIN
    self.fv := fv;
    (* The other fields have to be filled in manually. *)
    RETURN self;
  END TSInit;

PROCEDURE TSUpdate(self: TotalStats) =
  <* FATAL FormsVBT.Error *>
  VAR
    commIn, wireIn, commOut, wireOut := 0.0d0;
    listTot, detTot, updTot, wireTot: LONGREAL;
  BEGIN
    LOCK self.ls DO
      commOut := commOut + self.ls.bytesOut;
      wireOut := wireOut + self.ls.wireBytesOut;
      listTot := self.ls.wireBytesOut;
    END;
    LOCK self.ds DO
      commIn := commIn + self.ds.bytesIn;
      wireIn := wireIn + self.ds.wireBytesIn;
      commOut := commOut + self.ds.bytesOut;
      wireOut := wireOut + self.ds.wireBytesOut;
      detTot := self.ds.wireBytesIn + self.ds.wireBytesOut;
    END;
    LOCK self.us DO
      commIn := commIn + self.us.totals.commBytes;
      wireIn := wireIn + self.us.totals.wireBytes;
      updTot := self.us.totals.wireBytes;
    END;
    wireTot := listTot + detTot + updTot;

    LOCK VBT.mu DO
      FormsVBT.PutText(self.fv, "listerWireTot",
	Fmt.LongReal(listTot / 1024.0d0, Fmt.Style.Fix, 0));
      FormsVBT.PutText(self.fv, "detailerWireTot",
	Fmt.LongReal(detTot / 1024.0d0, Fmt.Style.Fix, 0));
      FormsVBT.PutText(self.fv, "updaterWireTot",
	Fmt.LongReal(updTot / 1024.0d0, Fmt.Style.Fix, 0));

      FormsVBT.PutText(self.fv, "totCommIn",
	Fmt.LongReal(commIn / 1024.0d0, Fmt.Style.Fix, 0));
      FormsVBT.PutText(self.fv, "totWireIn",
	Fmt.LongReal(wireIn / 1024.0d0, Fmt.Style.Fix, 0));
      FormsVBT.PutText(self.fv, "totCommOut",
	Fmt.LongReal(commOut / 1024.0d0, Fmt.Style.Fix, 0));
      FormsVBT.PutText(self.fv, "totWireOut",
	Fmt.LongReal(wireOut / 1024.0d0, Fmt.Style.Fix, 0));

      FormsVBT.PutText(self.fv, "totWireTot",
	Fmt.LongReal(wireTot / 1024.0d0, Fmt.Style.Fix, 0));
    END;
  END TSUpdate;

(*****************************************************************************)
(* Clock. *)
(*****************************************************************************)

TYPE
  Clock = AutoRepeat.T OBJECT
    fv: FormsVBT.T;
    name: TEXT;
    updaterStats: UpdaterStats;
    started := FALSE;
    startTime: Time.T;
    seconds: CARDINAL;
  METHODS
    init(fv: FormsVBT.T; name: TEXT; updaterStats: UpdaterStats): Clock
      := ClockInit;
  OVERRIDES
    repeat := ClockTick;
  END;

PROCEDURE ClockInit(self: Clock;
                    fv: FormsVBT.T;
		    name: TEXT;
		    updaterStats: UpdaterStats): Clock =
  BEGIN
    EVAL NARROW(self, AutoRepeat.T).init(0, 500);
    self.fv := fv;
    self.name := name;
    self.updaterStats := updaterStats;
    RETURN self;
  END ClockInit;

PROCEDURE ClockTick(self: Clock) =
  <* FATAL FormsVBT.Error *>
  VAR
    elapsed: Time.T;
    hours, minutes, seconds: CARDINAL;
    clockText: TEXT;
    fileBytes: LONGREAL;
    xferRate: LONGREAL;
    xferRateText: TEXT;
  BEGIN
    IF NOT self.started THEN
      self.startTime := Time.Now();
      self.seconds := 0;
      self.started := TRUE;
      RETURN;
    END;

    elapsed := Time.Now() - self.startTime;
    seconds := FLOOR(elapsed);
    IF seconds # self.seconds THEN
      self.seconds := seconds;

      hours := seconds DIV 3600;
      seconds := seconds MOD 3600;
      minutes := seconds DIV 60;
      seconds := seconds MOD 60;
      clockText := Fmt.Pad(Fmt.Int(hours), 2, '0') & ":" &
	Fmt.Pad(Fmt.Int(minutes), 2, '0') & ":" &
	Fmt.Pad(Fmt.Int(seconds), 2, '0');

      LOCK self.updaterStats DO
	fileBytes := self.updaterStats.totals.fileBytes;
      END;

      xferRate := fileBytes / 1024.0d0 / elapsed;
      xferRateText := Fmt.Pad(Fmt.LongReal(xferRate, Fmt.Style.Fix, 1), 7) &
	" KB/sec";

      LOCK VBT.mu DO
	FormsVBT.PutText(self.fv, self.name, clockText);
	FormsVBT.PutText(self.fv, "xferRate", xferRateText);
      END;
    END;
  END ClockTick;

(*****************************************************************************)

PROCEDURE Run(supFile: Pathname.T;
              config: FSClient.Configuration)
  RAISES {Error, Thread.Alerted} =
CONST
  FormFile = "SupGUI.fv";
  PathVar = "$CVSupPath";
VAR
  fv: FormsVBT.T;
  controller: Controller;
BEGIN
  (* If the client host's hostname is set up wrong, such that it is
     impossible to find out its IP address, then we will get an
     "IP.FatalError" exception from the X11 initialization code.  Find
     out about it now, so that we can produce an intelligent error
     message rather than a core dump. *)
  VAR
    ok := FALSE;
    addr: IP.Address;
  BEGIN
    TRY
      ok := IP.GetHostByName(UnixMisc.GetHostName(), addr);
    EXCEPT ELSE END;
    IF NOT ok THEN
      RAISE Error("Cannot get IP address of my own host"
	& " -- is its hostname correct?");
    END;
  END;

  TRY
    TRY
      WITH path = Rsrc.BuildPath(PathVar, SupGUIBundle.Get()) DO
	fv := NEW(FormsVBT.T).initFromRsrc(FormFile, path);
      END;
    EXCEPT
    | Rd.Failure(list) =>
      RAISE Error("Cannot read \"" & FormFile & "\": " &
	ErrMsg.StrError(list));
    | Rsrc.NotFound =>
      RAISE Error("Cannot find the \"" & FormFile & "\" resource");
    END;

    LOCK VBT.mu DO
      FormsVBT.PutText(fv, "clientVersion", Version.Name);
      FormsVBT.PutText(fv, "protoVersion",
	Fmt.Int(CVProto.Current.major) & "." &
	Fmt.Int(CVProto.Current.minor));
      FormsVBT.PutText(fv, "target", Version.Target);
    END;

    controller := NEW(Controller).init(
      fv := fv,
      supFile := supFile,
      config := config);
    Trestle.Install(fv);
    EVAL Thread.Fork(controller);
    Trestle.AwaitDelete(fv);
  EXCEPT
  | FormsVBT.Error(msg) =>
    RAISE Error("FormsVBT error: " & msg);
  | TrestleComm.Failure =>
    RAISE Error("Lost the connection to the X server");
  END;
END Run;

BEGIN
  Supported := TRUE;
END SupGUI.
