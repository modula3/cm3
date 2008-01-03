MODULE Echo EXPORTS Main;

IMPORT ObjectSpace, TrackerPosition, TrackerPositionProxy, SharedObjRT,
       Process, NetObj, IO, IP, Params, Fmt, Thread, Text, AtomList,
       Atom, SharedObj, TrackerPositionCB, Time, Scan;

PROCEDURE PrintAtomList (t: Text.T; al: AtomList.T) =
  BEGIN
    IO.Put(t & ":\n");
    WHILE al # NIL DO
      IO.Put("  " & Atom.ToText(al.head) & "\n");
      al := al.tail;
    END;
  END PrintAtomList;

TYPE Action = {Sequencer, Producer, Consumer};

TYPE
  Proxy = TrackerPositionProxy.T OBJECT 
    pos: TrackerPosition.T;
  OVERRIDES
    get := Get;
    init := Init;
  END;

TYPE 
  MyCB = TrackerPositionCB.T OBJECT 
      suspend: BOOLEAN;
    OVERRIDES
      post_set := SetCB;
    END;

PROCEDURE SetCB (self: MyCB; 
                 <*UNUSED*> READONLY obj: TrackerPosition.T; 
                 READONLY val: TrackerPosition.Data): BOOLEAN =
  BEGIN
    IF self.suspend # val.suspend THEN
      self.suspend := val.suspend;
      IO.Put("Suspend set to => " & Fmt.Bool(val.suspend) & "\n");

      (* A TRUE return value indicates that we do not need to be
         notified via "anyChange()" *)
    END;
    RETURN TRUE;
  END SetCB;


PROCEDURE Get(self: Proxy): TrackerPosition.T =
  BEGIN
    RETURN self.pos;
  END Get;

PROCEDURE Init(self: Proxy; t: TrackerPosition.T): TrackerPositionProxy.T =
  BEGIN
    self.pos := t;
    RETURN self;
  END Init;

VAR
  sequencer: TEXT          := NIL;
  producer : TEXT          := NIL;
  action   : Action        := Action.Sequencer;
  seqSpace : ObjectSpace.T;

  trackerPos  : TrackerPosition.T;
  trackerProxy: TrackerPositionProxy.T;
  data        : TrackerPosition.Data;

  count       : INTEGER;

  cb: MyCB;

  time: Time.T;

  debug_level : INTEGER := 0;

PROCEDURE Usage (arg: TEXT) =
  VAR prog := Params.Get(0);
  BEGIN
    IO.Put("Invalid argument: " & arg & "\n");
    IO.Put(Fmt.F("Usage:\n\t%s -s\n\t%s -p sequencer\n"
                   & "\t%s -c producer sequencer", prog, prog, prog));
    Process.Exit(1);
  END Usage;

BEGIN
  VAR i := 1;
  BEGIN
    WHILE i < Params.Count DO
      WITH arg = Params.Get(i) DO
        IF Text.GetChar(arg, 0) = '-' THEN
          CASE Text.GetChar(arg, 1) OF
          | 's' => action := Action.Sequencer; INC(i); EXIT;
          | 'p' =>
              action := Action.Producer;
              INC(i);
              sequencer := Params.Get(i);
              INC(i);
          | 'c' =>
              action := Action.Consumer;
              INC(i);
              producer := Params.Get(i);
              INC(i);
              sequencer := Params.Get(i);
              INC(i);
          | 'd' =>
              INC(i);
              debug_level := Scan.Int(Params.Get(i));
          ELSE
            Usage(arg);
          END;
        ELSE
          Usage(arg);
        END;
      END;
      INC(i);
    END;
    IF i < Params.Count THEN Usage(Params.Get(i)); END;
  END;

  SharedObjRT.DebugLevel(debug_level);

  TRY
    CASE action OF
    | Action.Sequencer =>
        WITH name = IP.GetCanonicalByAddr(IP.GetHostAddr()) DO
          IO.Put("Exporting space as " & name & "\n");
          SharedObjRT.ExportSpace(name);
        END;
        WITH space = SharedObjRT.LocalSpace() DO
          space.setDfltSequencer(space);
        END;
        LOOP Thread.Pause(1.0D0) END;

    | Action.Producer =>
        WITH name = IP.GetCanonicalByAddr(IP.GetHostAddr()) DO
          IO.Put("Exporting space as " & name & "\n");
          SharedObjRT.ExportSpace(name);
        END;

        LOOP
          WITH name = IP.GetCanonicalByName(sequencer) DO
            IO.Put("Importing sequencer " & name & "\n");
            seqSpace := SharedObjRT.ImportSpace(name, name);
          END;
          IF seqSpace # NIL THEN EXIT END;
          IO.Put("Sequencer not ready.  Retrying ...\n");
          Thread.Pause(1.0D0);
        END;
        SharedObjRT.LocalSpace().setDfltSequencer(seqSpace);
        IO.Put("Sequencer set.\n");

        trackerPos := NEW(TrackerPosition.T).init();
        IO.Put("trackerPos initialized.\n");
        trackerProxy := NEW(Proxy).init(trackerPos);
        IO.Put("trackerProxy initialized.\n");

        IO.Put("Exporting proxy via netobjd\n");
        LOOP
          TRY
            NetObj.Export("Proxy", trackerProxy, NIL);
            EXIT;
          EXCEPT
          | NetObj.Error =>
          END;
          IO.Put("Failed to exported proxy via netobjd.  Retrying ...\n");
          Thread.Pause(1.0D0);
        END;
        IO.Put("Exported proxy via netobjd\n");

        count := 1;
        LOOP
          data.position.x := FLOAT(count) / 30.0;
          IF count MOD 30 = 0 THEN
            data.suspend := NOT data.suspend;
            IO.Put("Suspend toggled => " & Fmt.Bool(data.suspend) & " in " &
              Fmt.LongReal(time, prec := 6) & " seconds\n");
          END;
          time := Time.Now();
          trackerPos.set(data);
          time := Time.Now() - time;
          Thread.Pause(0.033D0);
          INC(count);
        END;

    | Action.Consumer =>
        WITH name = IP.GetCanonicalByAddr(IP.GetHostAddr()) DO
          IO.Put("Exporting space as " & name & "\n");
          SharedObjRT.ExportSpace(name);
        END;

        LOOP
          WITH name = IP.GetCanonicalByName(sequencer) DO
            IO.Put("Importing sequencer " & name & "\n");
            seqSpace := SharedObjRT.ImportSpace(name, name);
          END;
          IF seqSpace # NIL THEN EXIT END;
          IO.Put("Sequencer not ready.  Retrying ...\n");
          Thread.Pause(1.0D0);
        END;
        SharedObjRT.LocalSpace().setDfltSequencer(seqSpace);

        VAR addr: IP.Address;
        BEGIN
          IF NOT IP.GetHostByName(producer, addr) THEN
            Process.Crash("No such host \"" & producer & "\"\n");
          END;
        END;
        VAR agent: NetObj.Address;
        BEGIN
          LOOP
            TRY
              agent := NetObj.Locate(producer);
              IO.Put("Located netobjd on host \"" & producer & "\"\n");
              EXIT;
            EXCEPT
            | NetObj.Error =>
            END;
            Thread.Pause(1.0D0);
          END;

          LOOP
            TRY
              trackerProxy := NetObj.Import("Proxy", agent);
              IF trackerProxy # NIL THEN
                IO.Put(Fmt.F("Located target on host \"%s\" via netobjd\n",
                             producer));
                EXIT;
              END;
              Thread.Pause(1.0D0);
            EXCEPT
            | NetObj.Error =>
            END;
          END;
        END;

        IO.Put("Getting trackerPos\n");
        trackerPos := trackerProxy.get();

        IO.Put("Received trackerPos.  Getting data.\n");
        trackerPos.get(data);
        IO.Put("Received data.\n");

        cb := NEW(MyCB, suspend := data.suspend).init(trackerPos);

        LOOP Thread.Pause(1.0D0) END;
    END;
  EXCEPT
  | SharedObj.Error (el) => PrintAtomList("SharedObj.Error", el);
  | IP.Error (el) => PrintAtomList("IP.Error", el);
  | NetObj.Error (el) => PrintAtomList("NetObj.Error", el);
  | Thread.Alerted => IO.Put("Thread.Alerted\n");
  | NetObj.Invalid => IO.Put("NetObj.Invalid\n");
  END;
END Echo.
