(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Tue Aug 27 16:13:37 PDT 1996 by steveg *)

MODULE App;

<* PRAGMA LL *>

IMPORT
  Atom, Env, FileRd, Fmt, IP, Lex, OSError, Params,
  Rd, Stdio, Text, TextRd, TextWr, Thread, Wr;

VAR
  readWriteMu := NEW(MUTEX);
  readWriteCV := NEW(Thread.Condition);
  readingCnt := 0;
  writingCnt := 0;
  (* single writer, multiple reader algorithm.

     if a thread is writing, then writingCnt # 0.
     if a thread is reading, then readingCnt # 0.

     a thread can read if another thread is reading.
     a thread cannot read if another thread is writing.
     a thread can write if no thread is reading or writing.

     readingCnt and writingCnt are protected by readWriteMu.
  *)

  hostName, hostIP: TEXT;

PROCEDURE ReadLock() =
  BEGIN
    LOCK readWriteMu DO
      WHILE writingCnt # 0 DO
        Thread.Wait(readWriteMu, readWriteCV);
      END;

      INC(readingCnt);
    END;
  END ReadLock;

PROCEDURE ReadUnlock() =
  BEGIN
    LOCK readWriteMu DO
      DEC(readingCnt);
    END;
  END ReadUnlock;

PROCEDURE WriteLock() =
  BEGIN
    LOCK readWriteMu DO
      WHILE readingCnt # 0 OR writingCnt # 0 DO
        Thread.Wait(readWriteMu, readWriteCV);
      END;

      INC(writingCnt);
    END;
  END WriteLock;

PROCEDURE WriteUnlock() =
  BEGIN
    LOCK readWriteMu DO
      DEC(writingCnt);
    END;
  END WriteUnlock;

REVEAL
  Log = LogPublic BRANDED OBJECT
  OVERRIDES
    log := LogMsg;
  END;

PROCEDURE LogMsg(<* UNUSED *> self: Log; 
                 msg: TEXT; status: LogStatus) RAISES {Error} =
  BEGIN
    IF status = LogStatus.Error THEN
      RAISE Error(msg);
    END;
  END LogMsg;

PROCEDURE GetHostName (ipAddr: BOOLEAN := FALSE): TEXT =
  VAR addr: IP.Address;
  <* FATAL IP.Error *>
  BEGIN
    IF hostName = NIL THEN
      addr := IP.GetHostAddr();
      hostIP :=
        Fmt.F("%s.%s.%s.%s", Fmt.Int(addr.a[0]), Fmt.Int(addr.a[1]),
              Fmt.Int(addr.a[1]), Fmt.Int(addr.a[1]));
      hostName := IP.GetCanonicalByAddr(addr);
    END;
    IF ipAddr THEN
      RETURN hostIP;
    ELSE
      RETURN hostName;
    END;
  END GetHostName;

TYPE
  Arg = {Debug, NoDebug, Verbose, NoVerbose};

  AppArgHandler = ArgHandler OBJECT
  OVERRIDES
    set := SetArg;
  END;

VAR
  argMu := NEW(MUTEX);
  debug := FALSE; <* LL = argMu *>
  verbose := FALSE; <* LL = argMu *>
  noDebug := TRUE; <* LL = argMu *>
  noVerbose := TRUE; <* LL = argMu *>

PROCEDURE SetArg(self: ArgHandler; <* UNUSED *> src: ArgSource; 
                 value: TEXT; <* UNUSED *> log: Log) =
  BEGIN
    LOCK argMu DO
      CASE VAL(self.id, Arg) OF
      | Arg.Debug => debug := Text.Equal(value, "TRUE");
      | Arg.Verbose => verbose := Text.Equal(value, "TRUE");
      | Arg.NoDebug => noDebug := Text.Equal(value, "TRUE");
      | Arg.NoVerbose => noVerbose := Text.Equal(value, "TRUE");
      END;
    END;
  END SetArg;

PROCEDURE Debug(): BOOLEAN =
  BEGIN
    LOCK argMu DO
      RETURN debug AND NOT noDebug;
    END;
  END Debug;

PROCEDURE Verbose(): BOOLEAN =
  BEGIN
    LOCK argMu DO
      RETURN verbose AND NOT noVerbose;
    END;
  END Verbose;

PROCEDURE SetValue(value: Value; f: BOOLEAN) =
  BEGIN
    LOCK argMu DO
      CASE value OF
      | Value.Debug => debug := f;
      | Value.NoDebug => noDebug := f;
      | Value.Verbose => verbose := f;
      | Value.NoVerbose => noVerbose := f;
      END;
    END;
  END SetValue;

TYPE
  DefaultLog = Log OBJECT
  OVERRIDES
    log := DefaultLogMsg;
  END;

PROCEDURE DefaultLogMsg(self: Log; msg: TEXT; 
                      status: LogStatus) RAISES {Error} =
  <* FATAL Wr.Failure, Thread.Alerted *>
  VAR
    wr: Wr.T;
  BEGIN
    IF status IN SET OF LogStatus{LogStatus.Verbose..LogStatus.Status} THEN
      wr := Stdio.stdout;
    ELSE
      wr := Stdio.stderr;
    END;
    Wr.PutText(wr, Fmt.F("%s: %s\n", LogStatusText[status], msg));
    Wr.Flush(wr);
    Log.log(self, msg, status);
  END DefaultLogMsg;

TYPE
  NullLog = Log OBJECT
  END;

REVEAL
  ArgHandler = ArgHandlerPublic BRANDED OBJECT
    atoms: ARRAY ArgSource OF Atom.T;
    src := ArgSource.None;
  OVERRIDES
    init := InitArgHandler;
    set := DefaultSetArg;
  END;

PROCEDURE MakeAtom(txt: TEXT): Atom.T =
  BEGIN
    IF txt = NIL THEN RETURN NIL ELSE RETURN Atom.FromText(txt) END;
  END MakeAtom;

PROCEDURE InitArgHandler(self: ArgHandler;
                         switchName, envName, configName: TEXT;
                         register := TRUE): ArgHandler =
  BEGIN
    self.atoms[ArgSource.Switch] := MakeAtom(switchName);
    self.atoms[ArgSource.Env] := MakeAtom(envName);
    IF configName = NIL THEN
      self.atoms[ArgSource.Config] := self.atoms[ArgSource.Switch];
    ELSE
      self.atoms[ArgSource.Config] := MakeAtom(configName);
    END;
    IF register THEN
      RegisterArgHandler(self);
    END;
    RETURN self;
  END InitArgHandler;

PROCEDURE DefaultSetArg(<* UNUSED *> self: ArgHandler;
                        <* UNUSED *> src: ArgSource;
                        <* UNUSED *> value: TEXT;
                        <* UNUSED *> log: Log) =
  BEGIN
    <* ASSERT FALSE *>
  END DefaultSetArg;

TYPE
  ArgHandlerList = REF RECORD
    head: ArgHandler;
    tail: ArgHandlerList;
  END;

VAR
  argHandlerList: ArgHandlerList := NIL;

PROCEDURE RegisterArgHandler(handler: ArgHandler) =
  BEGIN
    WriteLock();
    TRY
      argHandlerList := NEW(ArgHandlerList, head := handler, 
                            tail := argHandlerList);
    FINALLY
      WriteUnlock();
    END;
  END RegisterArgHandler;

EXCEPTION
  ConfigError;

CONST
  DefaultConfigFile = ".app_config";
  ConfigSwitch = "-config";
  ConfigEnv = "HTTP_CONFIG";

VAR
  defaultConfigFile: TEXT;

PROCEDURE SwitchError(log: Log) RAISES {Error} =
  VAR
    list: ArgHandlerList;
    wr := TextWr.New();
    anyArg := FALSE;
  BEGIN
    TRY
      Wr.PutText(wr, "Options: ");
      ReadLock();
      TRY
        list := argHandlerList;
        WHILE list # NIL DO
          WITH at = list.head.atoms[ArgSource.Switch] DO
            IF at # NIL THEN
              Wr.PutText(wr, Fmt.F("-%s ", Atom.ToText(at)));
              IF list.head.hasParam THEN
                Wr.PutText(wr, Fmt.F("<%s> ", list.head.paramName));
              END;
            ELSE
              anyArg := TRUE;
            END;
          END;
          list := list.tail;
        END;
      FINALLY
        ReadUnlock();
      END;
      IF anyArg THEN
        Wr.PutText(wr, "argument(s)...");
      END;    
    EXCEPT
    | Wr.Failure, Thread.Alerted =>
    END;
    log.log(TextWr.ToText(wr), LogStatus.Error);
  END SwitchError;

PROCEDURE MatchArgHandler(src: ArgSource; name: Atom.T): ArgHandler =
  VAR
    list: ArgHandlerList;
  BEGIN
    ReadLock();
    TRY
      list := argHandlerList;
      WHILE list # NIL DO
        IF name = list.head.atoms[src] THEN
          RETURN list.head;
        END;
        list := list.tail;
      END;
    FINALLY
      ReadUnlock();
    END;
    RETURN NIL;
  END MatchArgHandler;

PROCEDURE ParseSwitches (log: Log; logConfiguration: BOOLEAN)
  RAISES {Error} =
  VAR
    i         : INTEGER;
    arg, value: TEXT;
    handler   : ArgHandler;
    anyArg                 := FALSE;
  BEGIN
    i := 1;
    WHILE i < Params.Count DO
      arg := Params.Get(i);
      IF Text.GetChar(arg, 0) # '-' THEN
        handler := MatchArgHandler(ArgSource.Switch, AnyArgument);
        anyArg := TRUE;
      ELSE
        handler := MatchArgHandler(
                     ArgSource.Switch, Atom.FromText(Text.Sub(arg, 1)));
      END;
      IF handler = NIL THEN
        IF NOT Text.Equal(arg, ConfigSwitch) THEN SwitchError(log) END;
        IF i + 1 = Params.Count THEN SwitchError(log) END;
        value := Params.Get(i + 1);
        INC(i);
      ELSE
        IF anyArg THEN
          value := arg;
        ELSIF handler.src
                IN SET OF ArgSource{ArgSource.Switch, ArgSource.None} THEN
          IF handler.hasParam THEN
            IF i + 1 = Params.Count THEN SwitchError(log) END;
            value := Params.Get(i + 1);
            INC(i);
          ELSE
            value := "TRUE";
          END;
        END;
        handler.src := ArgSource.Switch;
        IF logConfiguration THEN
          log.log(
            Fmt.F("program switch: %s: %s", arg, value), LogStatus.Verbose);
        END;
        handler.set(ArgSource.Switch, value, log);
      END;
      INC(i);
    END;
  END ParseSwitches;

PROCEDURE ParseEnv (log: Log; logConfiguration: BOOLEAN) RAISES {Error} =
  VAR
    list : ArgHandlerList;
    value: TEXT;
  BEGIN
    ReadLock();
    TRY
      list := argHandlerList;
      WHILE list # NIL DO
        IF list.head.src = ArgSource.None THEN
          WITH at = list.head.atoms[ArgSource.Env] DO
            IF at # NIL THEN
              value := Env.Get(Atom.ToText(at));
              IF value # NIL THEN
                list.head.src := ArgSource.Env;
                IF logConfiguration THEN
                  log.log(Fmt.F("environment switch: %s: %s",
                                Atom.ToText(at), value), LogStatus.Verbose);
                END;
                list.head.set(ArgSource.Env, value, log);
              END;
            END;
          END;
        END;
        list := list.tail;
      END;
    FINALLY
      ReadUnlock();
    END;
  END ParseEnv;

CONST 
  NonColon = SET OF CHAR{'\000'..'\377'} - SET OF CHAR{':'};

PROCEDURE ParseConfig (configFile      : TEXT;
                       log             : Log;
                       logConfiguration: BOOLEAN) RAISES {Error} =
  VAR
    rd                : FileRd.T;
    trd               : TextRd.T;
    line, field, value: TEXT;
    handler           : ArgHandler;
  BEGIN
    TRY
      IF logConfiguration THEN
        log.log(Fmt.F("config file: %s", configFile), LogStatus.Verbose);
      END;
      rd := FileRd.Open(configFile);
      LOOP
        line := Rd.GetLine(rd);
        trd := TextRd.New(line);
        field := Lex.Scan(trd, NonColon);
        IF Rd.EOF(trd) OR Rd.GetChar(trd) = ':' THEN
          handler :=
            MatchArgHandler(ArgSource.Config, Atom.FromText(field));
          IF handler = NIL THEN
            RAISE ConfigError;
          ELSIF handler.src
                  IN SET OF ArgSource{ArgSource.None, ArgSource.Config} THEN
            handler.src := ArgSource.Config;
            Lex.Skip(trd);
            IF handler.hasParam THEN
              IF Rd.EOF(trd) THEN RAISE ConfigError END;
              value := Rd.GetLine(trd);
            ELSE
              value := "TRUE";
            END;
            IF logConfiguration THEN
              log.log(Fmt.F("config file switch: %s: %s", field, value),
                      LogStatus.Verbose);
            END;
            handler.set(ArgSource.Config, value, log);
            IF NOT Rd.EOF(trd) THEN RAISE ConfigError END;
          END;
        ELSE
          RAISE ConfigError;
        END;
      END;
    EXCEPT
    | Rd.EndOfFile => Rd.Close(rd); <* NOWARN *>
    | OSError.E =>
        IF configFile # defaultConfigFile THEN
          log.log(Fmt.F("Can't open config file %s", configFile),
                  LogStatus.Error);
        END;
    | Rd.Failure, Thread.Alerted =>
        log.log(Fmt.F("Problems reading config file %s", configFile),
                LogStatus.Error);
    | ConfigError =>
        log.log(Fmt.F("Bad entry in configFile %s: %s", configFile, line),
                LogStatus.Error);
    END;
  END ParseConfig;

PROCEDURE ArgDefaults (log: Log; logConfiguration: BOOLEAN) RAISES {Error} =
  VAR list: ArgHandlerList;
  BEGIN
    ReadLock();
    TRY
      list := argHandlerList;
      WHILE list # NIL DO
        IF list.head.src = ArgSource.None THEN
          list.head.src := ArgSource.Default;
          IF list.head.atoms[ArgSource.Switch] = NIL
               OR list.head.default = NIL THEN
            log.log("Bad Default arg", LogStatus.Verbose);
            log.log(
              Fmt.F("id = %s", Fmt.Int(list.head.id)), LogStatus.Verbose);
            IF list.head.paramName = NIL THEN
              log.log("NIL paramName", LogStatus.Verbose);
            ELSE
              log.log(Fmt.F("paramName = %s", list.head.paramName),
                      LogStatus.Verbose);
            END;
            IF list.head.default = NIL THEN
              log.log("NIL default", LogStatus.Verbose);
            ELSE
              log.log(Fmt.F("default = %s", list.head.default),
                      LogStatus.Verbose);
            END;
            IF list.head.atoms[ArgSource.Switch] = NIL THEN
              log.log("NIL switch atom", LogStatus.Verbose);
            ELSE
              log.log(
                Fmt.F("switch = %s",
                      Atom.ToText(list.head.atoms[ArgSource.Switch])),
                LogStatus.Verbose);
            END;
            IF list.head.atoms[ArgSource.Env] = NIL THEN
              log.log("NIL env atom", LogStatus.Verbose);
            ELSE
              log.log(Fmt.F("env = %s",
                            Atom.ToText(list.head.atoms[ArgSource.Env])),
                      LogStatus.Verbose);
            END;
            IF list.head.atoms[ArgSource.Config] = NIL THEN
              log.log("NIL config atom", LogStatus.Verbose);
            ELSE
              log.log(
                Fmt.F("config = %s",
                      Atom.ToText(list.head.atoms[ArgSource.Config])),
                LogStatus.Verbose);
            END;
          ELSIF logConfiguration THEN
            log.log(Fmt.F("Default arg %s: %s",
                          Atom.ToText(list.head.atoms[ArgSource.Switch]),
                          list.head.default), LogStatus.Verbose);
          END;
          list.head.set(ArgSource.Default, list.head.default, log);
        END;
        list := list.tail;
      END;
    FINALLY
      ReadUnlock();
    END;
  END ArgDefaults;

PROCEDURE InitializeArguments(log: Log; 
                              configFile: TEXT; 
                              logConfiguration: BOOLEAN) RAISES {Error} =
  VAR
    i: INTEGER;
    arg: TEXT;
  BEGIN
    IF log = NIL THEN log := defaultLog END;
    IF configFile = NIL THEN configFile := DefaultConfigFile; END;
    defaultConfigFile := configFile;

    IF Env.Get(ConfigEnv) # NIL THEN configFile := Env.Get(ConfigEnv) END;

    i := 1;
    WHILE i < Params.Count DO
      arg := Params.Get(i);
      IF Text.Equal(arg, ConfigSwitch) THEN
        IF i + 1 = Params.Count THEN 
          log.log("No parameter for \"-config\" switch", LogStatus.Status);
          SwitchError(log);
        END;
        configFile := Params.Get(i + 1);
        EXIT;
      END;
      INC(i);
    END; 

    ParseSwitches(log, logConfiguration);
    ParseEnv(log, logConfiguration);
    ParseConfig(configFile, log, logConfiguration);
    ArgDefaults(log, logConfiguration);
  END InitializeArguments;


BEGIN
  EVAL NEW(AppArgHandler, id := ORD(Arg.Debug), hasParam:= FALSE).init(
                                     switchName := "debug", 
                                     envName := "APP_DEBUG");
  EVAL NEW(AppArgHandler, id := ORD(Arg.NoDebug), hasParam:= FALSE).init(
                                     switchName := "noDebug", 
                                     envName := "APP_NODEBUG");
  EVAL NEW(AppArgHandler, id := ORD(Arg.Verbose), hasParam:= FALSE).init(
                                     switchName := "verbose", 
                                     envName := "APP_VERBOSE");
  EVAL NEW(AppArgHandler, id := ORD(Arg.NoVerbose), hasParam:= FALSE).init(
                                     switchName := "noVerbose", 
                                     envName := "APP_NOVERBOSE");

  defaultLog := NEW(DefaultLog);
  nullLog := NEW(NullLog);

END App.
