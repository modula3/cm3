(*--------------------------------------------------------------------------*)
MODULE CompactRC;

IMPORT Rd, Text, TextTextTbl, TextRd, Thread, Pathname, Env, Bundle, Rsrc,
       FileRd, OSError, Params, RefList, TextSeq, TextList, Fmt;
(* IMPORT Fmt; *)
IMPORT SMsg AS Msg, MsgX, MsgIF, TextUtils, TextReadingUtils, FSUtils,
       ProcessEnv, CompactEnvName, MiniEnv, SimpleValueEnv,
       CompactClient, System;
IMPORT PathRepr, TextTuple, TextTupleSeq, TextTextTupleSeqTbl, RegEx;

(*--------------------------------------------------------------------------*)
PROCEDURE TpcOverridesInit(self : TpcOverrides) : TpcOverrides =
  BEGIN
    self.hosttype := NIL;
    self.ostype := NIL;
    self.variant := NIL;
    self.compiler := NIL;
    self.options := NIL;
    RETURN self;
  END TpcOverridesInit;

(*--------------------------------------------------------------------------*)
PROCEDURE MappingDefined(name : TEXT) : BOOLEAN =
  VAR map : TextTupleSeq.T;
  BEGIN
    RETURN mappingLists.get(name, map);
  END MappingDefined;

(*--------------------------------------------------------------------------*)
PROCEDURE GetMapping(name : TEXT) : TextTupleSeq.T =
  VAR map : TextTupleSeq.T;
  BEGIN
    IF mappingLists.get(name, map) THEN
      RETURN map;
    ELSE
      RETURN NIL;
    END;
  END GetMapping;

(*--------------------------------------------------------------------------*)
PROCEDURE SetSomeDefaults(env : TextTextTbl.T) =
  BEGIN
    EVAL env.put("HOME", PathRepr.Native(MiniEnv.home));
    EVAL env.put("USER", PathRepr.Native(MiniEnv.user));
    EVAL env.put("COMPACTROOT", PathRepr.Native(MiniEnv.compactroot));
    EVAL env.put("TMPDIR", PathRepr.Native(MiniEnv.tmpdir));
    EVAL env.put(CompactEnvName.Passphrase, MiniEnv.pass);
    EVAL env.put("editor", PathRepr.Native(MiniEnv.editor));
    EVAL env.put("editor-args-pathstyle", "native");
    EVAL env.put("httpd-editor", PathRepr.Native(MiniEnv.editor));
    (* editors defined *)
  END SetSomeDefaults;

(*--------------------------------------------------------------------------*)
PROCEDURE SetEnvVars(env : TextTextTbl.T) =
  BEGIN
    (* hack for external makedir *)
    IF Env.Get("PKG_USE_EXTERNAL_MKDIR") # NIL THEN
      EVAL env.put("useExternalMakedir", "TRUE");
    END;
    IF Env.Get("PKG_USE_POSIX_PATHNAMES") # NIL THEN
      EVAL env.put("usePosixPathnames", "TRUE");
    END;
    WITH val = Env.Get("PRJ_ROOT") DO
      IF val # NIL THEN
        EVAL env.put("PRJ_ROOT", PathRepr.Native(val));
      END;
    END;
    WITH val = Env.Get("COLLECTIONROOT") DO
      IF val # NIL THEN
        EVAL env.put("COLLECTIONROOT", PathRepr.Native(val));
      END;
    END;
    WITH val = Env.Get("PROJECTROOT") DO
      IF val # NIL THEN
        EVAL env.put("PROJECTROOT", PathRepr.Native(val));
      END;
    END;
  END SetEnvVars;

(*--------------------------------------------------------------------------*)
PROCEDURE SetTPCDefaults(env : TextTextTbl.T;
                         tpcOverrides : TpcOverrides := NIL;
                         tpcWarnings  : BOOLEAN := FALSE; 
                         msg : MsgIF.T := NIL;
                         warn := TRUE) =
  VAR
    val : TEXT;
  BEGIN
    (* set some defaults for TPC if not explicitly declared *)
    EVAL env.put("tpc-hosttype-default", MiniEnv.tpc_hosttype);
    IF tpcOverrides = NIL OR tpcOverrides.hosttype = NIL THEN
      IF NOT env.get("tpc-hosttype", val) AND
         NOT env.get("tpc-hosttype-start", val) THEN
        IF Text.Equal(MiniEnv.tpc_hosttype, "") AND 
          tpcWarnings AND warn THEN
          MsgX.Warning(msg, "tpc-hosttype is unknown");
        END;
      END;
    ELSE
      EVAL env.put("tpc-hosttype", tpcOverrides.hosttype);
      IF Msg.dFlag THEN
        MsgX.V(msg, "  tpc-override: tpc-hosttype=\"" & 
          tpcOverrides.hosttype & "\"");
      END;
    END;

    EVAL env.put("tpc-ostype-default", MiniEnv.tpc_ostype);
    IF tpcOverrides = NIL OR tpcOverrides.ostype = NIL THEN
      IF NOT env.get("tpc-ostype", val) AND
         NOT env.get("tpc-ostype-start", val) THEN
        IF Text.Equal(MiniEnv.tpc_ostype, "") AND 
          tpcWarnings AND warn  THEN
          MsgX.Warning(msg, "tpc-ostype is unknown");
        END;
      END;
    ELSE
      EVAL env.put("tpc-ostype", tpcOverrides.ostype);
      IF Msg.dFlag THEN
        MsgX.V(msg, "  tpc-override: tpc-ostype=\"" & 
          tpcOverrides.ostype & "\"");
      END;
    END;

    EVAL env.put("tpc-variant-default", MiniEnv.tpc_variant);
    IF tpcOverrides = NIL OR tpcOverrides.variant = NIL THEN
      IF NOT env.get("tpc-variant", val) AND
         NOT env.get("tpc-variant-start", val) THEN
        IF Text.Equal(MiniEnv.tpc_variant, "") AND 
          tpcWarnings AND warn THEN
          MsgX.Warning(msg, "tpc-variant is unknown");
        END;
      END;
    ELSE
      EVAL env.put("tpc-variant", tpcOverrides.variant);
      IF Msg.dFlag THEN
        MsgX.V(msg, "  tpc-override: tpc-variant=\"" & 
          tpcOverrides.variant & "\"");
      END;
    END;

    EVAL env.put("tpc-compiler-default", MiniEnv.tpc_compiler);
    IF tpcOverrides = NIL OR tpcOverrides.compiler = NIL THEN
      IF NOT env.get("tpc-compiler", val) AND 
         NOT env.get("tpc-compiler-start", val) THEN
        IF Text.Equal(MiniEnv.tpc_compiler, "") AND
          tpcWarnings AND warn THEN
          MsgX.Warning(msg, "tpc-compiler is unknown");
        END;
      END;
    ELSE
      EVAL env.put("tpc-compiler", tpcOverrides.compiler);
      IF Msg.dFlag THEN
        MsgX.V(msg, "  tpc-override: tpc-compiler=\"" & 
          tpcOverrides.compiler & "\"");
      END;
    END;

    IF tpcOverrides = NIL OR tpcOverrides.options = NIL THEN
      (* There is no sensible default for options, so we leave it undefined.
      IF NOT env.get("tpc-options", val) THEN
        EVAL env.put("tpc-options", MiniEnv.tpc_options);
      END;
      *)
    ELSE
      EVAL env.put("tpc-options", tpcOverrides.options);
      EVAL env.put("tpc-opts", Text.Sub(tpcOverrides.options, 1));
      IF Msg.dFlag THEN
        MsgX.V(msg, "  tpc-override: tpc-options=\"" & 
          tpcOverrides.options & "\"");
      END;
    END;
  END SetTPCDefaults;

(*--------------------------------------------------------------------------*)
PROCEDURE ReadAll(env          : TextTextTbl.T;
                  rsrcPath     : Rsrc.Path;
                  initBundle   : Bundle.T; 
                  tpcOverrides : TpcOverrides := NIL;
                  tpcWarnings  : BOOLEAN := FALSE; 
                  msg : MsgIF.T := NIL) =

  (*------------------------------------------------------------------------*)
  PROCEDURE ReadIfExists(compactrc : TEXT) =
    VAR
      rd  : FileRd.T;
    BEGIN
      IF FSUtils.IsFile(compactrc) THEN
	TRY
	  rd := FileRd.Open(compactrc);
          MsgX.V(msg, "reading " & compactrc);
	  Read(rd, env, msg);
	EXCEPT
          OSError.E => 
          MsgX.Fatal(msg, "cannot read initialization file: " & compactrc);
	END;
      ELSE
        MsgX.V(msg, "no file " & compactrc);
      END;
    END ReadIfExists;

  (*------------------------------------------------------------------------*)
  PROCEDURE ReadIfNonNil(rd  : Rd.T; name : TEXT) =
    BEGIN
      IF rd = NIL THEN
        MsgX.V(msg, "no remote resource " & name);
      ELSE
        MsgX.V(msg, "reading remote resource " & name);
        Read(rd, env, msg);
      END;
    END ReadIfNonNil; 

  (*------------------------------------------------------------------------*)
  PROCEDURE ReadRemote(server : TEXT; name : TEXT; 
                       eb := CompactClient.EB.Warn) =
    BEGIN
      IF server # NIL THEN
        WITH rd = CompactClient.RemoteRsrcRd(compactClient, server,
                                             name, env, msg, eb) DO
          ReadIfNonNil(rd, name);
        END;
      END;
    END ReadRemote;

  (*------------------------------------------------------------------------*)
  PROCEDURE ReadText(txt : TEXT) =
    BEGIN
      trd := TextRd.New(txt);
      Read(trd, env, msg);
    END ReadText;

  (*------------------------------------------------------------------------*)
  PROCEDURE ReadCascade(server : TEXT; name : TEXT;
                        eb := CompactClient.EB.Warn) =
    BEGIN
      (* first evaluate the compiled-in resources *)
      WITH txt = Bundle.Get(initBundle, name) DO
        IF txt = NIL THEN
          MsgX.V(msg, "no bundled resource " & name);
        ELSE
          MsgX.V(msg, "reading bundled resource " & name);
          ReadText(txt);
        END;
      END;
      (* then check for global resource from server *)
      IF server # NIL THEN
        WITH rd = CompactClient.RemoteRsrcRd(compactClient, server,
                                             name, env, msg, eb) DO
          ReadIfNonNil(rd, name);
        END;
      END;
      (* read resource from file system *)
      TRY
        WITH txt = Rsrc.Get(name, rsrcPath) DO
          IF txt = NIL THEN
            MsgX.V(msg, "no local resource " & name);
          ELSE
            MsgX.V(msg, "reading local resource " & name);
            ReadText(txt);
          END;
        END;
      EXCEPT
        Rsrc.NotFound => (* skip *)
      | Rd.Failure => MsgX.Fatal(msg, "can't read resource: " & name)
      | Thread.Alerted => 
        MsgX.Fatal(msg, "internal error reading resource: " & name)
      END;
      (* now read ~/.{name} if it exists *)
      ReadIfExists(Pathname.Join(MiniEnv.home, "." & name, NIL));
    END ReadCascade;

  (*------------------------------------------------------------------------*)
  CONST
    crc = "compactrc";
  VAR
    compactrc  := Pathname.Join(MiniEnv.home, ".compactrc", NIL);
    server   : TEXT;
    hostname : TEXT;
    hosttype : TEXT;
    ostype   : TEXT;
    platform : TEXT;
    trd      : TextRd.T;
  BEGIN
    server := Env.Get("COMPACTSERVER");
    ReadCascade(server, crc);

    (* if we already have a definition for `compactserver', try to
       read a remote resource again if necessary *)
    IF server = NIL THEN
      server := GetValue(env, "compactserver", msg);
      ReadRemote(server, crc);
    END;
    SetTPCDefaults(env, tpcOverrides, tpcWarnings, msg, warn := FALSE);
    hosttype := GetValue(env, "tpc-hosttype-default", msg);
    ostype   := GetValue(env, "tpc-ostype-default", msg);
    platform := hosttype & "-" & ostype;
    (* let's see if there are any platform specific overrides *)
    ReadCascade(server, crc & "-" & hosttype, CompactClient.EB.Ignore);
    ReadCascade(server, crc & "-" & ostype, CompactClient.EB.Ignore);
    ReadCascade(server, crc & "-" & platform, CompactClient.EB.Ignore);
    hostname := Env.Get("HOSTNAME");
    IF hostname = NIL THEN
      hostname := Env.Get("COMPUTERNAME");
    END;
    IF hostname = NIL THEN
      hostname := System.Hostname();
    END;
    IF hostname = NIL THEN
      Msg.Warning("no hostname");
    ELSE
      WITH i = TextUtils.Pos(hostname, ".") DO
        IF i > 0 THEN
          hostname := Text.Sub(hostname, 0, i);
        END;
      END;
      Msg.V("hostname = " & hostname);
      EVAL env.put("hostname", hostname);
    END;
    IF hostname # NIL THEN
      ReadCascade(server, crc & "-" & hostname, CompactClient.EB.Ignore);
    END;
    compactrc := Env.Get("COMPACTRC");
    IF compactrc # NIL THEN
      ReadIfExists(compactrc);
    END;
  END ReadAll;


(*--------------------------------------------------------------------------*)
PROCEDURE Eval(initBundle   : Bundle.T; 
               tpcOverrides : TpcOverrides := NIL;
               tpcWarnings  : BOOLEAN := FALSE; 
               readEnvFile  : BOOLEAN := TRUE;
               msg : MsgIF.T := NIL) : TextTextTbl.T =

  (*------------------------------------------------------------------------*)
  VAR
    env      := NEW(TextTextTbl.Default).init();
    rsrcPath :  Rsrc.Path;
    origin   :  TEXT;
    config := Pathname.Join(MiniEnv.compactroot, "config", NIL);
  BEGIN
    SetSomeDefaults(env);
    SetEnvVars(env);
    (* MsgX.D(msg, "Params.Count = " & Fmt.Int(Params.Count)); *)
    IF Params.Count > 0 THEN
      (*
      FOR i := 0 TO Params.Count - 1 DO
        MsgX.D(msg, "Param " & Fmt.Int(i) & ": " & Params.Get(i));
      END;
      *)
      origin := Pathname.Prefix(Params.Get(0));
      rsrcPath := Rsrc.BuildPath(
                      Pathname.Join(MiniEnv.home, "compact", NIL),
                      config,
                      origin,
                      PathRepr.Native("/usr/contrib/lib/compact"));
      rsrcPath := RefList.AppendD(
                      rsrcPath, 
                      RefList.List2(
                          PathRepr.Native("/usr/local/lib/compact"),
                          PathRepr.Native("/opt/compact")));
    ELSE
      (* MsgX.D(msg, "2"); *)
      rsrcPath := Rsrc.BuildPath(
                      Pathname.Join(MiniEnv.home, "compact", NIL),
                      config,
                      PathRepr.Native("/usr/contrib/lib/compact"),
                      PathRepr.Native("/usr/local/lib/compact"));
      rsrcPath := RefList.AppendD(
                      rsrcPath, 
                      RefList.List1(
                          PathRepr.Native("/opt/compact")));
    END;

    IF Msg.dFlag THEN
      VAR path := NEW(TextSeq.T).init(); BEGIN
        FOR i := 0 TO RefList.Length(rsrcPath) - 1 DO
          WITH e = RefList.Nth(rsrcPath, i) DO
            TYPECASE e OF
              TEXT => path.addhi(e);
            ELSE
              (* skip *)
            END;
          END;
        END;
        MsgX.D(msg, "search path for global initialization file is " &
          TextUtils.TextSeqToText(path, ":"));
      END;
    END;

    ReadAll(env, rsrcPath, initBundle, tpcOverrides, tpcWarnings, msg);

    (* read environment file if wanted *)
    IF readEnvFile THEN
      WITH fn = EnvFileLocation(MiniEnv.home) DO
        IF FSUtils.IsFile(fn) THEN
          MsgX.V(msg, "reading environment file " & fn);
          WITH sve = NEW(SimpleValueEnv.T).init(msgif := msg) DO
            EVAL sve.setFromTextTextTbl(env);
            TRY
              ReadEnvFile(fn, env, sve, msg);
            EXCEPT
              SimpleValueEnv.E(e) =>
              MsgX.Error(msg, "error reading environment file " & 
                fn & ": " & e);
            END;
          END;
        END;
      END;
    END;

    SetTPCDefaults(env, tpcOverrides, tpcWarnings, msg);
    (* last of all: editor overrides from the environment *)
    IF MiniEnv.editorovr # NIL THEN
      EVAL env.put("editor", MiniEnv.editorovr);
    END;      
    IF MiniEnv.httpdeditorovr # NIL THEN
      EVAL env.put("httpd-editor", MiniEnv.httpdeditorovr);
    END;      
    RETURN env;
  END Eval;

(*--------------------------------------------------------------------------*)
PROCEDURE Read(rd : Rd.T; env : TextTextTbl.T; msg : MsgIF.T := NIL) =
  VAR
    line, varname, value : TEXT;
    trd : TextRd.T;
    poolprefix : TEXT := NIL;
    mappingPattern := RegEx.Compile("-mapping$"); <* NOWARN *>
    mappingList : TextTupleSeq.T;
    localMappingLists := NEW(TextTextTupleSeqTbl.Default).init();
    localMappingList : TextTupleSeq.T;

  (*------------------------------------------------------------------------*)
  PROCEDURE GetLocalMapping(name : TEXT) : TextTupleSeq.T =
    VAR map : TextTupleSeq.T;
    BEGIN
      IF localMappingLists.get(name, map) THEN
        RETURN map;
      ELSE
        RETURN NIL;
      END;
    END GetLocalMapping;

  (*------------------------------------------------------------------------*)
  PROCEDURE CombineMappingLists() =
    VAR name : TEXT;
    BEGIN
      WITH iter = localMappingLists.iterate() DO
        WHILE iter.next(name, localMappingList) DO
          mappingList := GetMapping(name);
          IF mappingList = NIL THEN
            mappingList := localMappingList;
          ELSE
            mappingList := TextTupleSeq.Cat(localMappingList, mappingList);
          END;
          EVAL mappingLists.put(name, mappingList);
        END;
      END;
    END CombineMappingLists;

  BEGIN
    TRY
      WHILE NOT Rd.EOF(rd) DO
        line := TextUtils.Compress(Rd.GetLine(rd));
        IF line # NIL AND NOT Text.Empty(line) AND
           Text.GetChar(line, 0) # '#' AND Text.GetChar(line, 0) # ';' THEN
          trd := TextRd.New(line);
          WHILE NOT Rd.EOF(trd) DO
            varname := TextReadingUtils.GetTokenOrString(trd);
            IF RegEx.Execute(mappingPattern, varname) > -1 THEN
              (* mapping declaration *)
              localMappingList := GetLocalMapping(varname);
              IF localMappingList = NIL THEN
                localMappingList := NEW(TextTupleSeq.T).init();
              END;
              VAR left, op, right : TEXT; 
                  alreadyThere := FALSE; 
                  tuple : TextTuple.T := NIL;
              BEGIN
                left  := TextReadingUtils.GetTokenOrString(trd);
                op    := TextReadingUtils.GetTokenOrString(trd);
                right := TextReadingUtils.GetTokenOrString(trd);
                IF Text.Equal(op, "=>") THEN
                  FOR i := 0 TO localMappingList.size() - 1 DO
                    tuple := localMappingList.get(i);
                    IF Text.Equal(left, tuple.getFst()) THEN
                      alreadyThere := TRUE; EXIT;
                    END;
                  END;
                  IF alreadyThere THEN
                    (* overwrite the existing entry *)
                    EVAL tuple.init(left, right);
                  ELSE
                    WITH tuple = NEW(TextTuple.T).init(left, right) DO
                      localMappingList.addhi(tuple);
                    END;
                  END;
                  IF Msg.dFlag THEN
                    MsgX.V(msg, "  " & varname & ": " & left & " " & op & 
                      " " & right);
                  END;
                ELSE
                  MsgX.Error(msg, "mapping declaration error in " & 
                    "configuration file: " & line);
                END;
              END;
              EVAL localMappingLists.put(varname, localMappingList);
            ELSE
              (* default (name, value) declaration *)
	      value := TextReadingUtils.GetTokenOrString(trd);
              IF Msg.dFlag THEN
                MsgX.V(msg, "  " & varname & "=\"" & value & "\"");
              END;
	      IF Text.Equal(varname, "poolprefix") THEN
		poolprefix := PathRepr.Native(value);
		EVAL env.put("poolprefix", poolprefix);
	      ELSIF Text.Equal(varname, "localpool") THEN
		IF poolprefix # NIL THEN
		  value := Pathname.Join(poolprefix, 
                                         PathRepr.Native(value), NIL);
                ELSE
                  value := PathRepr.Native(value);
		END;
		EVAL env.put("local pool", value);
		EVAL env.put("localpool", value);    (* used by tf *)
	      ELSIF Text.Equal(varname, "projectpool") THEN
		IF poolprefix # NIL THEN
		  value := Pathname.Join(poolprefix, 
                                         PathRepr.Native(value), NIL);
                ELSE
                  value := PathRepr.Native(value);
		END;
		EVAL env.put("project pool", value);
		EVAL env.put("projectpool", value);   (* used by tf *)
	      ELSIF Text.Equal(varname, "globalpool") THEN
		IF poolprefix # NIL THEN
		  value := Pathname.Join(poolprefix, 
                                         PathRepr.Native(value), NIL);
                ELSE
                  value := PathRepr.Native(value);
		END;
		EVAL env.put("global pool", value);
		EVAL env.put("globalpool", value);    (* used by tf *)
	      ELSIF Text.Equal(varname, "useexternalmkdir") THEN
		EVAL env.put("useExternalMakedir", value);
	      ELSIF Text.Equal(varname, "useposixpathnames") THEN
		EVAL env.put("usePosixPathnames", value);
	      ELSIF Text.Equal(varname, "project-collection") THEN
		EVAL env.put(varname, PathRepr.Native(value));
		EVAL env.put("projects", PathRepr.Native(value));
	      ELSIF Text.Equal(varname, "package-collection") THEN
		EVAL env.put(varname, PathRepr.Native(value));
		EVAL env.put("collection", PathRepr.Native(value));
	      ELSIF Text.Equal(varname, "bindir") OR
                    Text.Equal(varname, "localbindir") OR
	            Text.Equal(varname, "projectbindir") OR
                    Text.Equal(varname, "configpath") OR
	            Text.Equal(varname, "globalbindir") OR
	            Text.Equal(varname, "packageprefix") OR
	            Text.Equal(varname, "docdir") OR
	            Text.Equal(varname, "localdocdir") OR
	            Text.Equal(varname, "projectdocdir") OR
	            Text.Equal(varname, "globaldocdir") OR
	            Text.Equal(varname, "collectionroot") OR
	            Text.Equal(varname, "package-collection-path") OR
	            Text.Equal(varname, "project-collection-path") OR
	            Text.Equal(varname, "editor") OR
	            TextUtils.Pos(varname, "httpd-editor") = 0 OR
                    Text.Equal(varname, "httpd-logfile") OR
	            Text.Equal(varname, "browser") OR
	            Text.Equal(varname, "browser-arguments") OR
	            Text.Equal(varname, "browser-remote-arguments") OR
                    Text.Equal(varname, "cvspath") OR
                    Text.Equal(varname, "dcvspath") THEN
		EVAL env.put(varname, PathRepr.Native(value));
	      ELSIF Text.Equal(varname, "configuration") THEN
		EVAL env.put("configuration", value);
		EVAL env.put("tpc-default", value);
		EVAL env.put("tpc", value);
              ELSIF Text.Equal(varname, "linebreakcol") OR
	            Text.Equal(varname, "httpd-resources") OR
                    Text.Equal(varname, "httpd-port") OR
                    Text.Equal(varname, "httpd-addr") OR
                    Text.Equal(varname, "httpd-listen-address") OR
                    Text.Equal(varname, "httpd-help-resources") OR
                    Text.Equal(varname, "httpd-home-page") OR
                    Text.Equal(varname, "httpd-error-page") OR
                    Text.Equal(varname, "httpd-terminate-page") OR
                    Text.Equal(varname, "httpd-source-page") OR
                    Text.Equal(varname, "httpd-execute-page") OR
                    Text.Equal(varname, "httpd-verbose") OR
                    Text.Equal(varname, "httpd-trace") OR
                    Text.Equal(varname, "httpd-debug") OR
                    Text.Equal(varname, "httpd-pkgvmopt") OR
                    Text.Equal(varname, "httpd-pkgmopt") OR
                    Text.Equal(varname, "httpd-m3buildopt") OR
                    Text.Equal(varname, "httpd-buildopt") OR
                    Text.Equal(varname, "httpd-internal-log") OR
                    Text.Equal(varname, "httpd-terminate-without-sessions") OR
                    Text.Equal(varname, "httpd-terminate-default-session") OR
                    Text.Equal(varname, "httpd-colored-projects") OR
                    Text.Equal(varname, "httpd-colored-packages") OR
                    Text.Equal(varname, "httpd-colored-files") OR
                    Text.Equal(varname, "httpd-internal-vc") OR
                    Text.Equal(varname, "httpd-gc-ratio") OR
                    Text.Equal(varname, "httpd-gc-background") OR
                    Text.Equal(varname, "httpd-stacksize") OR
                    Text.Equal(varname, "httpd-default-stacksize") OR
                    Text.Equal(varname, "httpd-global-lock") OR
                    Text.Equal(varname, "prjm-internal-vc") OR
                    Text.Equal(varname, "internal-vc") OR
                    Text.Equal(varname, "repository") OR
                    Text.Equal(varname, "dcvs-repository") OR
                    Text.Equal(varname, "pkgkind") OR
                    Text.Equal(varname, "vcignore") OR
                    Text.Equal(varname, "vc-ignore") OR
                    Text.Equal(varname, "vc-options") OR
                    Text.Equal(varname, "vc-locking") OR
                    Text.Equal(varname, "pics-size") OR
                    Text.Equal(varname, "color-scheme") OR
                    Text.Equal(varname, "enforce-pkgdeps") OR
	            Text.Equal(varname, "editor-args-pathstyle") OR
                    Text.Equal(varname, "optVerbose") OR
                    Text.Equal(varname, "optDebug") OR
                    Text.Equal(varname, "optQuiet") OR
                    Text.Equal(varname, "optForce") THEN
		EVAL env.put(varname, value);
	      ELSIF Text.Equal(varname, "cvsroot") THEN
		EVAL env.put("repository", value);
	      ELSIF Text.Equal(varname, "dcvsroot") THEN
		EVAL env.put("dcvs-repository", value);
	      ELSIF TextUtils.Pos(varname, "tpc-hosttype") = 0 OR
                    TextUtils.Pos(varname, "tpc-ostype") = 0 OR
                    TextUtils.Pos(varname, "tpc-variant") = 0 OR
                    TextUtils.Pos(varname, "tpc-compiler") = 0 OR
                    TextUtils.Pos(varname, "tpc-options") = 0 THEN
		EVAL env.put(varname, value);
	      ELSIF Text.Equal(varname, "tpc") OR
                    Text.Equal(varname, "tpc-default") THEN
		EVAL env.put("tpc", value);
		EVAL env.put("tpc-default", value);
		EVAL env.put("configuration", value);
                (* file cache ignore patterns *)
	      ELSIF Text.Equal(varname, "filecache-ignore-dirs") OR
                    Text.Equal(varname, "filecache-ignore-files") OR
                    Text.Equal(varname, "fingerprint-ignore-dirs") OR
                    Text.Equal(varname, "fingerprint-ignore-files") THEN
                EVAL env.put(varname, value);
              ELSIF TextUtils.Pos(varname, "external-") = 0 AND
                    TextUtils.Pos(varname, "-hook") > 0 THEN
                EVAL env.put(varname, value);
	      ELSE
                MsgX.V(msg, "unexpected variable name in configuration " &
                  "file: " & varname & ", assuming macro", level := 2);
                MsgX.D(msg, "unexpected variable name in configuration " &
                  "file: " & varname & ", assuming macro", level := 2);
                EVAL env.put(varname, value);
	      END;
            END;
          END;
        END;
      END;
      IF env.get("editor-args-pathstyle", value) THEN
        IF Text.Equal(value, "posix") THEN
          MiniEnv.editorArgsPathStyle := 'p';
        ELSIF Text.Equal(value, "win32") THEN
          MiniEnv.editorArgsPathStyle := 'w';
        ELSE
          MiniEnv.editorArgsPathStyle := 'n';
        END;
      END;
    EXCEPT
      Rd.Failure => MsgX.Error(msg, "read failure on config file");
    | Rd.EndOfFile => (* skip *)
    | Thread.Alerted => MsgX.Fatal(msg, "internal error: alert");
    END;
    TRY
      Rd.Close(rd);
    EXCEPT ELSE END;
    CombineMappingLists();
  END Read;

(*--------------------------------------------------------------------------*)
PROCEDURE EnvFileLocation(home : TEXT) : TEXT =
  VAR
    fn : TEXT;
    rc : TEXT;
    compact := Pathname.Join(home, "compact", NIL);
  BEGIN
    fn := Pathname.Join(home, ".compact-env-rc", NIL);
    IF FSUtils.IsFile(fn) THEN
      RETURN fn;
    END;
    fn := Pathname.Join(compact, "compact-env-rc", NIL);
    IF FSUtils.IsFile(fn) THEN
      RETURN fn;
    END;
    fn := Pathname.Join(home, ".compact-env-rc", NIL);
    rc := Pathname.Join(home, ".compactrc", NIL);
    IF FSUtils.IsFile(rc) THEN
      RETURN fn;
    END;
    fn := Pathname.Join(compact, "compact-env-rc", NIL);
    rc := Pathname.Join(compact, "compactrc", NIL);
    IF FSUtils.IsFile(rc) THEN
      RETURN fn;
    END;
    IF FSUtils.IsDir(compact) THEN
      RETURN fn;
    END;
    RETURN Pathname.Join(home, ".compact-env-rc", NIL);
  END EnvFileLocation;

(*--------------------------------------------------------------------------*)
PROCEDURE ReadEnvFile(fn : TEXT; env : TextTextTbl.T; sve : SimpleValueEnv.T;
                      msg : MsgIF.T := NIL) RAISES {SimpleValueEnv.E} =
  VAR
    keys : TextList.T;
    n, v : TEXT;
    old  : TEXT;
  BEGIN
    SimpleValueEnv.ReadFile(fn, sve);
    keys := sve.keyList(rec := FALSE);
    WHILE keys # NIL DO
      n := keys.head;
      IF sve.type(n) = SimpleValueEnv.Type.Text OR
         sve.type(n) = SimpleValueEnv.Type.Int OR
         sve.type(n) = SimpleValueEnv.Type.Nat OR
         sve.type(n) = SimpleValueEnv.Type.List OR
         sve.type(n) = SimpleValueEnv.Type.Seq THEN
        v := sve.substTextVal(n);
        old := GetValue(env, n, msg);
        IF env.put(n, v) AND NOT Text.Equal(v, old) THEN
          MsgX.Warning(msg, "overwriting old setting for variable `" &
            n & "' (`" & old & "') with new value `" & v & 
            "' from file " & fn);
        END;
      END;
      keys := keys.tail;
    END;
  END ReadEnvFile;

(*--------------------------------------------------------------------------*)
PROCEDURE WriteEnvFile(fn : TEXT := NIL; sve : SimpleValueEnv.T;
                       keys : TextList.T := NIL; rec := FALSE)
  RAISES {SimpleValueEnv.E} =
  VAR 
    home : TEXT;
  BEGIN
    IF sve.type("HOME") = SimpleValueEnv.Type.Text THEN
      home := sve.substTextVal("HOME");
    ELSE
      home := MiniEnv.home;
    END;
    IF fn = NIL THEN
      fn := EnvFileLocation(home);
    END;
    SimpleValueEnv.WriteFile(fn, sve, keys, rec);
  END WriteEnvFile;

(*--------------------------------------------------------------------------*)
PROCEDURE ComputePkgKind(env : TextTextTbl.T; msg : MsgIF.T := NIL) : TEXT =
  VAR
    pkgKind         : TEXT;
    tpc_hosttype    : TEXT;
    tpc_ostype      : TEXT;
    platformMapping : TextTupleSeq.T;
    platformPattern : RegEx.Pattern;
    platform        : TEXT;
    suffix          : TEXT := NIL;
    i               : INTEGER;
  BEGIN
    IF NOT env.get("tpc-hosttype", tpc_hosttype) THEN
      tpc_hosttype := "unknown";
    END;
    IF NOT env.get("tpc-ostype", tpc_ostype) THEN
      tpc_ostype := "unknown";
    END;
    platform := tpc_hosttype & "-" & tpc_ostype;
    (* first determine a suffix for the default package kind *)
    platformMapping := GetMapping("platform-suffix-mapping");
    IF platformMapping # NIL THEN
      MsgX.V(msg, "  evaluating pkgkind platform suffix...");
      i := 0;
      WHILE suffix = NIL AND i < platformMapping.size() DO
        WITH tuple = platformMapping.get(i) DO
          MsgX.D(msg, "  trying platform suffix mapping " & tuple.getFst() &
            " => " & tuple.getScd());
          TRY
            platformPattern := RegEx.Compile(tuple.getFst());
            IF RegEx.Execute(platformPattern, platform) > -1 THEN
              suffix := tuple.getScd();
              MsgX.V(msg, "  " & tuple.getFst() & " matching platform, " &
                "using platform suffix " & suffix);
            END;
          EXCEPT
            RegEx.Error(e) => MsgX.Error(msg, 
                                         "malformed regular expression " &
                                         "in configuration file: " & e);
          END;
        END;
        INC(i);
      END;
    END;
    IF suffix # NIL THEN
      EVAL env.put("platform-suffix", suffix);
    END;
    (* platform-suffix adjusted to current platform *)
    IF env.get("pkgkind", pkgKind) THEN
      pkgKind := TextUtils.SubstEnvVars(pkgKind, penv);
      (* all environment variables have been substituted in 
         the pkgkind expression *)
      TRY
        pkgKind := TextUtils.SubstituteVariables(pkgKind, env);
      EXCEPT
        TextUtils.Error(e) => MsgX.Fatal(msg, "error in pkgkind string: " & e);
      END;
      (* all internal environement variables have been
         substituted in the pkgkind expression *)
    ELSE
      pkgKind := NIL;
    END;
    IF pkgKind = NIL THEN
      MsgX.V(msg, "default package kind from compactrc undefined");
    ELSE
      MsgX.V(msg, "default package kind from compactrc is " & pkgKind);
    END;
    RETURN pkgKind;
  END ComputePkgKind;

(*--------------------------------------------------------------------------*)
PROCEDURE ComputeTPC(env : TextTextTbl.T; msg : MsgIF.T := NIL) : TEXT =
  VAR
    cfg             := Env.Get("PKG_CONFIGURATION");
    pkgKindMapping  :  TextTupleSeq.T;
    pkgKindPattern  :  RegEx.Pattern;
    pkgKind         :  TEXT;
    i               :  INTEGER;
  BEGIN
    IF NOT env.get("pkgkind", pkgKind) THEN
      pkgKind := "undefined";
    END;
    MsgX.V(msg, "  detecting target platform configuration (tpc)...");
    IF cfg = NIL THEN
      cfg := Env.Get("TPC");
    END;
    IF cfg = NIL THEN
      (* first use pkgkind-mapping to compute a valid tpc *)
      pkgKindMapping := GetMapping("pkgkind-tpc-mapping");
      IF pkgKindMapping # NIL THEN
        MsgX.V(msg, "  evaluating pkgkind mapping...");
        i := 0;
        WHILE cfg = NIL AND i < pkgKindMapping.size() DO
          WITH tuple = pkgKindMapping.get(i) DO
            MsgX.D(msg, "  trying package kind mapping " & tuple.getFst() & 
              " => " & tuple.getScd());
            TRY
              pkgKindPattern := RegEx.Compile(tuple.getFst());
              IF RegEx.Execute(pkgKindPattern, pkgKind) > -1 THEN
                cfg := tuple.getScd();
                MsgX.V(msg, " " & tuple.getFst() & " matching package kind, " &
                  "using TPC expression " & cfg);
              END;
            EXCEPT
              RegEx.Error(e) => MsgX.Error(msg, 
                                           "malformed regular expression " &
                                           "in configuration file: " & e);
            END;
          END;
          INC(i);
        END;
      END;
      IF cfg = NIL THEN
	(* fall back on default-configuration *)
	IF env.get("configuration", cfg) THEN
	  MsgX.V(msg, "  using TPC from resources: " & cfg);
	ELSE
	  MsgX.Fatal(
              msg, 
              "need target-platform-configuration (TPC) from environment\n" &
              "or from ~/.compactrc or compactrc resource file.\n" &
              "Try `pkgm -man' and search for TPC for more information.");
	END;
      END;
    ELSE
      MsgX.V(msg, "  using TPC/PKG_CONFIGURATION from environment: " & cfg);
      EVAL env.put("configuration", cfg);
    END;
    cfg := TextUtils.SubstEnvVars(cfg, penv);
    (* all environment variables have been substituted in 
       the tpc expression *)
    TRY
      cfg := TextUtils.SubstituteVariables(cfg, env);
    EXCEPT
      TextUtils.Error(e) => MsgX.Fatal(msg, "error in TPC string: " & e);
    END;
    (* all internal environement variables have been
       substituted in the tpc expression *)
    MsgX.V(msg, "  TPC = " & cfg);
    RETURN cfg;
  END ComputeTPC;

(*--------------------------------------------------------------------------*)
PROCEDURE Defined(env : TextTextTbl.T; name : TEXT) : BOOLEAN =
  VAR
    val : TEXT;
  BEGIN
    RETURN env.get(name, val);
  END Defined;

(*--------------------------------------------------------------------------*)
PROCEDURE Get(env : TextTextTbl.T; name : TEXT) : TEXT =
  VAR
    val : TEXT;
  BEGIN
    IF env.get(name, val) THEN
      RETURN val
    ELSE
      RETURN NIL;
    END;
  END Get; 

(*--------------------------------------------------------------------------*)
PROCEDURE Put(env : TextTextTbl.T; name : TEXT; val : TEXT) =
  BEGIN
    EVAL env.put(name, val);
  END Put; 

(*--------------------------------------------------------------------------*)
PROCEDURE GetValue(env : TextTextTbl.T; name : TEXT; msg : MsgIF.T) : TEXT =
  VAR 
    val := Get(env, name);
  BEGIN
    IF val = NIL THEN RETURN NIL END;
    (* obsolete
    IF Defined(env, "HOME") THEN
      val := TextUtils.Substitute(val, "{HOME}", Get(env, "HOME"));
    END;
    IF Defined(env, "USER") THEN
      val := TextUtils.Substitute(val, "{USER}", Get(env, "USER"));
    END;
    *)
    val := TextUtils.SubstEnvVars(val, penv);
    (* all environment variables have been substituted in val *)
    TRY
      val := TextUtils.SubstituteVariables(val, env);
    EXCEPT
      TextUtils.Error(e) => 
      MsgX.Error(msg, "error in variable definition: " & e);
    END;
    (* all internal environement variables have been substituted in val *)
    RETURN val;
  END GetValue; 

(*--------------------------------------------------------------------------*)
PROCEDURE Evaluate(env: TextTextTbl.T; msg : MsgIF.T) : TextTextTbl.T = 
  VAR
    iter := env.iterate();
    res := NEW(TextTextTbl.Default).init(env.size());
    name, val : TEXT;
  BEGIN
    WHILE iter.next(name, val) DO
      IF val # NIL AND Text.Length(val) > 0 THEN
        IF Text.GetChar(val, 0) = '!' THEN
          val := Text.Sub(val, 1);
        ELSE
          val := GetValue(env, name, msg);
          IF val = NIL THEN
            val := "undefined";
          END;
        END;
        EVAL res.put(name, val);
      END;
    END;
    RETURN res;
  END Evaluate;

(*--------------------------------------------------------------------------*)
PROCEDURE GetEditorCmd(env : SimpleValueEnv.T; 
                       files : TEXT; msg : MsgIF.T := NIL) : TEXT =
  VAR
    fnexample : TEXT;
    fnext     : TEXT;
    res       : TEXT := NIL;
  BEGIN
    WITH fnlist = TextUtils.Tokenize(files) DO
      IF fnlist.size() > 0 THEN
        fnexample := fnlist.get(fnlist.size() - 1);
        fnext := Pathname.LastExt(fnexample);
      ELSE
        fnexample := files;
        fnext := "";
      END;
    END;
    IF MiniEnv.editorArgsPathStyle = 'p' THEN
      files := PathRepr.Posix(files);
    ELSIF MiniEnv.editorArgsPathStyle = 'w' THEN
      files := PathRepr.Win32(files);
    ELSE
      files := PathRepr.Native(files);
    END;
    (* let's first try the filename-editor-mapping, if it is defined *)
    IF MappingDefined("filename-editor-mapping") THEN
      VAR
        map := GetMapping("filename-editor-mapping");
        i   := 0;
        re  :  RegEx.Pattern;
      BEGIN
        WHILE i < map.size() AND res = NIL DO
          WITH tuple = map.get(i), 
               pat = tuple.getFst(), 
               ed = tuple.getScd() DO
            TRY
              re := RegEx.Compile(pat);
              IF RegEx.Execute(re, fnexample) > - 1 THEN
                res := ed & " " & files;
              END;
            EXCEPT
              RegEx.Error(e) => 
              MsgX.Error(msg, "malformed regular expression " &
                "in configuration file: " & e);
            END;
          END;
          INC(i);
        END;
      END;
    END;
    IF res = NIL THEN
      (* Let's look for a special declaration based on the file extension, 
         e.g. httpd-editor-txt or httpd-editor-gif *)
      IF env.defined("httpd-editor-" & fnext) THEN
        res := env.substTextVal("httpd-editor-" & fnext) & " " & files;
      ELSIF env.defined("httpd-editor") THEN
        res := env.substTextVal("httpd-editor") & " " & files;
      ELSIF env.defined("editor") THEN
        res := env.substTextVal("editor") & " " & files;
      ELSE
        res := "xterm -e vi " & files;
      END;
    END;
    RETURN res;
  END GetEditorCmd;

(*--------------------------------------------------------------------------*)
CONST ignErr = CompactClient.EB.Ignore;

(*--------------------------------------------------------------------------*)
PROCEDURE GetRsrcText(name : TEXT; bundle : Bundle.T; rsrcPath : Rsrc.Path;
                      env  : TextTextTbl.T := NIL; 
                      senv : SimpleValueEnv.T := NIL;
                      commentStart := "# ";
                      msg  : MsgIF.T := NIL) : TEXT =

  PROCEDURE AddComment(c : TEXT) =
    BEGIN
      IF commentStart = NIL THEN RETURN END;
      res := res & "\n" & commentStart & "--------------------------------" &
                 "--------------------------------------------\n";
      res := res & commentStart & "(" & Fmt.Int(nr) & ") " & c & "\n";
      res := res & commentStart & "--------------------------------" &
                 "--------------------------------------------\n" & "\n";
      INC(nr);
    END AddComment;

  PROCEDURE AddRemoteText(server : TEXT) =
    BEGIN
      IF server # NIL THEN
        WITH data = CompactClient.RemoteRsrcText(compactClient,
                                                 server, name, env,
                                                 NIL, ignErr) DO
          IF data = NIL THEN
            MsgX.V(msg, "no remote resource " & name);
          ELSE
            MsgX.V(msg, "reading remote resource " & name);
            AddComment("remote resource from server " & server);
            res := res & data;
          END;
        END;
      END;
    END AddRemoteText;

  VAR
    data   :  TEXT;
    server :  TEXT;
    nr     := 1;
    res    := "";
  BEGIN
    (* read the compiled-in definitions for package kinds *)
    MsgX.V(msg, "reading bundled resource " & name);
    AddComment("bundled resource");
    res   := res & Bundle.Get(bundle, name);
    (* try the remote resource service *)
    server := Env.Get("COMPACTSERVER");
    AddRemoteText(server);
    IF server = NIL THEN
      IF env # NIL THEN
        server := GetValue(env, "compactserver", msg);
        AddRemoteText(server);
      END;
      IF senv # NIL THEN
        server := senv.textValOrNil("compactserver");
        AddRemoteText(server);
      END;
    END;
    (* last but not least check resources in the local file system *)
    TRY
      data := Rsrc.Get(name, rsrcPath);
      MsgX.V(msg, "reading local resource " & name);
      AddComment("local resource from file");
      res := res & data;
    EXCEPT
      Rsrc.NotFound => (* skip *)
      MsgX.V(msg, "no local resource " & name);
    ELSE
      MsgX.Fatal(msg, "internal error in resource " & name, 1001);
    END;
    RETURN res;
  END GetRsrcText;

(*--------------------------------------------------------------------------*)
PROCEDURE GetSingleRsrcText(name : TEXT; rsrcPath : Rsrc.Path;
                            bundlePath : Rsrc.Path;
                            env  : TextTextTbl.T := NIL;
                            senv : SimpleValueEnv.T := NIL;
                            msg  : MsgIF.T := NIL) : TEXT =

  PROCEDURE GetRemoteText(server : TEXT) =
    BEGIN
      IF server # NIL THEN
        WITH data = CompactClient.RemoteRsrcText(compactClient,
                                                 server, name, env,
                                                 NIL, ignErr) DO
          IF data = NIL THEN
            MsgX.V(msg, "no remote resource " & name);
          ELSE
            MsgX.V(msg, "reading remote resource " & name);
            res := data;
          END;
        END;
      END;
    END GetRemoteText;

  VAR
    res : TEXT := NIL;
    server : TEXT := NIL;
  BEGIN
    (* check if there is a local resource *)
    IF rsrcPath # NIL THEN
      TRY
        res := Rsrc.Get(name, rsrcPath);
        MsgX.V(msg, "reading local resource " & name);
      EXCEPT
        Rsrc.NotFound => (* skip *)
        MsgX.V(msg, "no local resource " & name);
      ELSE
        MsgX.Error(msg, "error reading local resource " & name);
      END;
      IF res # NIL THEN RETURN res END;
    END;
    (* try to contact the remote service defined in `compactserver' *)
    IF env # NIL THEN
      IF senv # NIL THEN
        server := senv.textValOrNil("compactserver");
        GetRemoteText(server);
        IF res # NIL THEN RETURN res END;
      END;
      IF env # NIL THEN
        server := GetValue(env, "compactserver", msg);
        GetRemoteText(server);
        IF res # NIL THEN RETURN res END;
      END;
    END;
    (* try to locate the remote service defined by environment variable *)
    IF server = NIL THEN
      server := Env.Get("COMPACTSERVER");
    END;
    GetRemoteText(server);
    IF res # NIL THEN RETURN res END;
    (* last of all try the bundled resources *)
    IF bundlePath # NIL THEN
      TRY
        res := Rsrc.Get(name, bundlePath);
        MsgX.V(msg, "reading bundled resource " & name);
      EXCEPT
        Rsrc.NotFound => (* skip *)
        MsgX.V(msg, "no bundled resource " & name);
      ELSE
        MsgX.Error(msg, "error reading bundled resource " & name);
      END;
    END;
    RETURN res;
  END GetSingleRsrcText;

(*--------------------------------------------------------------------------*)
VAR
  penv := ProcessEnv.Current();
  compactClient : CompactClient.T := NIL;
BEGIN
  mappingLists := NEW(TextTextTupleSeqTbl.Default).init();
END CompactRC.
