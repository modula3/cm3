(*---------------------------------------------------------------------------*)
MODULE M3MiniShell EXPORTS Main;

IMPORT ParseParams, Stdio, Rd, Wr, Process, Text, Thread, TextSeq, Rsrc, 
       TextExtras AS TextEx, OSError, ASCII, Scan, Env, Pathname;
IMPORT SMsg AS Msg, System, RsrcUtils, TextReadingUtils, Copyright, PathRepr,
       ProcessEnv, Glob, FSUtils, TextUtils, RegEx, DirStack;
IMPORT MiniShellBundle, Creation;
IMPORT (* FSFixed AS *) FS;

<* FATAL Thread.Alerted *>

(*--------------------------------------------------------------------------*)
VAR
  rsrcPath       :  Rsrc.Path;
  targets	 :  TextSeq.T;
  nTargets       :  CARDINAL;
  keepGoing      :  BOOLEAN;
  noAction       :  BOOLEAN;
  pathnameConversion := TRUE;
  useInternalCmds    := TRUE;

(*--------------------------------------------------------------------------*)
VAR
  QuoteChar      := '`';

(*---------------------------------------------------------------------------*)
PROCEDURE M(msg : TEXT) =
  BEGIN
    TRY
      Wr.PutText(Stdio.stdout, msg & "\n");
      Wr.Flush(Stdio.stdout);
    EXCEPT ELSE
      Msg.Fatal("cannot write to stdout", 1000);
    END;
  END M;

(*--------------------------------------------------------------------------*)
PROCEDURE ProcessParameters() =
  VAR 
    pageit := TRUE;
    myArgs := TRUE;
    act    :  TEXT;

  PROCEDURE ActIs(t : TEXT) : BOOLEAN = 
    BEGIN
      RETURN Text.Equal(act, t);
    END ActIs;

  BEGIN
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      TRY
        WHILE myArgs DO
          act := pp.getNext();
          myArgs := FALSE;
          IF ActIs("-nopager") OR ActIs("-pipe") THEN
            myArgs := TRUE;
            pageit := FALSE;
          END;
          (* help option *)
          IF ActIs("-h") OR ActIs("-help") THEN
            myArgs := TRUE;
            TRY
              RsrcUtils.PageResource(rsrcPath, "m3msh.help", pageit);
            EXCEPT
              RsrcUtils.Error(e) => M("error listing description: " & e);
            END;
            Process.Exit(0);
          END;
          (* man option *)
          IF ActIs("-man") OR ActIs("-desc") THEN
            myArgs := TRUE;
            TRY
              RsrcUtils.PageResource(rsrcPath, "m3msh.desc", pageit);
            EXCEPT
              RsrcUtils.Error(e) => M("error listing description: " & e);
            END;
            Process.Exit(0);
          END;
          (* copyright option *)
          IF ActIs("-cr") OR ActIs("-copyright") THEN
            myArgs := TRUE;
            Copyright.Show(Copyright.T.All);
            Process.Exit(0);
          END;
          (* creation date option *)
          IF ActIs("-created") THEN
            M(Creation.Date & " on " & Creation.System);
            Process.Exit(0);
          END;
          (* some message and trace options *)
          IF ActIs("-v") THEN
            myArgs := TRUE;
            Msg.vFlag := TRUE;
          END;
          IF ActIs("-d") THEN
            myArgs := TRUE;
            Msg.dFlag := TRUE;
          END;
          IF ActIs("-q") THEN
            myArgs := TRUE;
            Msg.tFlag := FALSE;
          END;
          IF ActIs("-k") THEN
            myArgs := TRUE;
            keepGoing := TRUE;
          END;
          IF ActIs("-n") THEN
            myArgs := TRUE;
            noAction := TRUE;
          END;
          IF ActIs("-nointernalcmds") OR ActIs("-noic") THEN
            useInternalCmds := FALSE;
          END;
          IF ActIs("-qc") THEN
            myArgs := TRUE;
            WITH arg = pp.getNext() DO
              IF arg # NIL AND NOT Text.Empty(arg) THEN
                QuoteChar := Text.GetChar(arg, 0);
              END;
            END;
          END;
          IF ActIs("-npc") OR ActIs("-nopathnameconversion") THEN
            myArgs := TRUE;
            pathnameConversion := FALSE;
          END;

          (* add more options before this line *)
        END;

        IF NOT ActIs("--") THEN
          DEC(pp.next);
          pp.parsed[pp.next] := FALSE;
        END;
	nTargets := NUMBER(pp.arg^) - pp.next;
        (* build parameters *)
	targets := NEW(TextSeq.T).init(nTargets);
	FOR i := 1 TO nTargets DO
	  VAR t := pp.getNext(); BEGIN
            IF pathnameConversion THEN
              targets.addhi(PathRepr.Native(t));
            ELSE
              targets.addhi(t);
            END;
	  END;
	END;
        pp.finish();
      EXCEPT
        ParseParams.Error => Msg.Fatal("parameter error");
      END;
    END;
    (* all command line parameters handled *)
  END ProcessParameters;

(*---------------------------------------------------------------------------*)
CONST ExecFailure = 16_7FFFFFFF;

(*---------------------------------------------------------------------------*)
VAR
  longList   := FALSE;
  recursive  := FALSE;
  postorder  := FALSE;
  inorder    := FALSE;

(*---------------------------------------------------------------------------*)
PROCEDURE EvalSomeOptions(VAR args : TextSeq.T) =
  VAR cont := TRUE;
  BEGIN
    longList   := FALSE;
    recursive  := FALSE;
    postorder  := FALSE;
    inorder    := FALSE;
    WHILE cont AND args.size() > 0 DO
      WITH arg = args.get(0) DO
        IF Text.Equal(arg, "-l") THEN
          longList := TRUE;
          EVAL args.remlo();
        ELSIF Text.Equal(arg, "-r") THEN
          recursive := TRUE;
          EVAL args.remlo();
        ELSIF Text.Equal(arg, "-post") THEN
          postorder := TRUE;
          EVAL args.remlo();
        ELSIF Text.Equal(arg, "-in") THEN
          inorder := TRUE;
          EVAL args.remlo();
        ELSE
          cont := FALSE;
        END;
      END;
    END;
  END EvalSomeOptions;

(*---------------------------------------------------------------------------*)
PROCEDURE List(fn : Pathname.T) RAISES {} =
  VAR t : TEXT;
  BEGIN
    IF longList THEN 
      IF FSUtils.IsFile(fn) THEN
        t := " [file]";
      ELSIF FSUtils.IsDir (fn) THEN
        t := " [dir]";
      ELSE
        t := " [unknown type]";
      END;
      M(fn & t);
    ELSE
      M(fn);
    END;
  END List; 

(*---------------------------------------------------------------------------*)
PROCEDURE ListRec(fn : Pathname.T) RAISES {FSUtils.E} =
  BEGIN
    IF FSUtils.IsDir(fn) THEN
      VAR
        fns   := FileList(fn, levels := 1, prefix := fn);
        dirs  := NEW(TextSeq.T).init();
        files := NEW(TextSeq.T).init();
      BEGIN
        FOR i := 0 TO fns.size() - 1 DO
          WITH act = fns.get(i) DO
            IF FSUtils.IsDir(act) THEN
              IF inorder OR postorder THEN
                IF longList THEN List(act); END;
                ListRec(act);
              ELSE
                dirs.addhi(act);
              END;
            ELSE
              IF postorder THEN
                files.addhi(act);
              ELSE
                List(act);
              END;
            END;
          END;
        END;
        IF postorder THEN
          FOR j := 0 TO files.size() - 1 DO
            WITH file = files.get(j) DO
              List(file);
            END;
          END;
        ELSE
          FOR j := 0 TO dirs.size() - 1 DO
            WITH dir = dirs.get(j) DO
              IF longList THEN List(dir); END;
              ListRec(dir);
            END;
          END;
        END;
      END;
    ELSE
      List(fn);
    END;
  END ListRec; 

(*---------------------------------------------------------------------------*)
PROCEDURE Rm(fn : Pathname.T) RAISES {FSUtils.E} =
  BEGIN
    IF FSUtils.Exists(fn) THEN
      FSUtils.Rm(fn);
    END;
  END Rm;

(*---------------------------------------------------------------------------*)
PROCEDURE RmRec(fn : Pathname.T) RAISES {FSUtils.E} =
  BEGIN
    IF FSUtils.Exists(fn) THEN
      FSUtils.RmRec(fn);
    END;
  END RmRec;

(*---------------------------------------------------------------------------*)
PROCEDURE Rmdir(fn : Pathname.T) RAISES {FSUtils.E} =
  BEGIN
    IF FSUtils.Exists(fn) THEN
      FSUtils.Rmdir(fn);
    END;
  END Rmdir;

(*---------------------------------------------------------------------------*)
PROCEDURE Exec(pgm : TEXT; args : TextSeq.T; env : ProcessEnv.T) : INTEGER =

  TYPE P = PROCEDURE(fn : Pathname.T) RAISES {FSUtils.E};

  PROCEDURE Apply(p : P; args : TextSeq.T) : INTEGER RAISES {FSUtils.E} =
    BEGIN
      FOR i := 0 TO args.size() -1 DO
        WITH fn = args.get(i) DO
          p(fn);
        END;
      END;
      RETURN 0;
    END Apply;

  BEGIN
    TRY
      IF Text.Equal(pgm, "cd") THEN
        IF args.size() > 0 THEN
          DirStack.SetWorkingDir(args.get(0));
        ELSE
          DirStack.SetWorkingDir(homeDir);
        END;
        RETURN 0;
      ELSIF Text.Equal(pgm, "pushd") THEN
        IF args.size() > 0 THEN
          DirStack.PushDir(args.get(0));
        ELSE
          DirStack.PushDir(homeDir);
        END;
        RETURN 0;
      ELSIF Text.Equal(pgm, "popd") THEN
        DirStack.PopDir();
        RETURN 0;
      ELSIF Text.Equal(pgm, "exit") THEN
        VAR ret : INTEGER := 0; BEGIN
          IF args.size() > 0 THEN
            TRY
              ret := Scan.Int(args.get(0));
            EXCEPT ELSE
              ret := -1;
            END;
          END;
          Process.Exit(ret);
          RETURN ret; (* make the compiler happy *)
        END;
      ELSE
        IF useInternalCmds THEN
          IF Text.Equal(pgm, "touch") THEN
            RETURN Apply(FSUtils.Touch, args);
          ELSIF Text.Equal(pgm, "mkdir") THEN 
            RETURN Apply(FSUtils.Mkdir, args);
          ELSIF Text.Equal(pgm, "rm") THEN
            EvalSomeOptions(args);
            IF recursive THEN
              RETURN Apply(RmRec, args);
            ELSE
              RETURN Apply(Rm, args);
            END;
          ELSIF Text.Equal(pgm, "rmdir") THEN
            RETURN Apply(Rmdir, args);
          ELSIF Text.Equal(pgm, "list") THEN
            EvalSomeOptions(args);
            IF args.size() = 0 THEN
              args := ExpandWildcards("*");
            END;
            IF recursive THEN
              RETURN Apply(ListRec, args);
            ELSE
              RETURN Apply(List, args);
            END;
          END;
        END;
        RETURN System.Exec(pgm, args, env);
      END;
    EXCEPT
      System.ExecuteError(e) => Msg.Error(e); RETURN ExecFailure;
    | DirStack.Error(e)      => Msg.Error(e); RETURN ExecFailure;
    | FSUtils.E(e)           => Msg.Error(e); RETURN ExecFailure;
    END;
  END Exec;

(*---------------------------------------------------------------------------*)
PROCEDURE FileList(dir : TEXT; levels := 1; prefix := NIL) : TextSeq.T =
  VAR
    res  := NEW(TextSeq.T).init();

  PROCEDURE FL(dir : TEXT; levels : INTEGER; prefix : TEXT; 
               VAR res : TextSeq.T) =
    VAR
      iter :  FS.Iterator;
      fn   :  TEXT;
    BEGIN
      IF NOT FSUtils.IsDir(dir) THEN
        Msg.Error(dir & " seems to be no directory");
        RETURN;
      END;
      IF levels > 0 THEN
        TRY
          iter := FS.Iterate(dir);
          WHILE iter.next(fn) DO
            IF prefix # NIL THEN
              fn := prefix & "/" & fn;
            END;
            IF FSUtils.IsDir(fn) THEN
              FL(fn, levels - 1, fn, res);
            END;
            res.addhi(fn);
          END;
        EXCEPT
          OSError.E(e) => Msg.Error("cannot list dir " &
            dir & ": " & System.AtomListToText(e));
        END;
      END;
    END FL;

  BEGIN (* FileList *)
    FL(dir, levels, prefix, res);
    RETURN res;
  END FileList;

(*---------------------------------------------------------------------------*)
PROCEDURE ExpandWildcards(token : TEXT) : TextSeq.T = 
  VAR
    res     := NEW(TextSeq.T).init();
    levels  := TextUtils.Split(token, "/").size();
    files   :  TextSeq.T;
    first   :  CHAR;
    options := Glob.MatchOptions{Glob.MatchOption.Pathname, 
                                 Glob.MatchOption.Period};
    rest    :  TEXT;
    updirs  :  TEXT := NIL;
  BEGIN
    IF token = NIL OR Text.Empty(token) THEN 
      res.addhi("");
      RETURN res;
    END;
    first := Text.GetChar(token, 0);
    IF first = QuoteChar THEN
      res.addhi(Text.Sub(token, 1, LAST(INTEGER)));
      RETURN res;
    END;
    IF first = '/' THEN
      files := FileList("/", levels - 1, "");
    ELSE
      rest := token;
      (* cut off leading `./' sequences *)
      WHILE TextUtils.Pos(rest, "./") = 0 DO
        rest := Text.Sub(rest, 2, LAST(INTEGER));
        DEC(levels);
        IF updirs = NIL THEN
          updirs := ".";
        ELSE
          updirs := updirs & "/.";
        END;
      END;
      IF TextUtils.Pos(rest, "../") = 0 THEN
        (* rest starts with ../ *)
        REPEAT
          rest := Text.Sub(rest, 3, LAST(INTEGER));
          IF updirs = NIL THEN
            updirs := "..";
          ELSE
            updirs := updirs & "/..";
          END;
          DEC(levels);
        UNTIL TextUtils.Pos(rest, "../") # 0;
        files := FileList(updirs, levels, updirs);
      ELSE
        files := FileList(".", levels, updirs);
      END;
    END;
    FOR i := 0 TO files.size() - 1 DO
      WITH fn = files.get(i) DO
        TRY
          IF Glob.Match(token, fn, options) THEN
            res.addhi(fn);
          END;
        EXCEPT
          RegEx.Error(e) => Msg.Error("error in globbing expression: " & e);
        END;
      END;
    END;
    RETURN res;
  END ExpandWildcards;

(*---------------------------------------------------------------------------*)
PROCEDURE ContainsWildcards(token : TEXT) : BOOLEAN = 
  VAR index : CARDINAL := 0;
  BEGIN
    RETURN TextEx.FindCharSet(token, ASCII.Set{'*', '?'}, index);
  END ContainsWildcards;

(*---------------------------------------------------------------------------*)
PROCEDURE DoWildcardExpansion(token : TEXT; VAR args : TextSeq.T) =
  VAR
    first   :  CHAR;
  BEGIN
    IF ContainsWildcards(token) THEN
      Msg.D("expand wildcards");
      WITH res = ExpandWildcards(token) DO
        FOR i := 0 TO res.size() - 1 DO
          WITH arg = res.get(i) DO
            Msg.D(" expanded to `" & arg & "'");
            args.addhi(arg);
          END;
        END;
        IF res.size() = 0 THEN
          args.addhi(token);
        END;
      END;
    ELSE
      Msg.D("no wildcards");
      IF token = NIL OR Text.Empty(token) THEN 
        args.addhi("");
      ELSE
        first := Text.GetChar(token, 0);
        IF first = QuoteChar THEN
          WITH arg = Text.Sub(token, 1, LAST(INTEGER)) DO
            args.addhi(arg);
            Msg.D(" reduced to `" & arg & "'");
          END;
        ELSE
          Msg.D(" no change `" & token & "'");
          args.addhi(token);
        END;
      END;
    END;
  END DoWildcardExpansion;

(*---------------------------------------------------------------------------*)
VAR
  token : TEXT;
  pgm   : TEXT := NIL;
  args  : TextSeq.T := NIL;
  done  : BOOLEAN;
  ret   : INTEGER := 0;
  i     : INTEGER;
  env   : ProcessEnv.T := ProcessEnv.Current(); 
  homeDir := "/";
BEGIN
  Msg.tFlag := TRUE;
  token := Env.Get("HOME");
  IF token # NIL THEN
    homeDir := token;
  END;
  rsrcPath := 
      Rsrc.BuildPath("/usr/contrib/lib/compact",
                     "/usr/local/lib/compact",
                     "/opt/compact",
                     MiniShellBundle.Get());
  (* rsrcPath defined *)

  ProcessParameters();

  IF nTargets > 0 THEN
    (* execute only the given command line *)
    i := 0;
    WHILE i < nTargets DO
      pgm := targets.get(i);
      INC(i);
      args := NEW(TextSeq.T).init(10);
      done := FALSE;
      WHILE NOT done AND i < nTargets DO
        token := targets.get(i);
        INC(i);
        Msg.D(" token = `" & token & "'");
        IF Text.Equal(token, ";") THEN
          ret := Exec(pgm, args, env);
          done := TRUE;
        ELSIF Text.Equal(token, "&&") THEN
          ret := Exec(pgm, args, env);
          IF ret # 0 THEN
            Process.Exit(ret);
          END;
          done := TRUE;
        ELSIF Text.Equal(token, "||") THEN
          ret := Exec(pgm, args, env);
          IF ret = 0 THEN
            Process.Exit(ret);
          END;
          done := TRUE;
        ELSE
          DoWildcardExpansion(token, args);
        END;
      END;
    END;
  ELSE
    (* read from stdin until EOF *)
    TRY
      WHILE NOT Rd.EOF(Stdio.stdin) DO
        pgm := TextReadingUtils.GetToken(Stdio.stdin); 
        args := NEW(TextSeq.T).init(10);
        done := FALSE;
        WHILE NOT done AND NOT Rd.EOF(Stdio.stdin) DO
          token := TextReadingUtils.GetTokenOrString(Stdio.stdin);
          Msg.D(" token = " & token);
          IF Text.Equal(token, ";") THEN
            ret := Exec(pgm, args, env);
            done := TRUE;
          ELSIF Text.Equal(token, "&&") THEN
            ret := Exec(pgm, args, env);
            IF ret # 0 THEN
              Process.Exit(ret);
            END;
            done := TRUE;
          ELSIF Text.Equal(token, "||") THEN
            ret := Exec(pgm, args, env);
            IF ret = 0 THEN
              Process.Exit(ret);
            END;
            done := TRUE;
          ELSE
            DoWildcardExpansion(token, args);
          END;
        END;
      END;
    EXCEPT
      Rd.Failure,
      Rd.EndOfFile => (* skip *)
    END;
  END;
  IF NOT done AND pgm # NIL AND args # NIL THEN
    ret := Exec(pgm, args, env);
  END;
  Process.Exit(ret);
END M3MiniShell.
