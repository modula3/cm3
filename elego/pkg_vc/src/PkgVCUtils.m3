(*---------------------------------------------------------------------------*)
MODULE PkgVCUtils;

IMPORT System, Fmt, Rd, FileRd, Text, TextRd, Thread, Process,
       RdExtras, ASCII, TextTextTbl, Pathname;
IMPORT TextExtras AS TextEx;
IMPORT (* FSFixed AS *) FS;
IMPORT OSSpecials, MsgIF, MsgX, TextUtils, FSUtils, MiniEnv, PathRepr;

EXCEPTION Failed;

(*---------------------------------------------------------------------------*)
PROCEDURE GetCommitMessage(editor : TEXT; msgif : MsgIF.T; 
                           desc := ""; pkg := "<unknown>") : TEXT =
  VAR
    msg: TEXT; (* constructed message *)
  BEGIN
    IF desc = NIL THEN
      msg := lb & lb & "";
    ELSE
      msg := lb & lb & desc;
    END;
    IF pkg = NIL THEN
      pkg := "";
    ELSIF Text.Empty(pkg) THEN
      pkg := "<unknown>";
    END;
    msg := msg & lb &
      "PKG: Please enter a commit log message for package " &
      pkg &"." & lb &
      "PKG: It should describe all the changes you have made since" & lb &
      "PKG: the last checkout or update." & lb &
      "PKG: Try to be as exact and informative as you can." & lb &
      "PKG: All lines beginning with PKG: will be erased." & lb;
    RETURN GetMessage(editor, msgif, msg);
  END GetCommitMessage;

(*---------------------------------------------------------------------------*)
PROCEDURE MsgWithoutPkgInfoLines(fmsg : TEXT; msgif : MsgIF.T := NIL) : TEXT =
  VAR
    rd   : Rd.T;
    line : TEXT;
    msg  : TEXT;
  BEGIN
    IF fmsg = NIL THEN RETURN NIL END;
    TRY
      rd := TextRd.New(fmsg);
      msg := "";
      WHILE NOT Rd.EOF(rd) DO
        line := Rd.GetLine(rd);
        IF Text.Equal("PKG:", Text.Sub(line, 0, 4)) THEN
          MsgX.D(msgif, "line `" & line & "' omitted");
        ELSE
          MsgX.D(msgif, "line `" & line & "' added");
          msg := msg & lb & line;
        END;
      END;
      Rd.Close(rd);
    EXCEPT ELSE
      msg := "[internal line removal failed]" & lb & fmsg;
    END;
    RETURN TextUtils.Squeeze(TextUtils.Compress(msg));
  END MsgWithoutPkgInfoLines;

(*---------------------------------------------------------------------------*)
PROCEDURE GetMessage(editor : TEXT; msgif : MsgIF.T; msg : TEXT;
                     failIfUnchanged := TRUE) : TEXT =
  VAR 
    pid := Fmt.Int(Process.GetMyID());
    tmpfile := "vcm" & pid;
    cmd  : TEXT;
    rd   : Rd.T;
    ret  : INTEGER;
    okay : BOOLEAN;
    memo : TEXT; (* original msg saved for editing *)
    fmsg : TEXT; (* message read after editing from file *)

  (*-------------------------------------------------------------------------*)
  PROCEDURE RemoveTmpFile() =
    VAR
      bk1 := tmpfile & "~";
      bk2 := tmpfile & ".bak";
    BEGIN
      TRY FS.DeleteFile(tmpfile); EXCEPT ELSE END;
      IF FSUtils.Exists(bk1) THEN
        TRY FS.DeleteFile(bk1); EXCEPT ELSE END;
      END;
      IF FSUtils.Exists(bk2) THEN
        TRY FS.DeleteFile(bk2); EXCEPT ELSE END;
      END;
    END RemoveTmpFile;

  BEGIN (* GetCommitMessage *)
    IF MiniEnv.tmpdir # NIL THEN
      tmpfile := Pathname.Join(MiniEnv.tmpdir, tmpfile, NIL);
    END;
    IF MiniEnv.editorArgsPathStyle = 'p' THEN
      cmd := editor & " " & PathRepr.Posix(tmpfile);
    ELSIF MiniEnv.editorArgsPathStyle = 'w' THEN
      cmd := editor & " " & PathRepr.Win32(tmpfile);
    ELSE
      cmd := editor & " " & tmpfile;
    END;
    memo := msg;
    TRY
      FSUtils.PutFile(tmpfile, msg);
    EXCEPT
      FSUtils.E(e) => MsgX.Error(msgif, "cannot save commit message: " & e);
      RemoveTmpFile();
      RETURN NIL;
    END;
    TRY
      ret := System.Execute(cmd);
      okay := ret = 0;
    EXCEPT
      System.ExecuteError(err) => MsgX.Error2(msgif, 
                                              "CVS.GetCommitMessage", err);
                                  okay := FALSE;
    | Thread.Alerted => MsgX.Error2(msgif, "CVS.GetCommitMessage",
                                    "alerted");
                        okay := FALSE;
    END;
    IF NOT okay THEN
      RemoveTmpFile();
      RETURN NIL;
    END;
    TRY
      rd := FileRd.Open(tmpfile);
      fmsg := Rd.GetText(rd, LAST(INTEGER));
      Rd.Close(rd);
      IF failIfUnchanged THEN
        WITH cset = ASCII.Controls + ASCII.Spaces DO
          IF TextEx.CIEqual(TextUtils.RemoveChars(memo, cset),
                            TextUtils.RemoveChars(fmsg, cset)) THEN
            RAISE Failed;
          END;
        END;
      END;
      msg := MsgWithoutPkgInfoLines(fmsg, msgif);
    EXCEPT ELSE
      RemoveTmpFile();
      RETURN NIL;
    END;
    RemoveTmpFile();
    msg := TextUtils.Compress(msg);
    IF Text.Empty(msg) THEN
      RETURN NIL;
    ELSE
      RETURN msg;
    END;
  END GetMessage;

(*---------------------------------------------------------------------------*)
PROCEDURE CheckCommitMsg(msg, msgFileName, pkgName, pkgRoot, user, 
                         repository, action, name : TEXT; env : TextTextTbl.T)
  RAISES {E} =
  VAR
    id : TEXT := NIL;
    pid := Fmt.Int(Process.GetMyID());
    tmpfile := "vcm" & pid;

  (*-------------------------------------------------------------------------*)
  PROCEDURE RemoveTmpFile() =
    BEGIN
      TRY
        FS.DeleteFile(tmpfile);
      EXCEPT ELSE END;
    END RemoveTmpFile;

  (*-------------------------------------------------------------------------*)
  PROCEDURE LocalEnv(env : TextTextTbl.T) : TextTextTbl.T =
    VAR
      n, v : TEXT;
      iter := env.iterate();
      res := NEW(TextTextTbl.Default).init();
    BEGIN
      WHILE iter.next(n, v) DO
        EVAL res.put(n, v);
      END;
      IF msg # NIL THEN
        EVAL res.put("msg", msg);
      END;
      IF tmpfile # NIL THEN
        EVAL res.put("msgfn", tmpfile);
      END;
      IF pkgName # NIL THEN
        EVAL res.put("pkgname", pkgName);
      END;
      IF pkgRoot # NIL THEN
        EVAL res.put("pkgroot", pkgRoot);
      END;
      IF user # NIL THEN
        EVAL res.put("user", user);
      END;
      IF repository # NIL THEN
        EVAL res.put("repository", repository);
      END;
      IF action # NIL THEN
        EVAL res.put("action", action);
      END;
      IF name # NIL THEN
        EVAL res.put("name", name);
      END;
      IF id # NIL THEN
        EVAL res.put("id", id);
      END;
      RETURN res;
    END LocalEnv;

  (*-------------------------------------------------------------------------*)
  PROCEDURE CheckHook(hook : TEXT) RAISES {E} =
    VAR
      cmd : TEXT;
      ret : INTEGER;
    BEGIN
    IF NOT env.get(hook, cmd) THEN
      IF NOT env.get("external-commit-hook", cmd) THEN
        (* If no hook is defined, we just return without check *)
        RETURN;
      END;
    END;
      TRY
        FSUtils.PutFile(tmpfile, msg);
      EXCEPT
        FSUtils.E(e) => 
        RemoveTmpFile();
        RAISE E("cannot save commit message: " & e);
      END;
      TRY
        cmd := TextUtils.SubstituteVariables(cmd, LocalEnv(env));
        IF Text.Length(cmd) > 0 AND Text.GetChar(cmd, 0) = '!' THEN
          cmd := Text.Sub(cmd, 1);
        END;
      EXCEPT
        TextUtils.Error(e) => RAISE E("variable substitution in " & hook &
          " command failed: " & e);
      END;
      TRY
        ret := System.ExecuteList(cmd);
      EXCEPT
        System.ExecuteError(e) => 
        RemoveTmpFile();
        RAISE E("execution of command `" & cmd & "' failed: " & e);
      | Thread.Alerted =>
        RemoveTmpFile();
        RAISE E("execution of command `" & cmd & "' interrupted");
      END;
      RemoveTmpFile();
      IF ret # 0 THEN
        RAISE E("command `" & cmd & "' exited with " & Fmt.Int(ret));
      END;
    END CheckHook;

  (*-------------------------------------------------------------------------*)
  PROCEDURE ExtractRequestId(rid : TEXT) = 
    CONST NonIdChars = ASCII.Spaces + ASCII.Set{',', '.', '?', '`', ':', ';',
                                                '\'', '\"'};
    VAR
      pos : INTEGER;
      t   : TEXT;
      trd : TextRd.T;
    BEGIN
      IF id # NIL THEN RETURN END;
      pos := TextUtils.Pos(msg, rid);
      IF pos = -1 THEN RETURN END;
      t := Text.Sub(msg, pos + Text.Length(rid) + 1);
      trd := TextRd.New(t);
      TRY
        id := RdExtras.GetText(trd, NonIdChars, NonIdChars);
      EXCEPT ELSE
        (* skip *)
      END;
    END ExtractRequestId;

  (*-------------------------------------------------------------------------*)
  BEGIN (* CheckCommitMsg *)
    IF MiniEnv.tmpdir # NIL THEN
      tmpfile := Pathname.Join(MiniEnv.tmpdir, tmpfile, NIL);
    END;
    IF env = NIL THEN
      RAISE E("cannot check commit message without environment table");
    END;

    IF msg = NIL THEN
      IF msgFileName = NIL THEN
        RAISE E("no commit message");
      END;
      TRY
        msg := FSUtils.FileContents(msgFileName);
      EXCEPT
        FSUtils.E(e) => RAISE E("cannot get commit message: " & e);
      END;
    END;

    ExtractRequestId("request-id");
    ExtractRequestId("request id");
    IF    Text.Equal(action, "package-commit") THEN
      CheckHook("external-package-commit-hook")
    ELSIF Text.Equal(action, "package-release") THEN
      CheckHook("external-package-release-hook")
    ELSIF Text.Equal(action, "project-snapshot") THEN
      CheckHook("external-project-snapshot-hook")
    ELSIF Text.Equal(action, "project-release") THEN
      CheckHook("external-project-release-hook")
    ELSIF Text.Equal(action, "project-change-set") THEN
      CheckHook("external-project-change-set-hook")
    ELSE
      CheckHook("external-commit-hook")
    END;
  END CheckCommitMsg;

VAR
  lb := OSSpecials.LineBreak;
BEGIN
END PkgVCUtils.
