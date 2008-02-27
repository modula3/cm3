(* Copyright 1999-2002 elego Software Solutions GmbH, Berlin, Germany.
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
 * $Id: System.m3,v 1.3 2008-02-27 23:06:15 wagner Exp $ *)

(*---------------------------------------------------------------------------*)
MODULE System EXPORTS System;

IMPORT ASCII, Process, TextRd, Rd, RdExtras, Wr, Pipe,
       File, FileRd, FileWr, Thread, 
       AtomList, Atom, Text, TextSeq, OSError, Pathname, RegularFile,
       RefSeq, IntRefTbl;
IMPORT MsgX, MsgIF, ProcessEnv, TextReadingUtils, OSSpecials;
IMPORT (* FSFixed AS *) FS;

(*---------------------------------------------------------------------------*)
PROCEDURE AtomListToText(l : AtomList.T) : TEXT =
  VAR res : TEXT;
  BEGIN
    res := Atom.ToText(l.head);
    l := l.tail;
    WHILE l # NIL DO
      res := res & " " & Atom.ToText(l.head);
      l := l.tail;
    END;
    RETURN res;
  END AtomListToText;

(*---------------------------------------------------------------------------*)
PROCEDURE ParListToText(params : TextSeq.T) : TEXT =
  VAR res : TEXT := "";
  BEGIN
    FOR i := 0 TO params.size() - 1 DO
      res := res & ", " & params.get(i);
    END;
    RETURN res;
  END ParListToText;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecWithFileHandles(pgm : TEXT; params : TextSeq.T;
                              stdin, stdout, stderr : File.T;
                              env : ProcessEnv.T := NIL; 
                              msgif : MsgIF.T := NIL;
                              wd : TEXT := NIL) : INTEGER 
  RAISES {ExecuteError} =
  VAR p: Process.T;
  BEGIN
    p := RunWithFileHandles(pgm, params, stdin, stdout, stderr,
                            env, msgif, wd);
    IF p = NIL THEN
      RETURN -1;
    ELSE
      RETURN Process.Wait(p);
    END;
  END ExecWithFileHandles;

(*---------------------------------------------------------------------------*)
PROCEDURE RunWithFileHandles(pgm : TEXT; params : TextSeq.T;
                             stdin, stdout, stderr : File.T;
                             env : ProcessEnv.T := NIL; 
                             msgif : MsgIF.T := NIL;
                             wd : TEXT := NIL) : Process.T
  RAISES {ExecuteError} =
  VAR
    args := NEW(REF ARRAY OF TEXT, params.size());
    stdinParent, stdoutParent, stderrParent : File.T;
    child :  Process.T;
    senv  := ProcessEnv.SystemRepr(env);
  BEGIN
    MsgX.D(msgif, "System.RunWithFileHandles(" & pgm & 
      ParListToText(params) & ")");

    FOR i := 0 TO params.size() - 1 DO
      args^[i] := OSSpecials.QuotedProcessArgument(params.get(i));
    END;
    Process.GetStandardFileHandles(stdinParent, stdoutParent, stderrParent);
    IF stdin = NIL THEN
      stdin := stdinParent;
    END;
    IF stdout = NIL THEN
      stdout := stdoutParent;
    END;
    IF stderr = NIL THEN
      stderr := stderrParent;
    END;
    TRY
      (*
      IF Text.Equal(pgm, "echo") THEN
        VAR 
          wr := NEW(FileWr.T).init(stdout, TRUE); 
          nl := TRUE;
        BEGIN
          FOR i := 0 TO params.size() - 1 DO
            WITH par = params.get(i) DO
              IF i = 0 AND Text.Equal(par, "-c") OR
                Text.Equal(par, "-n") THEN
                nl := FALSE;
              ELSE
                (* add blank after all params except the last *)
                IF i < params.size() - 1 THEN
                  Wr.PutText(wr, par & " ");
                ELSE
                  Wr.PutText(wr, par);
                END;
              END;
            END;
          END;
          Wr.Flush(wr);
          IF nl THEN
            Wr.PutChar(wr, '\n');
            Wr.Flush(wr);
          END;
        END;
        RETURN NIL; (* no process created, just bail *)
      ELSE
      *)
      child := Process.Create(pgm, args^, senv, wd, stdin, stdout, stderr);
      (*END;*)
    EXCEPT 
      OSError.E(list) => RAISE ExecuteError("execution of `" & pgm & 
        "' failed: " & AtomListToText(list));
    END;
    RETURN child;
  END RunWithFileHandles;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecNW(VAR estdin: File.T;
                 VAR estdout: File.T;
                 VAR estderr: File.T;
                 pgm: TEXT; 
                 params: TextSeq.T;
                 env: ProcessEnv.T := NIL;
                 msgif: MsgIF.T := NIL;
                 wd: TEXT := NIL;
                 pstdin  : File.T := NIL;
                 pstdout : File.T := NIL;
                 pstderr : File.T := NIL) : Process.T
  RAISES {ExecuteError} =

  PROCEDURE MakeAbsolute(pn : Pathname.T) : Pathname.T =
    BEGIN
      IF Pathname.Absolute(pn) THEN
        RETURN pn;
      ELSE
        RETURN Pathname.Join(wd, pn, NIL);
      END;
    END MakeAbsolute; 
 
  VAR
    stdin, stdout, stderr : RegularFile.T;
    args   := NEW(TextSeq.T).init(params.size());
    i      : INTEGER;
    psize  := params.size();
  BEGIN
    MsgX.D(msgif, "System.ExecNW(" & pgm & ParListToText(params) & ")");
    stdin := NIL; stdout := NIL; stderr := NIL;
    i := 0;
    WHILE i < psize DO
      WITH arg = params.get(i) DO
        IF Text.Equal(arg, "<") THEN
          INC(i);
          IF i = psize THEN 
            RAISE ExecuteError("missing redirection argument");
          END;
          IF stdin # NIL THEN
            RAISE ExecuteError("too many input redirections");
          END;
          VAR
            pn := MakeAbsolute(params.get(i));
          BEGIN
            TRY
              stdin := FS.OpenFile(pn, truncate := FALSE,
                                   create := FS.CreateOption.Never);
            EXCEPT
              OSError.E(list) => RAISE ExecuteError("open failed on " &
                pn & ": " & AtomListToText(list));
            END;
          END;
        ELSIF Text.Equal(arg, ">") OR Text.Equal(arg, "1>") THEN
          INC(i);
          IF i = psize THEN 
            RAISE ExecuteError("missing redirection argument");
          END;
          IF stdout # NIL THEN
            RAISE ExecuteError("too many output redirections");
          END;
          VAR
            pn := MakeAbsolute(params.get(i));
          BEGIN
            TRY
              stdout := FS.OpenFile(pn, truncate := TRUE,
                                    create := FS.CreateOption.Ok);
            EXCEPT
              OSError.E(list) => RAISE ExecuteError("open failed on " &
                pn & ": " & AtomListToText(list));
            END;
          END;
        ELSIF Text.Equal(arg, ">>") OR Text.Equal(arg, "1>>") THEN
          INC(i);
          IF i = psize THEN 
            RAISE ExecuteError("missing redirection argument");
          END;
          IF stdout # NIL THEN
            RAISE ExecuteError("too many output redirections");
          END;
          VAR
            pn := MakeAbsolute(params.get(i));
          BEGIN
            TRY
              stdout := FS.OpenFile(pn, truncate := FALSE,
                                    create := FS.CreateOption.Ok);
              EVAL stdout.seek(RegularFile.Origin.End, 0);
            EXCEPT
              OSError.E(list) => RAISE ExecuteError("open failed on " &
                pn & ": " & AtomListToText(list));
            END;
          END;
        ELSIF Text.Equal(arg, "2>") THEN
          INC(i);
          IF i = psize THEN 
            RAISE ExecuteError("missing redirection argument");
          END;
          IF stderr # NIL THEN
            RAISE ExecuteError("too many output redirections");
          END;
          VAR
            pn := MakeAbsolute(params.get(i));
          BEGIN
            TRY
              stderr := FS.OpenFile(pn, truncate := TRUE,
                                    create := FS.CreateOption.Ok);
            EXCEPT
            | OSError.E(list) => RAISE ExecuteError("open failed on " &
                pn & ": " & AtomListToText(list));
            END;
          END;
        ELSIF Text.Equal(arg, "2>>") THEN
          INC(i);
          IF i = psize THEN 
            RAISE ExecuteError("missing redirection argument");
          END;
          IF stderr # NIL THEN
            RAISE ExecuteError("too many output redirections");
          END;
          VAR
            pn := MakeAbsolute(params.get(i));
          BEGIN
            TRY
              stderr := FS.OpenFile(pn, truncate := FALSE,
                                    create := FS.CreateOption.Ok);
              EVAL stderr.seek(RegularFile.Origin.End, 0);
            EXCEPT
              OSError.E(list) => RAISE ExecuteError("open failed on " &
                pn & ": " & AtomListToText(list));
            END;
          END;
        ELSIF Text.Equal(arg, "&>") THEN
          INC(i);
          IF i = psize THEN 
            RAISE ExecuteError("missing redirection argument");
          END;
          IF stdout # NIL OR stderr # NIL THEN
            RAISE ExecuteError("too many output redirections");
          END;
          VAR
            pn := MakeAbsolute(params.get(i));
          BEGIN
            TRY
              stdout := FS.OpenFile(pn, truncate := TRUE,
                                    create := FS.CreateOption.Ok);
            EXCEPT
              OSError.E(list) => RAISE ExecuteError("open failed on " &
                pn & ": " & AtomListToText(list));
            END;
          END;
          stderr := stdout;
        ELSIF Text.Equal(arg, "&>>") THEN
          INC(i);
          IF i = psize THEN 
            RAISE ExecuteError("missing redirection argument");
          END;
          IF stdout # NIL OR stderr # NIL THEN
            RAISE ExecuteError("too many output redirections");
          END;
          VAR
            pn := MakeAbsolute(params.get(i));
          BEGIN
            TRY
              stdout := FS.OpenFile(pn, truncate := FALSE,
                                    create := FS.CreateOption.Ok);
              EVAL stdout.seek(RegularFile.Origin.End, 0);
            EXCEPT
              OSError.E(list) => RAISE ExecuteError("open failed on " &
                pn & ": " & AtomListToText(list));
            END;
          END;
          stderr := stdout;
        ELSIF Text.Equal(arg, "$<") THEN (* file contents substitution *)
          INC(i);
          WHILE i < psize AND NOT Text.Equal(params.get(i), ">") DO
	    VAR
	      rd    :  FileRd.T;
	      token :  TEXT;
              fn    := MakeAbsolute(params.get(i));
	    BEGIN
	      TRY
		rd := FileRd.Open(fn);
	      EXCEPT
		OSError.E => RAISE ExecuteError("cannot open file " & fn);
	      END;
	      TRY
		WHILE NOT Rd.EOF(rd) DO
		  token := TextReadingUtils.GetTokenOrString(rd);
		  args.addhi(token);
		END;
	      EXCEPT
		Rd.Failure, 
		Thread.Alerted => RAISE ExecuteError("cannot read file " & 
                                                        fn);
	      | Rd.EndOfFile => (* skip *)
	      END;
	      TRY
		Rd.Close(rd);
	      EXCEPT
		Rd.Failure, 
		Thread.Alerted => RAISE ExecuteError("cannot close file " &
							fn);
	      END;
	    END;
            INC(i);
          END;
          IF i = psize THEN 
            RAISE ExecuteError("missing file contents closing `>' ");
          END;
        ELSIF Text.Equal(arg, "$(") THEN (* command substitution *)
          RAISE ExecuteError("Sorry, command substitution not yet " &
                "implemented. ");
        ELSE
          args.addhi(arg);
        END;
      END;
      INC(i);
    END;

    estdin := stdin;
    estdout := stdout;
    estderr := stderr;
    IF estdin = NIL AND pstdin # NIL THEN
      estdin := pstdin;
    END;
    IF estdout = NIL AND pstdout # NIL THEN
      estdout := pstdout;
    END;
    IF estderr = NIL AND pstderr # NIL THEN
      estderr := pstderr;
    END;
    RETURN RunWithFileHandles(pgm, args, estdin, estdout, estderr, env, 
                              msgif, wd);
  END ExecNW;

(*---------------------------------------------------------------------------*)
PROCEDURE Exec(pgm : TEXT; params : TextSeq.T; env : ProcessEnv.T := NIL;
               msgif : MsgIF.T := NIL; wd : TEXT := NIL;
               pstdin  : File.T := NIL;
               pstdout : File.T := NIL;
               pstderr : File.T := NIL) : INTEGER 
  RAISES {ExecuteError} =

  VAR
    stdin, stdout, stderr : File.T := NIL;
    ret: INTEGER;
    p : Process.T;
  BEGIN
    MsgX.D(msgif, "System.Exec(" & pgm & ParListToText(params) & ")");

    p := ExecNW(stdin, stdout, stderr,
                pgm, params, env, msgif, wd, pstdin, pstdout, pstderr);

    IF p = NIL THEN
      ret := -1;
    ELSE
      ret := Process.Wait(p);
    END;
    IF stdin # NIL THEN
      TRY
        stdin.close();
      EXCEPT
        OSError.E(list) => RAISE ExecuteError("close failed on stdin: " & 
          AtomListToText(list));
      END;
    END;
    IF stdout # NIL THEN
      TRY
        stdout.close();
      EXCEPT
        OSError.E(list) => RAISE ExecuteError("close failed on stdout: " & 
          AtomListToText(list));
      END;
    END;
    IF stderr # NIL AND stderr # stdout THEN
      TRY
        stderr.close();
      EXCEPT
        OSError.E(list) => RAISE ExecuteError("close failed on stderr: " & 
          AtomListToText(list));
      END;
    END;
    RETURN ret;
  END Exec;

(*---------------------------------------------------------------------------*)
PROCEDURE Execute(cmd : TEXT; env : ProcessEnv.T := NIL; 
                  msgif : MsgIF.T := NIL; wd : TEXT := NIL) : INTEGER
  RAISES {ExecuteError, Thread.Alerted} =
  VAR
    rd := TextRd.New(cmd);
    token : TEXT;
    pgm   : TEXT;
    args  := NEW(TextSeq.T).init(10);
  BEGIN
    MsgX.D(msgif, "System.Execute(" & cmd & ")");
    TRY
      pgm := TextReadingUtils.GetToken(rd); 
      WHILE NOT Rd.EOF(rd) DO
        token := TextReadingUtils.GetTokenOrString(rd);
        args.addhi(token);
      END;
    EXCEPT
      Rd.Failure,
      Rd.EndOfFile => (* skip *)
    END;
    RETURN Exec(pgm, args, env, msgif, wd);
  END Execute;

(*---------------------------------------------------------------------------*)
PROCEDURE ExecuteShell(cmd : TEXT; shell := "/bin/sh"; 
                       env : ProcessEnv.T := NIL; 
                       msgif : MsgIF.T := NIL; wd : TEXT := NIL) : INTEGER 
  RAISES {ExecuteError} =
  VAR
    stdin, stdout, stderr: File.T;
    child : Process.T;
    args  :  ARRAY [1..2] OF TEXT;
    senv  := ProcessEnv.SystemRepr(env);
  BEGIN
    MsgX.D(msgif, "System.ExecuteShell(" & cmd & ")");
    Process.GetStandardFileHandles(stdin, stdout, stderr);
    args[1] := "-c";
    args[2] := OSSpecials.QuotedProcessArgument(cmd);
    TRY
      child := Process.Create(shell, args, senv, wd, 
                              stdin, stdout, stderr);
    EXCEPT 
      OSError.E(list) => 
      RAISE ExecuteError("execution of `" & shell & " -c " & cmd & 
            "' failed: " & AtomListToText(list));
    END;

    RETURN Process.Wait(child);
  END ExecuteShell; 

(*---------------------------------------------------------------------------*)
PROCEDURE ExecuteList(cmd : TEXT; env : ProcessEnv.T := NIL; 
                      msgif : MsgIF.T := NIL; wd : TEXT := NIL) : INTEGER 
  RAISES {ExecuteError, Thread.Alerted} =

  CONST
    STDINC  = 10000000;
    STDOUTC = 10000001;
    STDERRC = 10000002;
    PIPEWR  = 10000003;

    SPACES   = ASCII.Spaces;
    IDEND    = SPACES + ASCII.Set{ '<', '>', '&', '|', ';', '$' };
    OPS      = ASCII.Set{ '<', '>', '&', '1', '2', '|', ';', '$' };
    OPEND    = ASCII.All - OPS;
    STRDELIM = ASCII.Set{ '\'', '\"' };

  VAR
    rd := TextRd.New(cmd);
    token : TEXT;
    pgm   : TEXT := NIL;
    args  : TextSeq.T := NIL;
    done  : BOOLEAN;
    ret   : INTEGER := 0;
    childWr, 
    childRd : Pipe.T := NIL;
    stdin, stdout, stderr: File.T;
    child: Process.T;
    jobid: INTEGER := 1;
    cjobid: INTEGER := 1;
    openFiles: IntRefTbl.T := NEW(IntRefTbl.Default).init();
    processes: RefSeq.T := NEW(RefSeq.T).init();
    c: CHAR;

  PROCEDURE WaitForAll() : INTEGER =
    VAR p: Process.T; ret: INTEGER;
    BEGIN
      WHILE processes.size() > 0 DO
        p := NARROW(processes.remlo(), Process.T);
        ret := Process.Wait(p);
        CloseFiles(cjobid);
        INC(cjobid);
      END;
      RETURN ret;
    END WaitForAll;

  PROCEDURE RememberFiles(stdin: File.T; stdout: File.T; stderr: File.T;
                          pipewr: File.T) =
    BEGIN
      IF stdin  # NIL THEN EVAL openFiles.put(STDINC  + jobid, stdin) END;
      IF stdout # NIL THEN EVAL openFiles.put(STDOUTC + jobid, stdout) END;
      IF stderr # NIL THEN EVAL openFiles.put(STDERRC + jobid, stderr) END;
      IF pipewr # NIL THEN EVAL openFiles.put(PIPEWR  + jobid, pipewr) END;
    END RememberFiles;

  PROCEDURE CloseFiles(jobid: INTEGER) =
    VAR ref: REFANY;
    BEGIN
      IF openFiles.get(PIPEWR + jobid, ref) THEN
        TRY
          WITH f = NARROW(ref, File.T) DO f.close(); END;
        EXCEPT
          OSError.E => (* skip *)
        END;
      END;
      IF openFiles.get(STDINC  + jobid, ref) THEN
        TRY
          WITH f = NARROW(ref, File.T) DO f.close(); END;
        EXCEPT
          OSError.E => (* skip *)
        END;
      END;
      IF openFiles.get(STDOUTC + jobid, ref) THEN
        TRY
          WITH f = NARROW(ref, File.T) DO f.close(); END;
        EXCEPT
          OSError.E => (* skip *)
        END;
      END;
      IF openFiles.get(STDERRC + jobid, ref) THEN
        TRY
          WITH f = NARROW(ref, File.T) DO f.close(); END;
        EXCEPT
          OSError.E => (* skip *)
        END;
      END;
    END CloseFiles;

  BEGIN
    MsgX.D(msgif, "System.ExecuteList(" & cmd & ")");
    TRY
      WHILE NOT Rd.EOF(rd) DO
        pgm := TextReadingUtils.GetToken(rd, terminate := IDEND,
                                         unget := TRUE);
        args := NEW(TextSeq.T).init(10);
        done := FALSE;
        WHILE NOT done AND NOT Rd.EOF(rd) DO
          c := RdExtras.Skip(rd, unget := TRUE);
          IF c IN OPS THEN
            token := TextReadingUtils.GetToken(rd, terminate := OPEND,
                                               unget := TRUE);
            IF c # '1' AND c # '2' THEN (* FIXME: use a real scanner *)
              IF NOT Text.Equal(token, "<") AND
                 NOT Text.Equal(token, ">") AND
                 NOT Text.Equal(token, "1>") AND
                 NOT Text.Equal(token, "2>") AND
                 NOT Text.Equal(token, "&>") AND
                 NOT Text.Equal(token, ">>") AND
                 NOT Text.Equal(token, "1>>") AND
                 NOT Text.Equal(token, "2>>") AND
                 NOT Text.Equal(token, "&>>") AND
                 NOT Text.Equal(token, ";") AND
                 NOT Text.Equal(token, "|") AND
                 NOT Text.Equal(token, "||") AND
                 NOT Text.Equal(token, "&&") THEN
                RAISE ExecuteError("operator syntax error: " & token);
              END;
            END;
          ELSIF c IN STRDELIM THEN
            token := TextReadingUtils.GetString(rd);
          ELSE
            token := TextReadingUtils.GetToken(rd, terminate := IDEND,
                                               unget := TRUE);
          END;
          (* MsgX.D(msgif, " token = " & token); *)
          IF Text.Equal(token, ";") THEN
            IF childRd = NIL THEN
              ret := Exec(pgm, args, env, msgif, wd);
            ELSE
              child := ExecNW(stdin, stdout, stderr,
                              pgm, args, env, msgif, wd, pstdin := childRd);
              IF child  # NIL THEN processes.addhi(child) END;
              RememberFiles(stdin, stdout, stderr, NIL);
              IF processes.size() > 0 THEN
                ret := WaitForAll();
              END;
              INC(jobid);
              childRd := NIL;
            END;
            done := TRUE;
          ELSIF Text.Equal(token, "&&") THEN
            IF childRd = NIL THEN
              ret := Exec(pgm, args, env, msgif, wd);
            ELSE
              child := ExecNW(stdin, stdout, stderr,
                              pgm, args, env, msgif, wd, pstdin := childRd);
              IF child  # NIL THEN processes.addhi(child) END;
              RememberFiles(stdin, stdout, stderr, NIL);
              IF processes.size() > 0 THEN
                ret := WaitForAll();
              END;
              INC(jobid);
              childRd := NIL;
            END;
            IF ret # 0 THEN
              RETURN ret;
            END;
            done := TRUE;
          ELSIF Text.Equal(token, "||") THEN
            IF childRd = NIL THEN
              ret := Exec(pgm, args, env, msgif, wd);
            ELSE
              child := ExecNW(stdin, stdout, stderr,
                              pgm, args, env, msgif, wd, pstdin := childRd);
              IF child  # NIL THEN processes.addhi(child) END;
              RememberFiles(stdin, stdout, stderr, NIL);
              IF processes.size() > 0 THEN
                ret := WaitForAll();
              END;
              INC(jobid);
              childRd := NIL;
            END;
            IF ret = 0 THEN
              RETURN ret;
            END;
            done := TRUE;
          ELSIF Text.Equal(token, "|") THEN
            VAR
              lastChildRd := childRd;
            BEGIN
              TRY
                Pipe.Open(hr := childRd, hw := childWr);
              EXCEPT
                OSError.E(l) =>
                RAISE ExecuteError("pipe creation error (" & 
                      AtomListToText(l) & ")"); 
              END;
              child := ExecNW(stdin, stdout, stderr,
                              pgm, args, env, msgif, wd, 
                              pstdin := lastChildRd,
                              pstdout := childWr);
            END;
            IF child  # NIL THEN processes.addhi(child) END;
            RememberFiles(stdin, stdout, stderr, childWr);
            done := TRUE;
            INC(jobid);
          ELSE
            args.addhi(token);
          END;
        END;
      END;
    EXCEPT
      Rd.Failure,
      Rd.EndOfFile => (* skip *)
    END;
    IF NOT done AND pgm # NIL AND args # NIL THEN
      IF childRd = NIL THEN
        ret := Exec(pgm, args, env, msgif, wd);
      ELSE
        child := ExecNW(stdin, stdout, stderr,
                        pgm, args, env, msgif, wd, pstdin := childRd);
        IF child  # NIL THEN processes.addhi(child) END;
        RememberFiles(stdin, stdout, stderr, NIL);
        INC(jobid);
      END;
    END;
    IF processes.size() > 0 THEN
      ret := WaitForAll();
    END;
    RETURN ret;
  END ExecuteList;

(*---------------------------------------------------------------------------*)
PROCEDURE SplitCmd(cmd : TEXT; VAR prog : TEXT;
                               VAR pargs : REF ARRAY OF TEXT) 
  RAISES {Thread.Alerted} =
  VAR
    args   := NEW(TextSeq.T).init(10);
  BEGIN
    TRY
      WITH tmpRd = TextRd.New(cmd) DO
        prog := TextReadingUtils.GetToken(tmpRd); 
        WHILE NOT Rd.EOF(tmpRd) DO
          args.addhi(TextReadingUtils.GetTokenOrString(tmpRd));
        END;
      END;
    EXCEPT
      Rd.Failure, Rd.EndOfFile => (* skip *)
    END;
    pargs := NEW(REF ARRAY OF TEXT, args.size());
    FOR i := 0 TO args.size() - 1 DO
      pargs^[i] := OSSpecials.QuotedProcessArgument(args.get(i));
    END;
  END SplitCmd;

(*---------------------------------------------------------------------------*)
PROCEDURE RdExecute(cmd : TEXT; VAR rd : Rd.T; wd : TEXT := NIL;
                    env : ProcessEnv.T := NIL; 
                    msgif : MsgIF.T := NIL) : Process.T
  RAISES {ExecuteError, Thread.Alerted} =
  VAR
    prog   : TEXT;
    pargs  : REF ARRAY OF TEXT;
    childWr, 
    selfRd : Pipe.T;
    pid    : Process.T;
    senv  := ProcessEnv.SystemRepr(env);
    stdinParent, stdoutParent, stderrParent : File.T;
  BEGIN
    IF wd = NIL THEN wd := "." END;
    MsgX.D(msgif, "RdExecute(" & cmd & ")(" & wd & ")");

    Process.GetStandardFileHandles(stdinParent, stdoutParent, stderrParent);
    SplitCmd(cmd, prog, pargs);
    TRY
      Pipe.Open(hr := selfRd, hw := childWr);
      pid := Process.Create(prog, pargs^, senv, wd, stdinParent, 
                            childWr, childWr);
    EXCEPT
      OSError.E(l) => 
      VAR t := AtomListToText(l);
      BEGIN
        IF Text.Equal(t, "errno=2") THEN
          RAISE ExecuteError("execution of: '" & cmd & "'  in wd: " & wd &
                "\n ***  process creation error. (executable not found)");
        END;
        RAISE ExecuteError("process/pipe creation error. (" & t & ")");
      END;
    END;

    TRY
      childWr.close();
    EXCEPT
      OSError.E => (* skip *)
    END;

    TRY
      rd := NEW(FileRd.T).init(selfRd);
    EXCEPT
      OSError.E(l) => 
      RAISE ExecuteError("pipe read error. (" & AtomListToText(l) & ")"); 
    END;
    RETURN pid;
  END RdExecute;  

(*---------------------------------------------------------------------------*)
PROCEDURE PipeTo(cmd : TEXT; VAR wr : Wr.T; wd : TEXT := NIL;
                 env : ProcessEnv.T := NIL; 
                 msgif : MsgIF.T := NIL) : Process.T
  RAISES {ExecuteError, Thread.Alerted} =
  VAR
    prog   : TEXT;
    pargs  : REF ARRAY OF TEXT;
    childRd, 
    selfWr : Pipe.T;
    pid    : Process.T;
    senv  := ProcessEnv.SystemRepr(env);
    stdinParent, stdoutParent, stderrParent : File.T;
  BEGIN
    IF wd = NIL THEN wd := "." END;
    MsgX.D(msgif, "PipeTo(" & cmd & ")(" & wd & ")");

    Process.GetStandardFileHandles(stdinParent, stdoutParent, stderrParent);
    SplitCmd(cmd, prog, pargs);
    TRY
      Pipe.Open(hr := childRd, hw := selfWr);
      pid := Process.Create(prog, pargs^, senv, wd, childRd, 
                            stdoutParent, stderrParent);
    EXCEPT
      OSError.E(l) => 
      VAR t := AtomListToText(l);
      BEGIN
        IF Text.Equal(t, "errno=2") THEN
          RAISE ExecuteError("execution of: '" & cmd & "'  in wd: " & wd &
                "\n ***  process creation error. (executable not found)");
        END;
        RAISE ExecuteError("process/pipe creation error. (" & t & ")");
      END;
    END;

    TRY
      childRd.close();
    EXCEPT
      OSError.E => (* skip *)
    END;

    TRY
      wr := NEW(FileWr.T).init(selfWr);
    EXCEPT
      OSError.E(l) => 
      RAISE ExecuteError("pipe write error. (" & AtomListToText(l) & ")"); 
    END;
    RETURN pid;
  END PipeTo;   

(*---------------------------------------------------------------------------*)
PROCEDURE Filter(cmd : TEXT; VAR rd : Rd.T; VAR wr : Wr.T; wd : TEXT := NIL;
                 env : ProcessEnv.T := NIL; msgif : MsgIF.T := NIL) : Process.T
  RAISES {ExecuteError, Thread.Alerted} =
  VAR
    prog   : TEXT;
    pargs  : REF ARRAY OF TEXT;
    childRd,
    childWr, 
    selfRd ,
    selfWr : Pipe.T;
    pid    : Process.T;
    senv  := ProcessEnv.SystemRepr(env);
    stdinParent, stdoutParent, stderrParent : File.T;
  BEGIN
    IF wd = NIL THEN wd := "." END;
    MsgX.D(msgif, "Filter(" & cmd & ")(" & wd & ")");
    
    Process.GetStandardFileHandles(stdinParent, stdoutParent, stderrParent);
    SplitCmd(cmd, prog, pargs);
    TRY
      Pipe.Open(hr := selfRd, hw := childWr);
      Pipe.Open(hr := childRd, hw := selfWr);
      pid := Process.Create(prog, pargs^, senv, wd, childRd, childWr, 
                            stderrParent);
    EXCEPT
      OSError.E(l) => 
      VAR t := AtomListToText(l);
      BEGIN
        IF Text.Equal(t, "errno=2") THEN
          RAISE ExecuteError("execution of: '" & cmd & "'  in wd: " & wd &
                "\n ***  process creation error. (executable not found)");
        END;
        RAISE ExecuteError("process/pipe creation error. (" & t & ")");
      END;
    END;
    
    TRY
      childRd.close();
      childWr.close();
    EXCEPT
      OSError.E => (* skip *)
    END;
    TRY
      wr := NEW(FileWr.T).init(selfWr);
      rd := NEW(FileRd.T).init(selfRd);
    EXCEPT
      OSError.E(l) => 
      RAISE ExecuteError("pipe write error. (" & AtomListToText(l) & ")"); 
    END;
    RETURN pid;
  END Filter;
  
(*---------------------------------------------------------------------------*)
BEGIN
END System.
