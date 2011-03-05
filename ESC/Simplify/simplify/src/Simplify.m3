(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed May 15 23:21:40 PDT 2002 by saxe                     *)
(*      modified on Mon Nov  4 17:35:54 PST 1996 by detlefs                  *)

(* In a loop, reads an S-expression from standard input, interprets that as a
   first-order formula, and determines whether it is valid; if it is not valid,
   prints out a conjunction of literals consistent with its negation. *)

MODULE Simplify EXPORTS Main;

IMPORT Prover, PredSx, Axioms, Html;
IMPORT CGITrans;
IMPORT RefList, Wr, Rd, RdClass, Sx, Params, FileRd, Process, OSError,
       Text, Bundle, TextRd, Env, Pathname, Fmt, Atom, RdCopy, 
       FileWr, FmtTime, Date, Time, Trit;
(* For Dill-Burch reader... *)
IMPORT RefSeq, Lex, FloatMode;
IMPORT RTCollectorSRC;

FROM Stdio IMPORT stdin, stdout, stderr;

IMPORT Thread;
<*FATAL Wr.Failure, Rd.Failure, Thread.Alerted, Sx.PrintError*>

PROCEDURE Usage(i: INTEGER) =
  BEGIN
    Wr.PutText(stdout,
               "Correct usage for Simplify is:\n" &
               "   Simplify [-print] [-ax file] [-nosc] [-labelsonly] [-noprune]\n" &
               "       [-noplunge] [-help] [-version] [filename]\n");
    Wr.Flush(stdout);
    Process.Exit(i)
  END Usage;

CONST VersionString = "1.5.7, 15 April 2002";

VAR
  fromFile := FALSE;
  fn: TEXT;
  in: Rd.T := stdin;
  printPred := FALSE;
  axiomSet := "def.ax";
  nosc := FALSE;
  noprune := FALSE;
  filenames := 0;
  nn := 0;
  bgPushSym := Atom.FromText("BG_PUSH");
  bgPopSym := Atom.FromText("BG_POP");
  lemmaSym := Atom.FromText("LEMMA");
  promptOnSym := Atom.FromText("PROMPT_ON");
  promptOffSym := Atom.FromText("PROMPT_OFF");
  promptOn := TRUE;
  syn := Sx.CopySyntax();
  html := FALSE;
  htmlLogFile: Wr.T;
  htmlInput: TEXT;
  htmlInputUsed: INTEGER;
  htmlInputLen: CARDINAL;

CONST
  HtmlLogFilename = "/udir/detlefs/Simplify.log";

(* Temp hack for reading dill-burch problems *)
TYPE
  HashConsRM = Sx.ReadMacro OBJECT
    map: RefSeq.T;
   OVERRIDES
    read := HashConsRMRead;
  END (* OBJECT *);

PROCEDURE DillBurchSyntax(): Sx.Syntax =
  VAR res := Sx.CopySyntax(); BEGIN
    Sx.SetReadMacro(res, '#', NEW(HashConsRM, map := NEW(RefSeq.T).init()));
    Sx.SetReadMacro(res, '.', NEW(Sx.ReadMacro, read := DotRead));
    RETURN res
  END DillBurchSyntax;

PROCEDURE DotRead(<*UNUSED*> self: Sx.ReadMacro;
                  rd: Rd.T; s: Sx.Syntax): RefList.T
    RAISES { Sx.ReadError, Thread.Alerted } =
  BEGIN
    TRY RETURN Sx.Read(rd, s) EXCEPT Rd.EndOfFile =>
      RAISE Sx.ReadError("End of file")
    END (* TRY *)
  END DotRead;

PROCEDURE HashConsRMRead(self: HashConsRM; rd: Rd.T; s: Sx.Syntax): RefList.T
    RAISES { Sx.ReadError, Thread.Alerted } =
  BEGIN
    TRY
      VAR i: INTEGER := Lex.Int(rd); c := Rd.GetChar(rd); BEGIN
        CASE c OF
        | '=' =>
            IF i-1 # self.map.size() THEN
              RAISE Sx.ReadError("Bad #")
            END (* IF *);
            self.map.addhi(NIL);
            VAR res := Sx.Read(rd, s); BEGIN
              self.map.put(i-1, res);
              RETURN RefList.List1(res)
            END (* BEGIN *)
        | '#' =>
            IF i > self.map.size() THEN
              RAISE Sx.ReadError("Bad #")
            END (* IF *);
            RETURN RefList.List1(self.map.get(i-1))
        ELSE
            RAISE Sx.ReadError("Bad #")
        END (* CASE *)
      END (* BEGIN *)
    EXCEPT
    | Lex.Error, FloatMode.Trap, Rd.EndOfFile =>
        RAISE Sx.ReadError("Bad #")
    END (* TRY *)
  END HashConsRMRead;

PROCEDURE PushBG(conjs: RefList.T) RAISES { Prover.Error } =
  <*FATAL Prover.Timeout*>
  BEGIN
    WHILE conjs # NIL DO
      VAR sx := conjs.head; BEGIN
        Prover.Assert(sx, shred := FALSE);
        IF fromFile AND printPred THEN
        END (* IF *)
      END (* BEGIN *);
      conjs := conjs.tail
    END (* WHILE *);
    Prover.UnitConsequences()
  END PushBG;

TYPE
  ProveResList = OBJECT
    value: Prover.ProveRes;
    next: ProveResList := NIL;
  END (* OBJECT *);

PROCEDURE ReverseD(l: ProveResList;): ProveResList =
  VAR cur := l; ans: ProveResList := NIL; next: ProveResList; BEGIN
    WHILE cur #NIL DO
      next := cur.next;
      cur.next := ans;
      ans := cur;
      cur := next;
    END;
    RETURN ans;
  END ReverseD;

PROCEDURE SaveRes(cl: SavingResClosure; res: Prover.ProveRes) =
  BEGIN
    cl.stash := NEW(ProveResList,
                    value := res,
                    next := cl.stash)
  END SaveRes;

TYPE SavingResClosure =
  Prover.ResClosure OBJECT
    stash: ProveResList := NIL;
  OVERRIDES
    apply := SaveRes;
  END;

PROCEDURE PrintRes(res: Prover.ProveRes) =
  BEGIN
    CASE res.kind OF
    | Prover.ResKind.Counterexample =>
        IF Prover.oldOutput THEN
          OldPrintRes(res);
          RETURN
        END;
        Wr.PutText(stdout, "Counterexample:\n");
    | Prover.ResKind.XKillTime =>
        Wr.PutText(stdout,
          "Exceeded PROVER_KILL_TIME -- " &
          "discontinuing search for counterexamples.\n");
    | Prover.ResKind.XKillIter =>
        Wr.PutText(stdout,
          "Exceeded PROVER_KILL_ITER -- " &
          "discontinuing search for counterexamples.\n");
    | Prover.ResKind.CCLimit =>
        Wr.PutText(stdout,
          "Reached PROVER_CC_LIMIT -- " &
           "will not search for additional counterexamples.\n");
    | Prover.ResKind.XSubKillTime =>
        Wr.PutText(stdout,
          "Exceeded PROVER_SUBGOAL_KILL_TIME -- " &
           "abandoning counterexample search for current subgoal.\n");
    | Prover.ResKind.XSubKillIter =>
        Wr.PutText(stdout,
          "Exceeded PROVER_SUBGOAL_KILL_ITER -- " &
           "abandoning counterexample search for current subgoal.\n");
    END;
    Wr.Flush(stdout);
    IF res.lbls # NIL AND res.kind # Prover.ResKind.CCLimit THEN
      Wr.PutText(stdout, "  labels: (");
      VAR lbls := res.lbls; BEGIN
        WHILE lbls # NIL DO
          Wr.PutText(stdout, "|" & Atom.ToText(lbls.head) & "|");
          lbls := lbls.tail;
          IF lbls # NIL THEN
            Wr.PutText(stdout, " ")
          END (* IF *)
        END (* WHILE *)
      END; (* BEGIN *)
      Wr.PutText(stdout, ")\n");
      Wr.Flush(stdout)
    END;
    IF res.context # NIL THEN
      Wr.PutText(stdout, "  context:\n"); 
      VAR resl: RefList.T := res.context; BEGIN
        Wr.PutText(stdout, "    (AND\n");
        WHILE resl # NIL DO
          Wr.PutText(stdout, "      ");
          Sx.Print(stdout, resl.head);
          Wr.PutText(stdout, "\n");
          resl := resl.tail
        END; (* WHILE *)
        Wr.PutText(stdout, "    )\n")
      END (* BEGIN *)
    END; (* IF *)
    Wr.PutText(stdout, "\n");
    Wr.Flush(stdout)
  END PrintRes;

PROCEDURE OldPrintRes(res: Prover.ProveRes) =
  BEGIN
    Wr.PutText(stdout, "Counterexample labels (");
    VAR lbls := res.lbls; BEGIN
      WHILE lbls # NIL DO
        Wr.PutText(stdout, "|" & Atom.ToText(lbls.head) & "|");
        lbls := lbls.tail;
        IF lbls # NIL THEN
          Wr.PutText(stdout, " ")
        END (* IF *)
      END (* WHILE *)
    END; (* BEGIN *)
    Wr.Flush(stdout);
    Wr.PutText(stdout, ")\n");
    IF res.context # PredSx.trueSym AND res.context # NIL THEN
      Wr.PutText(stdout, "counterexample context\n"); 
      VAR resl: RefList.T := res.context; BEGIN
        Wr.PutText(stdout, "  (AND\n");
        WHILE resl # NIL DO
          Wr.PutText(stdout, "    ");
          Sx.Print(stdout, resl.head);
          Wr.PutText(stdout, "\n");
          resl := resl.tail
        END; (* WHILE *)
        Wr.PutText(stdout, "  )\n")
      END (* BEGIN *)
    END; (* IF *)
    Wr.PutText(stdout, "\n");
    Wr.Flush(stdout)
  END OldPrintRes;


PROCEDURE ClPrintRes(<* UNUSED *> cl: PrintingResClosure;
                   res: Prover.ProveRes) =
  BEGIN
    PrintRes(res);
  END ClPrintRes;

TYPE PrintingResClosure =
  Prover.ResClosure OBJECT OVERRIDES
    apply := ClPrintRes
  END;

BEGIN
  RTCollectorSRC.incremental := FALSE;
  RTCollectorSRC.generational := FALSE;
  TRY
    Prover.Init()
  EXCEPT
  | Prover.Error(msg) =>
      Wr.PutText(stderr, msg & "\n");
      Wr.Flush(stderr);
      Process.Exit(1)
  END (* TRY *);
  VAR i := 1; BEGIN
    WHILE i < Params.Count DO
      VAR opti := Params.Get(i); BEGIN
        IF Text.Equal(opti, "-print") THEN
          printPred := TRUE;
        ELSIF Text.Equal(opti, "-ax") THEN
          INC(i);
          IF i = Params.Count THEN
            Usage(1)
          END (* IF *);
          axiomSet := Params.Get(i) & ".ax"
        ELSIF Text.Equal(opti, "-nosc") THEN
          nosc := TRUE;
          Prover.showCC := FALSE;
          Prover.ccLimitGlobal := 1
        ELSIF Text.Equal(opti, "-labelsonly") THEN
          Prover.showCC := FALSE;
        ELSIF Text.Equal(opti, "-noprune") THEN
          noprune := TRUE
        ELSIF Text.Equal(opti, "-noplunge") THEN
          Prover.noPlunge := TRUE
        ELSIF Text.Equal(opti, "-version") THEN
          Wr.PutText(stdout, "Simplify version " & VersionString & "\n");
          Wr.Flush(stdout);
          Process.Exit(0)
        ELSIF Text.Equal(opti, "-help") THEN
          Usage(0)
        ELSIF Text.Equal(opti, "-dbsyn") THEN
          syn := DillBurchSyntax()
        ELSIF Text.Equal(opti, "-htmldebug0") THEN
          Wr.PutText(stdout, Bundle.Get(Html.Get(), "html-output1"));
          EVAL RdCopy.ToWriter(stdin, stdout);
          Wr.Flush(stdout);
          Process.Exit(0)
        ELSIF Text.Equal(opti, "-htmldebug1") THEN
          Wr.PutText(stdout, Bundle.Get(Html.Get(), "html-output1"));
          TRY
            VAR tbl := CGITrans.P(CGITrans.PostMeth, CGITrans.DefCT);
                iter := tbl.iterate(); 
                name, value: TEXT;
            BEGIN
              WHILE iter.next(name, value) DO
                Wr.PutText(stdout, name & "=" & value & "<p>\n\n")
              END (* WHILE *)
            END (* BEGIN *)
          EXCEPT
          | CGITrans.Error(msg) =>
              Wr.PutText(stdout, "CGTrans.Error: " & msg & "\n")
          END (* TRY *);
          Wr.Flush(stdout);
          Process.Exit(0);
        ELSIF Text.Equal(opti, "-html") THEN
          html := TRUE;
          fromFile := TRUE;
          htmlLogFile := FileWr.OpenAppend(HtmlLogFilename); <*NOWARN*>
          TRY
            VAR tbl := CGITrans.P(CGITrans.PostMeth, CGITrans.DefCT);
                b1, b2, b3: BOOLEAN;
                inputType, ident: TEXT;
            BEGIN
              b1 := tbl.get("input-type", inputType);
              IF NOT b1 THEN
                Wr.PutText(stdout, "Input didn't specify 'input-type'.");
                Wr.Flush(stdout);
                Process.Exit(0)
              END (* IF *);
              b2 := tbl.get("ident", ident);
              IF NOT b2 THEN
                Wr.PutText(stdout, "Input didn't specify 'ident'.");
                Wr.Flush(stdout);
                Process.Exit(0)
              END (* IF *);
              b3 := tbl.get("input", htmlInput);
              IF NOT b3 THEN
                Wr.PutText(stdout, "Input didn't specify 'input'.");
                Wr.Flush(stdout);
                Process.Exit(0)
              END (* IF *);
              htmlInputLen := Text.Length(htmlInput);
              htmlInputUsed := 0;
              Wr.PutText(htmlLogFile, "==================================\n");
              Wr.PutText(htmlLogFile,
                         FmtTime.DateLong(Date.FromTime(Time.Now())) & "\n");
              Wr.PutText(htmlLogFile, ident & "\n");
              Wr.PutText(htmlLogFile, inputType & "\n");
              Wr.PutText(htmlLogFile, htmlInput & "\n");
              Wr.Flush(htmlLogFile);
              IF Text.Equal(inputType, "Comment") THEN
                Wr.Close(htmlLogFile);
                Wr.PutText(stdout, Bundle.Get(Html.Get(), "html-output2"));
                Wr.Flush(stdout);
                Process.Exit(0)
              ELSE
                <*ASSERT Text.Equal(inputType, "Theorem") *>
                Wr.PutText(stdout, Bundle.Get(Html.Get(), "html-output1"));
                Wr.Flush(stdout);
                in := TextRd.New(htmlInput)
              END (* IF *)
            END (* BEGIN *)
          EXCEPT
          | CGITrans.Error(msg) =>
              Wr.PutText(stdout, "CGITrans.Error: " & msg & "\n");
              Wr.Flush(stdout);
              Process.Exit(0)
          END (* TRY *)
        ELSE
          IF filenames = 0 THEN
            fn := opti;
            TRY
              in := FileRd.Open(fn);
            EXCEPT
            | OSError.E =>
                Wr.PutText(stdout, "Unable to open file '" & fn & "'.\n");
                Wr.Flush(stdout);
                Process.Exit(1)
            END (* TRY *);
            fromFile := TRUE;
            INC(filenames)
          ELSE
            Usage(1)
          END (* IF *)
        END (* IF *)
      END (* BEGIN *);
      INC(i)
    END (* WHILE *)
  END (* BEGIN *);
  VAR dir := Env.Get("AXIOMDIR"); BEGIN
    TRY
      IF dir = NIL THEN
        VAR axtxt := Bundle.Get(Axioms.Get(), axiomSet); BEGIN
          IF axtxt # NIL THEN
            Prover.AddAxioms(TextRd.New(axtxt))
          ELSE
            Wr.PutText(stdout, "Unable to open axiom file '" &
              axiomSet & "'.\n");
            Wr.Flush(stdout);
            Process.Exit(1)
          END (* IF *)
        END
      ELSE
        VAR pn := Pathname.Join(dir, axiomSet, NIL); BEGIN
          TRY Prover.AddAxioms(FileRd.Open(pn)) EXCEPT
          | OSError.E =>
              Wr.PutText(stdout, "Unable to open file '" & pn & "'.\n");
              Wr.Flush(stdout);
              Process.Exit(1)
          END (* TRY *)
        END (* BEGIN *)
      END (* IF *)
    EXCEPT
    | Prover.Error(msg) =>
        Wr.PutText(stdout, "Error in axioms: " & msg & ".\n");
        Wr.Flush(stdout);
        Process.Exit(1)
    END (* TRY *)
  END (* IF *);
  TRY
    LOOP
      IF NOT fromFile THEN
        IF promptOn THEN
          Wr.PutText(stdout, ">\t");
        END;
      Wr.Flush(stdout);
      END (* IF *);
      IF Prover.internDebug THEN
        Wr.PutText(stdout,
          "Simplify: Calling Sx.Read to get next command) ...\n");
        Wr.Flush(stdout)
      END (* IF *);
      VAR sx := Sx.Read(in, syn);
      BEGIN
        IF Prover.internDebug THEN
          Wr.PutText(stdout,
            "Simplify: ... returned from  Sx.Read\n");
          Wr.Flush(stdout)
        END;
        IF html THEN
          Wr.PutText(stdout, "<h2>Predicate</h2><p>\n<pre>\n");
          WHILE htmlInputUsed < in.cur DO
            Wr.PutChar(stdout, Text.GetChar(htmlInput, htmlInputUsed));
            INC(htmlInputUsed)
          END (* WHILE *);
          Wr.PutText(stdout,
                     "</pre><p>\n<h2>is</h2><p>\n<pre>\n");
          Wr.Flush(stdout);
        END (* IF *);
        TRY
          IF ISTYPE(sx, RefList.T) AND
            NARROW(sx, RefList.T).head = promptOnSym THEN
            promptOn := TRUE
          ELSIF ISTYPE(sx, RefList.T) AND
            NARROW(sx, RefList.T).head = promptOffSym THEN
            promptOn := FALSE
          ELSIF ISTYPE(sx, RefList.T) AND
            NARROW(sx, RefList.T).head = bgPushSym THEN
            VAR rl: RefList.T := sx; conjs := rl.tail; BEGIN
              Prover.Push();
              IF fromFile AND printPred THEN
                Wr.PutText(stdout, "Pushing background predicate\n\n");
                Sx.Print(stdout, conjs);
                Wr.PutText(stdout, "\n\n");
                Wr.Flush(stdout)
              ELSIF html THEN
                Wr.PutText(stdout, "<pre>Pushed.\n\n</pre>");
                Wr.Flush(stdout);
              END (* IF *);
              PushBG(conjs)
            END (* BEGIN *)
          ELSIF ISTYPE(sx, RefList.T) AND
            NARROW(sx, RefList.T).head = bgPopSym THEN
            Prover.Pop();
            IF fromFile AND printPred THEN
              Wr.PutText(stdout, "Popped background predicate.\n\n");
              Wr.Flush(stdout)
            END (* IF *)
          ELSIF ISTYPE(sx, RefList.T) AND
            (NARROW(sx, RefList.T).head = PredSx.defPredSym OR
             NARROW(sx, RefList.T).head = PredSx.defPredMapSym) THEN
            VAR rl: RefList.T := sx; BEGIN
              Prover.PredDef(rl);
              IF fromFile AND printPred THEN
                VAR sig: RefList.T := rl.tail.head; BEGIN
                  Wr.PutText(stdout, "Defined predicate " &
                    Atom.ToText(sig.head) & ".\n\n");
                  Wr.Flush(stdout)
                END (* BEGIN *)
              END (* IF *)
            END (* IF *)
          ELSE
            VAR isLemma := ISTYPE(sx, RefList.T) AND
                           NARROW(sx, RefList.T).head = lemmaSym;
                lemma: Sx.T;
                ccLimit: INTEGER;
                outcome: Trit.T;
            BEGIN
              INC(nn);
              IF fromFile AND printPred THEN
                Wr.PutText(stdout, "Predicate\n\n");
                Sx.Print(stdout, sx);
                Wr.PutText(stdout, "\n\nis ");
                Wr.Flush(stdout)
              END (* IF *);
              IF isLemma THEN
                VAR sxRL: RefList.T := sx; BEGIN
                  IF sxRL.tail.tail = NIL THEN
                    sx := sxRL.tail.head; lemma := sx
                  ELSE
                    sx := RefList.Cons(
                              PredSx.proofSym,
                              RefList.Cons(PredSx.trueSym, sxRL.tail));
                    lemma := RefList.Nth(sxRL, RefList.Length(sxRL)-1)
                  END (* IF *);
                  sxRL := NIL
                END; (* BEGIN *)
                ccLimit := 1
              ELSE
                ccLimit := Prover.ccLimitGlobal
              END (* IF *);
              VAR cl: Prover.ResClosure; BEGIN
                IF nosc THEN
                  cl := NIL;
                ELSIF noprune OR NOT Prover.showCC THEN
                  cl := NEW(PrintingResClosure)
                ELSE           
                  cl := NEW(SavingResClosure)
                END;
                outcome := Prover.Prove(
                                  sx,
                                  cl := cl,
                                  ccLimit := ccLimit,
                                  shred := NOT isLemma);
                TYPECASE cl OF
                | NULL, PrintingResClosure => (* skip *)
                | SavingResClosure (scl) => 
                  VAR stash := ReverseD(scl.stash); BEGIN
                    WHILE stash # NIL DO
                      PrintRes(Prover.Prune(stash.value));
                      stash := stash.next;
                    END (* WHILE *)
                  END (* BEGIN *)
                ELSE
                  <*ASSERT FALSE*>
                END; (* TYPECASE *)
              END; (* BEGIN *)
              IF outcome = Trit.True THEN
                Wr.PutText(stdout, Fmt.Int(nn) & ": Valid.");
                IF isLemma THEN
                  PushBG(RefList.List1(lemma));
                  Wr.PutText(stdout, "  (Added to background predicate).")
                END (* IF *);
                Wr.PutText(stdout, "\n\n"); Wr.Flush(stdout);
              ELSIF outcome = Trit.Unknown THEN
                Wr.PutText(stdout, Fmt.Int(nn) & ": Unknown.\n\n");
                Wr.Flush(stdout);
              ELSE (*  outcome = Trit.False *)
                Wr.PutText(stdout, Fmt.Int(nn) & ": Invalid.\n\n");
                Wr.Flush(stdout);
              END (* IF *);
            END (* BEGIN *)
          END (* IF *)
        EXCEPT
        | Prover.Error(msg) =>
            Wr.PutText(stdout, "Bad input: " & msg & "\n");
            Wr.Flush(stdout)
        END (* TRY *)
      END (* BEGIN *)
    END (* LOOP *)
  EXCEPT
  | Rd.EndOfFile =>
      IF html THEN Wr.Close(htmlLogFile) END (* IF *)
  | Sx.ReadError =>
      Wr.PutText(stdout, "Sx.ReadError in file.\n");
      Wr.Flush(stdout)
  END (* TRY *);
  IF html THEN Wr.PutText(stdout, "</pre>\n") END (* IF *)
END Simplify.
