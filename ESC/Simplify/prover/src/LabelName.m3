(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun May 12 01:29:32 PDT 2002 by saxe                     *)
(*      modified on Tue Aug 13 09:39:26 PDT 1996 by detlefs                  *)

(* This interface defines the types used to represent label names (see msg
   on src.sparta, detlefs 9/14/95), and the global set of asserted
   label names.
*)

MODULE LabelName;

IMPORT Enode, LNUndoRec, ContextPrivate;
IMPORT Atom, Word, FPrint, Time, Env, Fmt, Wr, Sx, Stdio, Text;
IMPORT AtomList, RefIntTbl, RefList, LNUndoRecSeq, TimeSeq, IntSeq,
       AtomSetList, AtomRefTbl;

IMPORT Satisfy; (* for "GetIters" *)
IMPORT Prover; (* for "tacticTrace" *)

IMPORT Thread;

<*FATAL Thread.Alerted, Wr.Failure, Sx.PrintError*>
REVEAL
  T = BRANDED OBJECT
    hash: Word.T;
    hashSet: BOOLEAN;
    term: BOOLEAN := FALSE;
    hasAtSign: BOOLEAN := FALSE;
    reported: BOOLEAN := FALSE;
    prefix: T := NIL;
   METHODS
    init(): T := TInit;
  END (* OBJECT *);

TYPE
  (* A T is one of the following types. *)
  Atm = T BRANDED OBJECT
    at: Atom.T;
  END (* OBJECT *);
  Quant = T BRANDED OBJECT
    t: T;
    args: RefList.T; (* OF Enode.T *)
  END (* OBJECT *);
  And = T BRANDED OBJECT
    t: T;
    i, n: CARDINAL;
  END (* OBJECT *);
  Or = T BRANDED OBJECT
    t: T;
    uniq: INTEGER;
  END (* OBJECT *);

(* Global state. *)

TYPE
  LNIntTbl = RefIntTbl.Default BRANDED OBJECT
   OVERRIDES
    keyHash := LNHash;
    keyEqual := LNEqual;
  END (* OBJECT *);

VAR
  lnSet: LNIntTbl;
  atomTab: AtomRefTbl.T;
  undoStack: LNUndoRecSeq.T;
  timeStack: TimeSeq.T;
  splitStack: IntSeq.T;
  iterStack: IntSeq.T;
  lastT: Time.T;
  lastSplits: INTEGER;
  lastIters: INTEGER;
  evLog: BOOLEAN;
  evTrace: BOOLEAN;
  houdiniLabels: BOOLEAN;

(* The "lnSet" is a table mapping "T"'s to integers.  If "lnSet" maps
   a "T" "t" to "0", then "t" is in the global set "S" of asserted
   label names.  If "lnSet" maps "t" to an integer "i" greater than 0,
   then "t" is the parent of "n" "And" nodes, "i" of which have not been
   asserted.
*)

VAR evs := 0;

PROCEDURE Init() =
  BEGIN
    lnSet := NEW(LNIntTbl).init();
    atomTab := NEW(AtomRefTbl.Default).init(100);
    undoStack := NEW(LNUndoRecSeq.T).init();
    timeStack := NEW(TimeSeq.T).init();
    splitStack := NEW(IntSeq.T).init();
    iterStack := NEW(IntSeq.T).init();
    evLog := Env.Get("PROVER_LABEL_LOG") # NIL;
    evTrace := Env.Get("PROVER_LABEL_TRACE") # NIL;
    houdiniLabels := Env.Get("PROVER_HOUDINI_LABELS") # NIL
  END Init;

PROCEDURE MkAtom(at: Atom.T): T =
  VAR res: T; resRA: REFANY; BEGIN
    IF atomTab.get(at, resRA) THEN
      res := resRA;
    ELSE
      VAR txt := Atom.ToText(at);
          atSignPos := Text.FindChar(txt, '@');
          colonPos := Text.FindChar(txt, ':'); BEGIN
        res := 
          NEW(Atm,
              term := TRUE,
              at := at,
              hasAtSign := atSignPos >= 0).init();
        EVAL atomTab.put(at, res); 
        undoStack.addhi(
           LNUndoRec.T{LNUndoRec.UndoType.MkAtom, res, anyAtSignLabels});
        IF houdiniLabels AND
            0 <= colonPos AND colonPos < atSignPos THEN
          res.prefix := MkAtom(Atom.FromText(Text.Sub(txt,0,atSignPos)))
        END
      END (* VAR *);
    END (* ELSE *);
    RETURN res
  END MkAtom;

PROCEDURE MkQuant(t: T; args: RefList.T): T =
  BEGIN RETURN NEW(Quant, term := t.term, t := t,
                   args := CopyList(args)).init()
  END MkQuant;

PROCEDURE CopyList(rl: RefList.T): RefList.T =
  BEGIN
    IF rl = NIL THEN RETURN NIL
    ELSE RETURN RefList.Cons(rl.head, CopyList(rl.tail))
    END (* IF *)
  END CopyList;
    
PROCEDURE MkAnd(t: T; i, n: CARDINAL): T =
  BEGIN RETURN NEW(And, t := t, i := i, n := n).init()
  END MkAnd;

PROCEDURE MkOr(t: T; uniq: INTEGER): T =
  BEGIN
    RETURN NEW(Or, t := t, uniq := uniq).init()
  END MkOr;

PROCEDURE Assert(t: T): BOOLEAN =
  VAR i: INTEGER; BEGIN
    IF evTrace THEN
      Wr.PutText(Stdio.stdout,
        "LabelName.Assert(" & LabelToText(t) & ")\n");
      Wr.Flush(Stdio.stdout)
    END;
    IF t.reported THEN
      IF Prover.tacticTrace THEN
        Wr.PutText(Stdio.stdout,
          "; LabelName.Assert: Detected previously-reoprted label: " &
          LabelToText(t) & "\n");
        Wr.Flush(Stdio.stdout);
      END;
      RETURN FALSE
    END;
    IF houdiniLabels THEN
      IF t.prefix # NIL THEN
        IF t.prefix.reported THEN
          IF Prover.tacticTrace THEN
            Wr.PutText(Stdio.stdout,
              "; LabelName.Assert: Detected previously-reoprted label: " &
              LabelToText(t.prefix) & "\n");
            Wr.Flush(Stdio.stdout);
          END;
          RETURN FALSE
        END
      END
    END;



    IF lnSet.get(t, i) THEN
      <*ASSERT i = 0 *>
      RETURN TRUE;
    END (* IF *);
    (* Otherwise... *)
    EVAL lnSet.put(t, 0);
    IF evLog AND (NOT Prover.inPruning) AND t.term THEN
      StartLblEntry(t)
    END (* IF *);
    IF traceLabelTrigger # NIL AND t.term AND
      Text.Equal(TermToText(t), traceLabelTrigger) THEN
      INC(traceTriggers)
    END (* IF *);
    undoStack.addhi(
      LNUndoRec.T{LNUndoRec.UndoType.Insert, t, anyAtSignLabels});
    IF t.hasAtSign THEN anyAtSignLabels := TRUE END;
    RETURN CloseTbl(t);
    END Assert;

VAR traceLabelTrigger: TEXT;

PROCEDURE StartLblEntry(t: T) =
  VAR nowT := Time.Now();
      nowSplits := ContextPrivate.GetNSplits();
      nowIters := Satisfy.GetIters();
      elapsedT := nowT - lastT;
      elapsedSplits := nowSplits - lastSplits;
      elapsedIters := nowIters - lastIters;
  BEGIN
    INC(evs);
    timeStack.addhi(nowT);
    splitStack.addhi(nowSplits);
    iterStack.addhi(nowIters);
    lastT := nowT;
    lastSplits := nowSplits;
    lastIters := nowIters;
    VAR msg := "S " & TermToText(t) & ": " &
               Fmt.LongReal(elapsedT, prec := 1) & " sec, " &
               Fmt.Int(elapsedSplits) & " splits, " &
               Fmt.Int(elapsedIters) & " iters.\n";
    BEGIN
      Wr.PutText(Stdio.stdout, ";" &
                 Fmt.Pad(msg, Text.Length(msg) + 3 * (timeStack.size()-1)));
      Wr.Flush(Stdio.stdout)
    END (* BEGIN *)
  END StartLblEntry;

PROCEDURE TermToText(t: T): TEXT =
  VAR res := Atom.ToText(TermToAtom(t)); BEGIN
    IF ISTYPE(t, Quant) THEN
      res := res & "(" & Fmt.Int(Signature(t) MOD 1000) & ")"
    END (* IF *);
    RETURN res
  END TermToText;

PROCEDURE Signature(t: T): INTEGER =
  BEGIN
    TYPECASE t OF <*NOWARN*>
    | Atm => RETURN 0
    | Quant(q) =>
        VAR res := Signature(q.t); args := q.args; BEGIN
          WHILE args # NIL DO
            VAR e: Enode.T := args.head; BEGIN
              res := res + e.id
            END (* BEGIN *);
            args := args.tail
          END (* WHILE *);
          RETURN res
        END (* BEGIN *)
    END (* TYPECASE *)
  END Signature;

PROCEDURE EndLblEntry(t: T) =
  VAR nowT := Time.Now();
      nowSplits := ContextPrivate.GetNSplits();
      nowIters := Satisfy.GetIters();
      elapsedT: Time.T;
      elapsedSplits, elapsedIters: INTEGER;
  BEGIN
    elapsedT := nowT - timeStack.remhi();
    elapsedSplits := nowSplits - splitStack.remhi();
    elapsedIters := nowIters - iterStack.remhi();
    lastT := nowT;
    lastSplits := nowSplits;
    lastIters := nowIters;
    VAR msg := "F " & TermToText(t) & ": " &
                   Fmt.LongReal(elapsedT, prec := 1) &" sec, " &
                   Fmt.Int(elapsedSplits) & " splits, " &
                   Fmt.Int(elapsedIters) & " iters.\n";
    BEGIN
      Wr.PutText(Stdio.stdout, ";" &
                 Fmt.Pad(msg, Text.Length(msg) + 3 * (timeStack.size())));
      Wr.Flush(Stdio.stdout)
    END (* BEGIN *)
  END EndLblEntry;

PROCEDURE CloseTbl(t: T): BOOLEAN =
  BEGIN
    IF t.reported THEN
      IF Prover.tacticTrace THEN
        Wr.PutText(Stdio.stdout,
          "; LabelName.CloseTbl: Detected previously-reoprted label: " &
          LabelToText(t) & "\n");
        Wr.Flush(Stdio.stdout);
      END;
      RETURN FALSE
    END;
    IF houdiniLabels THEN
      IF t.prefix # NIL THEN
        IF t.prefix.reported THEN
          IF Prover.tacticTrace THEN
            Wr.PutText(Stdio.stdout,
              "; LabelName.CloseTbl: Detected previously-reoprted label: " &
              LabelToText(t.prefix) & "\n");
            Wr.Flush(Stdio.stdout);
          END;
          RETURN FALSE
        END
      END
    END;
    TYPECASE t OF <*NOWARN*>
    | Quant(q) =>
        IF NOT ISTYPE(q.t, Atm) THEN
         RETURN Assert(q.t)
        END (* IF *)
    | Or(or) => RETURN Assert(or.t)
    | And(and) =>
        VAR ip: INTEGER; BEGIN
          IF lnSet.get(and.t, ip) THEN
            <*ASSERT ip >= 1 *>
            DEC(ip);
            EVAL lnSet.put(and.t, ip);
            undoStack.addhi(
              LNUndoRec.T{LNUndoRec.UndoType.Dec, and.t, anyAtSignLabels});
            IF ip = 0 THEN
              IF evLog AND (NOT Prover.inPruning) AND and.t.term THEN
                StartLblEntry(and.t)
              END (* IF *);
              IF and.t.hasAtSign THEN anyAtSignLabels := TRUE END;
              RETURN CloseTbl(and.t)
            END (* IF *)
          ELSE
            EVAL lnSet.put(and.t, and.n-1);
            undoStack.addhi(
              LNUndoRec.T{LNUndoRec.UndoType.Insert, and.t, anyAtSignLabels})
          END (* IF *)
        END (* BEGIN *)
    | Atm => (*SKIP*)
    END; (* TYPECASE *)
    RETURN TRUE;
  END CloseTbl;

PROCEDURE LabelToText(t: T): TEXT =
  BEGIN
    TYPECASE t OF <*NOWARN*>
    | Quant(q) =>
        RETURN "(LBL_QUANT " & LabelToText(q.t) & "(...))"
    | Or(or) => 
        RETURN "(LBL_OR " & LabelToText(or.t) & " " & Fmt.Int(or.uniq) &")"
    | And(and) =>
        RETURN "(LBL_AND " & LabelToText(and.t) & " "
        & Fmt.Int(and.i) &" "
        & Fmt.Int(and.n) &")"
    | Atm(atm) =>
        RETURN Atom.ToText(atm.at)
    END; (* TYPECASE *)
  END LabelToText;

PROCEDURE Push() =
  BEGIN
    undoStack.addhi(LNUndoRec.T{LNUndoRec.UndoType.Mark, NIL, anyAtSignLabels})
  END Push;

PROCEDURE Pop() =
  BEGIN
    LOOP
      VAR top := undoStack.remhi(); BEGIN
        CASE top.type OF
        | LNUndoRec.UndoType.Mark =>
            anyAtSignLabels := top.anyAtSignLabels;
            EXIT;
        | LNUndoRec.UndoType.MkAtom =>
            VAR atm: Atm := top.ln;
                dumRA: REFANY;
            BEGIN
              EVAL atomTab.delete(atm.at, dumRA)
            END;
        | LNUndoRec.UndoType.Insert =>
            VAR dummy: INTEGER; BEGIN
              EVAL lnSet.delete(top.ln, dummy);
              IF evLog  AND (NOT Prover.inPruning) AND
                  top.ln.term AND dummy = 0 THEN
                EndLblEntry(top.ln)
              END (* IF *);
              IF traceLabelTrigger # NIL AND top.ln.term AND
                Text.Equal(TermToText(top.ln), traceLabelTrigger) THEN
                DEC(traceTriggers)
              END (* IF *);
            END (* BEGIN *)
        | LNUndoRec.UndoType.Dec =>
            VAR i: INTEGER; b: BOOLEAN; BEGIN
              b := lnSet.get(top.ln, i); <*ASSERT b *>
              EVAL lnSet.put(top.ln, i+1)
            END (* BEGIN *)
        END (* TYPECASE *)
      END (* BEGIN *)
    END (* LOOP *)
  END Pop;

PROCEDURE Atoms(markReported: BOOLEAN := FALSE): AtomList.T =
  VAR resSet := NEW(AtomSetList.T).init();
      iter := lnSet.iterate(); r: REFANY; ln: T; i: INTEGER;
  BEGIN
    WHILE iter.next(r, i) DO
      ln := NARROW(r, T);
      IF i = 0 AND ln.term THEN
        IF markReported AND ln.hasAtSign THEN
          <* ASSERT NOT ln.reported *>
          ln.reported := TRUE;
          IF houdiniLabels THEN
            IF ln.prefix # NIL THEN
              <* ASSERT NOT ln.prefix.reported *>
              ln.prefix.reported := TRUE
            END
          END
        END;
        EVAL resSet.insert(TermToAtom(ln))
      END (* IF *)
    END (* WHILE *);
    VAR res: AtomList.T := NIL;
        iter := resSet.iterate(); at: Atom.T;
    BEGIN
      WHILE iter.next(at) DO
        res := AtomList.Cons(at, res)
      END (* WHILE *);
      RETURN res
    END (* BEGIN *)
  END Atoms;

PROCEDURE TermToAtom(ln: T): Atom.T =
  BEGIN
    <*ASSERT ln.term *>
    TYPECASE ln OF <*NOWARN*>
    | Atm(atm) => RETURN atm.at
    | Quant(q) => RETURN TermToAtom(q.t)
    END (* TYPECASE *)
  END TermToAtom;

(* ---------------------------------------------------------------------- *)

PROCEDURE LNHash(<*UNUSED*> tbl: LNIntTbl; READONLY lnRA: REFANY): Word.T =
  BEGIN RETURN Hash(lnRA) END LNHash;

PROCEDURE Hash(READONLY ln: T): Word.T =
  BEGIN
    IF NOT ln.hashSet THEN
      TYPECASE ln OF <*NOWARN*>
      | Atm(at) =>
	  ln.hash := Atom.Hash(at.at)
      | Quant(q) =>
	  ln.hash := Hash(q.t);
          VAR args := q.args; BEGIN
            WHILE args # NIL DO
              ln.hash := 
                Word.Xor(ln.hash, 
                         FPrint.Hash(Enode.FingerP(args.head)));
              args := args.tail
            END (* WHILE *)
          END (* FOR *);
      | And(and) =>
	  ln.hash := Hash(and.t);
          ln.hash := Word.Plus(Word.Rotate(ln.hash, 8), and.i);
          ln.hash := Word.Plus(Word.Rotate(ln.hash, 8), and.n);
      | Or(or) =>
	  ln.hash := Hash(or.t);
          ln.hash := Word.Xor(Word.Rotate(ln.hash, -8), or.uniq)
      END (* TYPECASE *);
      ln.hashSet := TRUE
    END (* IF *);
    RETURN ln.hash
  END Hash;

PROCEDURE LNEqual(<*UNUSED*> tbl: LNIntTbl;
                  READONLY ln1, ln2: REFANY): BOOLEAN =
  BEGIN RETURN Equal(ln1, ln2) END LNEqual;

PROCEDURE Equal(READONLY ln1, ln2: T): BOOLEAN =
  BEGIN
    IF ln1 = ln2 THEN RETURN TRUE END (* IF *);
    TYPECASE ln1 OF <*NOWARN*>
    | Atm(at1) =>
        TYPECASE ln2 OF
        | Atm(at2) => RETURN at1.at = at2.at
        ELSE RETURN FALSE
        END (* TYPECASE *)
    | Quant(q1) =>
        TYPECASE ln2 OF
        | Quant(q2) =>
            IF RefList.Length(q1.args) # RefList.Length(q2.args)
              OR NOT Equal(q1.t, q2.t) THEN
              RETURN FALSE
            ELSE
              VAR args1 := q1.args; args2 := q2.args; BEGIN
                WHILE args1 # NIL DO
                  IF args1.head # args2.head THEN
                    RETURN FALSE
                  END (* IF *);
                  args1 := args1.tail; args2 := args2.tail
                END (* IF *);
                RETURN TRUE
              END (* BEGIN *)
            END (* IF *)
        ELSE
            RETURN FALSE
        END (* TYPECASE *)
    | And(and1) =>
        TYPECASE ln2 OF
        | And(and2) =>
            RETURN
              and1.i = and2.i AND and1.n = and2.n AND 
              Equal(and1.t, and2.t)
        ELSE
            RETURN FALSE
        END (* TYPECASE *)
    | Or(or1) =>
        TYPECASE ln2 OF
        | Or(or2) =>
            RETURN or1.uniq = or2.uniq AND Equal(or1.t, or2.t)
        ELSE
            RETURN FALSE
        END (* TYPECASE *)
    END (* TYPECASE *)
  END Equal;

PROCEDURE TInit(self: T): T =
  BEGIN self.hashSet := FALSE; RETURN self END TInit;

PROCEDURE StartProof() =
  BEGIN
    lastT := Time.Now();
    lastSplits := 0;
    <*ASSERT ContextPrivate.GetNSplits() = 0 *>
    lastIters := Satisfy.GetIters();
    <*ASSERT lastIters = 0 *>
    EVAL timeStack.init();
    timeStack.addhi(lastT);
    splitStack.addhi(0);
    iterStack.addhi(0);
    evs := 0;
    VAR iter := atomTab.iterate();
        key: Atom.T; valRA: REFANY; val: T; BEGIN
      WHILE iter.next(key, valRA) DO
        val := valRA;
        val.reported := FALSE;
      END (* WHILE *);
    END (* BEGIN *);
  END StartProof;

PROCEDURE Print(wr: Wr.T; t: T) =
  BEGIN
    TYPECASE t OF <*NOWARN*>
    | Atm(at) =>
        Sx.Print(wr, at.at)
    | Quant(q) =>
        Wr.PutText(wr, "(LBLQUANT (");
        VAR args := q.args; BEGIN
          WHILE args # NIL DO
            Wr.PutText(wr, Fmt.Int(NARROW(args.head, Enode.T).id));
            args := args.tail;
            IF args # NIL THEN Wr.PutText(wr, " ") END
          END (* WHILE *);
          Wr.PutText(wr, ") ");
        END (* BEGIN *);
        Print(wr, q.t);
        Wr.PutText(wr, ")")
    | And(a) =>
        Wr.PutText(wr, "(LBLAND " & Fmt.Int(a.i) & " " & Fmt.Int(a.n) & " ");
        Print(wr, a.t);
        Wr.PutText(wr, ")")
    | Or(o) =>
        Wr.PutText(wr, "(LBLOR " & Fmt.Int(o.uniq) & " ");
        Print(wr, o.t);
        Wr.PutText(wr, ")")
    END (* TYPECASE *);
    Wr.Flush(wr)
  END Print;

PROCEDURE Stats() =
  BEGIN
    Wr.PutText(Stdio.stdout, "Asserted " & Fmt.Int(evs) &
      " labels.\n");
    Wr.Flush(Stdio.stdout)
  END Stats;


BEGIN
  traceLabelTrigger := Env.Get("PROVER_TRACE_TRIGGER");
  IF traceLabelTrigger = NIL THEN
    traceTriggers := 1
  END (* IF *)
END LabelName.

