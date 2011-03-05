(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright 1992 DEC *)
(* Last modified on Wed Apr  3 11:21:18 PST 2002 by saxe    *)
(*      modified on Wed Oct 23 12:24:37 PDT 1996 by detlefs *)

MODULE MatchingRule;

IMPORT PredSx, AF, PairSet, Context, Clause, Match, ContextPrivate,
       Enode, ClausePrivate, Prover, ParentSet;
IMPORT Word, Atom, Sx, FPrint, Wr, Fmt, Stdio, Env, Text, FileWr;
IMPORT RefList, RefRefTbl, RefSeq, RefPQ;

IMPORT Thread;
<*FATAL Sx.PrintError, Thread.Alerted, Wr.Failure *>

REVEAL
  T = Public BRANDED OBJECT
    hashV: Word.T;
    hashSet: BOOLEAN := FALSE;
   OVERRIDES
    init := TInit;
    toSx := ToSx;
    hash := Hash
  END (* OBJECT *);

(* Let "mr" be a "MatchingRule.Rule".  The abstract attribute "mr.Pats"
   is equal to the concrete field "mr.pats".  The definition of the
   abstract "Templ" attribute is somewhat more complicated: if
   "mr.opSym" is "NIL", then "mr.Templ" is "mr.template", if
   "mr.opSym" is non-"NIL", then "mr.pats" must have exactly one
   element, and "mr.Templ" is "(mr.opSym mr.pats.head
   mr.template)".  (This case is distinguished to allow the matcher
   to avoid reconstructing the left-hand side by instantiation of the
   pattern.)

   "commits" is a list of "CommittedArr"'s, each corresponding to one of
   the multipatterns in "pats".   Each "ca" is the "CommittedArr"
   corresponding to a multipattern "mp", then element "ca[i]" is an
   array of booleans for the "i"th cyclic shift of "mp".   Let
   "mpi = (p0 ... pN-1)" be the "i"th cyclic shift of "mp", and "b" be the
   corresponding boolean array.  Then "b[j]" is TRUE iff all the
   pattern variables in "pi" are bound by the patterns that precede it
   in the "mpi".  This is used as part of a matching optimization that
   matches committed patterns by quickly checking whether their
   instantiations are represented in the egraph.

   The "vars" field may optionally contain a list of variable names to
   use when negating a matching rule by instantiating the negation of
   the template.  "mr.unit" is TRUE iff "mr.Templ" is a unit clause.
*)

VAR
  mrId: CARDINAL;
  mrInitDebug := FALSE;

TYPE
  MRAFTbl = RefRefTbl.Default OBJECT
   OVERRIDES
    keyHash := MRKeyHash;
    keyEqual := MRKeyEqual;
  END (* OBJECT *);
VAR mrToAFTbl: MRAFTbl;

PROCEDURE MRKeyHash(<*UNUSED*>self: MRAFTbl; 
                     READONLY key: REFANY): Word.T =
  BEGIN RETURN Hash(key) END MRKeyHash;

PROCEDURE Hash(mr: T): Word.T =
  VAR res: Word.T := 0; BEGIN
    IF NOT mr.hashSet THEN
      FOR i := 0 TO LAST(mr.pats^) DO
        res := Word.Xor(PatternHash(mr.pats[i].p), res)
      END (* FOR *);
      VAR tHash: Word.T := 0; BEGIN
	TYPECASE mr.template OF
	| RefList.T(rl) =>
	    IF rl.head = PredSx.orSym THEN
	      VAR lits := rl.tail; BEGIN
		WHILE lits # NIL DO 
		  tHash := Word.Xor(PatternHash(lits.head), tHash);
		  lits := lits.tail
		END (* WHILE *)
	      END (* BEGIN *)
	    ELSE
	      tHash := PatternHash(rl)
	    END (* IF *)
	ELSE
	    tHash := PatternHash(mr.template)
	END (* TYPECASE *);
	res := Word.Plus(res, tHash)
      END (* BEGIN *);
      IF mr.unit THEN res := Word.Plus(res, 1) END (* IF *);
      IF mr.opSym # NIL THEN
	res := Word.Plus(res, Atom.Hash(mr.opSym))
      END (* IF *);
      mr.hashSet := TRUE;
      mr.hashV := res
    END (* IF *);
    RETURN mr.hashV
  END Hash;

PROCEDURE PatternHash(ra: REFANY): Word.T =
  BEGIN
    TYPECASE ra OF <*NOWARN*>
    | NULL =>
        RETURN 0
    | RefList.T(rl) =>
        RETURN Word.Plus(Word.Rotate(PatternHash(rl.head), 1),
                         PatternHash(rl.tail))
    | Atom.T(at) =>
        RETURN Atom.Hash(at)
    | REF INTEGER(ri) =>
        RETURN ri^
    | REF LONGREAL(rlr) =>
        RETURN FPrint.Hash(FPrint.FromLongReal(rlr^))
    | PatVar(pv) =>
        RETURN LAST(INTEGER) - pv^
    | Enode.T(e) =>
        RETURN Word.Rotate(FPrint.Hash(Enode.FingerP(e)), Word.Size DIV 2)
    END (* TYPECASE *)
  END PatternHash;

PROCEDURE MRKeyEqual(<*UNUSED*>self: MRAFTbl;
                      READONLY key1, key2: REFANY): BOOLEAN =
  BEGIN RETURN MREqual(key1, key2) END MRKeyEqual;

PROCEDURE MREqual(mr1, mr2: T): BOOLEAN =
  (* This could be made to tolerate different orders in the pats and the
     clause, but probably not worth it... *)
  BEGIN
    RETURN mr1 = mr2 OR
          (PatSetEqual(mr1.pats, mr2.pats)
       AND PatternEqual(mr1.template, mr2.template)
       AND mr1.opSym = mr2.opSym
       AND mr1.unit = mr2.unit)
  END MREqual;

PROCEDURE PatSetEqual(ps1, ps2: PatObjArr): BOOLEAN =
  BEGIN
    IF NUMBER(ps1^) # NUMBER(ps2^) THEN RETURN FALSE END (* IF *);
    FOR i := 0 TO LAST(ps1^) DO
      IF NOT PatternEqual(ps1[i], ps2[i]) THEN RETURN FALSE END (* IF *)
    END (* FOR *);
    RETURN TRUE
  END PatSetEqual;

PROCEDURE PatternEqual(p1, p2: REFANY): BOOLEAN =
  BEGIN
    TYPECASE p1 OF <*NOWARN*>
    | NULL =>
        RETURN p2 = NIL
    | RefList.T(rl1) =>
        TYPECASE p2 OF
        | NULL =>
            RETURN FALSE
        | RefList.T(rl2) =>
            RETURN PatternEqual(rl1.head, rl2.head)
               AND PatternEqual(rl1.tail, rl2.tail)
            
        ELSE RETURN FALSE
        END (* TYPECASE *)
    | REF INTEGER(ri1) =>
        TYPECASE p2 OF
        | REF INTEGER(ri2) => RETURN ri1^ = ri2^
        ELSE RETURN FALSE
        END (* TYPECASE *);
    | REF LONGREAL(rl1) =>
        TYPECASE p2 OF
        | REF LONGREAL(rl2) => RETURN rl1^ = rl2^
        ELSE RETURN FALSE
        END (* TYPECASE *);
    ELSE
        RETURN p1 = p2
    END (* TYPECASE *)
  END PatternEqual;

PROCEDURE TInit(self: T;
                 vars: RefList.T (* OF Atom.T *);
                 pats: RefList.T (* OF Pattern *);
                 template: Sx.T;
                 unit, clausal: BOOLEAN;
                 opSym: Atom.T := NIL;
                 immedPromote := FALSE;
                 plungeHint := FALSE): T =
  BEGIN
    INC(stats.ruleActivations);
    self.vars := NEW(REF ARRAY OF Atom.T, RefList.Length(vars));
    VAR i := 0; vtmp := vars; BEGIN
      WHILE vtmp # NIL DO
        self.vars[i] := vtmp.head; INC(i); vtmp := vtmp.tail
      END (* WHILE *)
    END (* BEGIN *);
    VAR pats2 := pats; i := 0; BEGIN
      self.pats := NEW(PatObjArr, RefList.Length(pats)); 
      WHILE pats2 # NIL DO
        <*ASSERT RefList.Length(pats2.head) <= Word.Size *>
        self.pats[i] := NEW(PatObj, p := pats2.head, matched := FALSE);
        pats2 := pats2.tail; 
        INC(i)
      END (* WHILE *)
    END (* BEGIN *);
    self.commits := ComputeCommits(pats);
    self.template := template;
    self.unit := unit;
    self.clausal := clausal;
    self.opSym := opSym;
    self.immedPromote := immedPromote;
    self.plungeHint := NOT(Prover.usePlungeHints) OR plungeHint;
    IF parentRule # NIL THEN self.parId := parentRule.id END (* IF *);

    self.trivial := NUMBER(self.pats^) = 1;
    IF self.trivial THEN
      self.trivial := RefList.Length(self.pats[0].p) = 1;
      IF self.trivial THEN
        VAR pat: RefList.T := self.pats[0].p.head; BEGIN
          self.trivial := IsTrivial(pat)
        END (* BEGIN *)
      END (* IF *)
    END (* IF *);

    IF NUMBER(self.pats^) = 0 THEN
      self.width := -1 (* patternless rule *)
    ELSE
      VAR width := RefList.Length(self.pats[0].p); i := 1; BEGIN
        WHILE i < NUMBER(self.pats^) AND
          width = RefList.Length(self.pats[i].p) DO
          INC(i)
        END (* WHILE *);
        IF i # NUMBER(self.pats^) THEN
          self.width := 0
        ELSE
          self.width := width
        END (* IF *)
      END (* BEGIN *)
    END (* IF *);

    INC(mrId);
    self.id := mrId;
    IF idWr # NIL THEN
      Wr.PutText(idWr, Fmt.Pad(Fmt.Int(self.id), 10, align := Fmt.Align.Left));
      VAR cl: TEXT; BEGIN
        IF self.unit THEN cl := "(unit)"
        ELSIF self.clausal THEN cl := "(clausal)"
        ELSE cl := "(non-clausal)"
        END (* IF *);
        Wr.PutText(idWr, Fmt.Pad(cl, 15, align := Fmt.Align.Left))
      END (* BEGIN *);
      Sx.Print(idWr, self.toSx());
      Wr.PutText(idWr, "\n")
    END (* IF *);

    IF mrInitDebug THEN
      Wr.PutText(Stdio.stdout, "\n  Creating matching rule:\n    ");
      Sx.Print(Stdio.stdout, self.toSx());
      Wr.PutText(Stdio.stdout, "\n")
    END (* IF *);

    PreInternMR(self);

    IF mrInitDebug THEN
      Wr.PutText(Stdio.stdout, "\n  Creating matching rule:\n    ");
      Sx.Print(Stdio.stdout, self.toSx());
      Wr.PutText(Stdio.stdout, "\n")
    END (* IF *);

    RETURN self
  END TInit;

PROCEDURE IsTrivial(pat: RefList.T): BOOLEAN =
  VAR p := pat.tail; BEGIN
    WHILE p # NIL DO
      IF NOT ISTYPE(p.head, PatVar) THEN RETURN FALSE END (* IF *);
      IF RefList.Member(p.tail, p.head) THEN RETURN FALSE END (* IF *);
      p := p.tail
    END (* WHILE *);
    RETURN TRUE
  END IsTrivial;

PROCEDURE ComputeCommits(pats: RefList.T): RefList.T =
  VAR res: RefList.T := NIL; BEGIN
    WHILE pats # NIL DO
      VAR mpat0: RefList.T := pats.head;
          ca: CommittedArr := NIL;
          mpatLen := RefList.Length(mpat0);
      BEGIN
        ca := NEW(CommittedArr, mpatLen, mpatLen);
	FOR k := 0 TO mpatLen-1 DO
	  VAR mpat := mpat0;
	      pvs := EmptyPatVarNumSet;
	      i := 0;
	  BEGIN
	    WHILE mpat # NIL DO
	      VAR patPVs := PatVars(mpat.head); BEGIN
		ca[k, i] := (patPVs - pvs = EmptyPatVarNumSet);
		pvs := pvs + patPVs
	      END (* BEGIN *);
	      mpat := mpat.tail; INC(i)
	    END (* WHILE *)
	  END (* BEGIN *);
	  mpat0 := RefList.Append(mpat0.tail, RefList.List1(mpat0.head))
	END (* FOR *);
        res := RefList.Cons(ca, res);
      END (* BEGIN *);
      pats := pats.tail;
    END (* WHILE *);
    RETURN RefList.ReverseD(res)
  END ComputeCommits;

REVEAL
  AtomF = AtomFPublic BRANDED OBJECT
    pos: T;
    neg: PredSx.T;
   OVERRIDES
    init := MatchRuleAFInit;
    assert := MatchRuleAFAssert;
    toSx := MatchRuleAFToSx;
    fingerprint := MatchRuleAFFP;
  END (* OBJECT *);

PROCEDURE MatchRuleAFInit(self: AtomF;
                          pos: T; neg: PredSx.T): AtomF =
  VAR ra: REFANY; res: AtomF; BEGIN
    IF mrToAFTbl.get(pos, ra) THEN
      res := ra
    ELSE
      EVAL AF.T.init(self);
      self.pos := pos;
      self.neg := neg;
      res := self;
      EVAL mrToAFTbl.put(pos, res);
      matchAFUndoStack.addhi(pos)
    END (* IF *);
    RETURN res
  END MatchRuleAFInit;

VAR matchRuleArrow := Atom.FromText("-->");

PROCEDURE MatchRuleAFToSx(self: AtomF;
                          <*UNUSED*> normForm: BOOLEAN): Sx.T =
  BEGIN RETURN ToSx(self.pos)
  END MatchRuleAFToSx;

PROCEDURE ToSx(mr: T): Sx.T =
  BEGIN
    IF mr.unit AND mr.opSym # NIL THEN
      <*ASSERT NUMBER(mr.pats^) = 1 *>
      RETURN RefList.List3(matchRuleArrow,
                           PatToPrintableSx(mr.pats[0].p),
                           RefList.List3(
                               mr.opSym,
                               PatToPrintableSx(mr.pats[0].p.head),
                               PatToPrintableSx(mr.template)))
    ELSE
      VAR pats: RefList.T := NIL; BEGIN
        FOR i := 0 TO LAST(mr.pats^) DO
          pats := RefList.Cons(PatToPrintableSx(mr.pats[i].p), pats)
        END (* FOR *);
        RETURN RefList.List3(matchRuleArrow,
                             RefList.ReverseD(pats),
                             PatToPrintableSx(mr.template))
      END (* BEGIN *)
    END (* IF *)
  END ToSx;

PROCEDURE MatchRuleAFFP(self: AtomF): FPrint.T =
  BEGIN RETURN FingerprintSx(self.neg)
  END MatchRuleAFFP;

VAR BoolFP := FPrint.FromText("BOOL");

PROCEDURE FingerprintSx(sx: Sx.T): FPrint.T =
  BEGIN
    TYPECASE sx OF <*NOWARN*>
    | NULL =>
        RETURN FPrint.Zero
    | Atom.T(at) =>
        RETURN FPrint.FromText(Atom.ToText(at))
    | REF INTEGER(ri) =>
        RETURN FPrint.FromInt(ri^)
    | REF REAL(rr) =>
        RETURN FPrint.FromLongReal(FLOAT(rr^, LONGREAL))
    | REF LONGREAL(rlr) =>
        RETURN FPrint.FromLongReal(rlr^)
    | REF EXTENDED(rex) =>
        RETURN FPrint.FromLongReal(FLOAT(rex^, LONGREAL))
    | REF BOOLEAN(rb) =>
        RETURN FPrint.Combine(BoolFP, FPrint.FromInt(ORD(rb^)))
    | RefList.T(rl) =>
        RETURN FPrint.Combine(FingerprintSx(rl.head),
                                FingerprintSx(rl.tail))
    | Enode.T(e) =>
        RETURN Enode.FingerP(e)
    END (* TYPECASE *)
  END FingerprintSx;

PROCEDURE MatchRuleAFAssert(self: AtomF;
                            plit: AF.Lit): BOOLEAN =
  BEGIN
    IF plit.sense THEN
      (* Don't add matching rules in D1P, since we won't do any
         matching. *)
      IF ContextPrivate.inD1P THEN RETURN TRUE END (* IF *);
      IF self.pos.unit THEN
(* Maybe I shouldn't do this!
        (* Check for validity. *)
        Context.Push();
        VAR sx: RefList.T; 
            nlit: AF.Lit;
            res: BOOLEAN;
        BEGIN
          TRY
            nlit := Context.Not(UnitRuleToLit(self.pos, s));
            res := Context.Assert(nlit, pf)
          FINALLY
            Context.Pop()
          END;
          IF NOT res THEN RETURN TRUE END (* IF *)
        END (* BEGIN *);
*)
        (* *)
        IF Prover.unitMatchInstance THEN
          VAR s: ConcSubst; <*FATAL Prover.Error *> BEGIN
            FOR i := 0 TO LAST(self.pos.vars^) DO
              IF self.pos.vars[i] # NIL THEN
                s[i] := Enode.FromSym(GenSym(self.pos.vars[i]))
              END (* IF *)
            END (* FOR *);
            <*ASSERT self.pos.opSym = NIL *>
            ContextPrivate.Propagate(
                ClausePrivate.SxToLiteral(self.pos.template, s))
          END (* BEGIN *)
        END (* BEGIN *)
      ELSIF self.pos.clausal THEN
        (*
        (* Check for validity. *)
        BEGIN
          Context.Push();
          VAR cl: Clause.T;
              lits: AF.LitList;
              sat := TRUE;
          BEGIN
            TRY
              cl := ClausePrivate.SxToClause(self.pos.template, s)
              lits := cl.lits;
              sat := TRUE;
              cl.mr := self.pos;
              WHILE lits # NIL AND sat DO
                VAR nlit := Context.Not(lits.head); BEGIN
                  nlit.clause := cl;
                  IF NOT ISTYPE(nlit.af, AtomF) AND
                    NOT ISTYPE(nlit.af, ContextPrivate.ProxyPropVar) THEN
                    sat := sat AND Context.Assert(nlit, NIL)
                  END (* IF *)
                END (* BEGIN *);
                lits := lits.tail
              END (* WHILE *)
            FINALLY
              Context.Pop()
            END;
            IF NOT sat THEN
              (*
              Wr.PutText(Stdio.stdout, "Eliminated valid rule:\n    ");
              Sx.Print(Stdio.stdout, self.pos.toSx());
              Wr.PutText(Stdio.stdout, "\n");
              Wr.Flush(Stdio.stdout);
              *)
              RETURN TRUE
            END (* IF *);
          END (* BEGIN *)
        END (* BEGIN *);
        (* Leave an instance if we should... *)
        IF Prover.nuMatchInstance THEN
          VAR cl := Clause.CNF(self.pos.template, s));
          BEGIN
            ClausePrivate.AddClause(pf.l, cl)
          END (* BEGIN *)
        END (* IF *)
        *)
      END (* IF *);
      FOR i := 0 TO LAST(self.pos.pats^) DO
        ComputePiecesAndPvs(self.pos, self.pos.pats[i]);
        self.pos.pats[i].pcPairs := PC(self.pos.pats[i].p);
        self.pos.pats[i].ppPairs := PP(self.pos.pats[i].p);
        self.pos.pats[i].pars := Parents(self.pos.pats[i]);
      END (* FOR *);

      IF self.pos.unit THEN
	Match.unitMatchRules := RefList.Cons(self.pos, Match.unitMatchRules);
        Context.opsEnabled[Context.Ops.UnitMatch] := TRUE
      ELSE
	Match.nonUnitMatchRules := RefList.Cons(self.pos, Match.nonUnitMatchRules);
        self.pos.score := 0.0;
        Context.opsEnabled[Context.Ops.RestrictedNUMatch] := TRUE
      END (* IF *);
      RETURN TRUE
    ELSE
      VAR nlit := Clause.CNF(PredSx.SkolemizeOuter( <*NOWARN*>
                                 self.neg, Context.rs),
                             EmptySub);
      BEGIN
        nlit.rightMost := plit.rightMost;
        ContextPrivate.Propagate(nlit);
        RETURN TRUE
      END (* BEGIN *)
    END (* IF *)
  END MatchRuleAFAssert;

TYPE PvRlArr = ARRAY PatVarNum OF RefList.T;

PROCEDURE ComputePiecesAndPvs(rule: T; po: PatObj) =
  VAR p := po.p; i := 0; BEGIN
    po.pieces := NEW(PieceRecArr, RefList.Length(p));
    WHILE p # NIL DO
      po.pieces[i].piece := p.head;
      po.pieces[i].pcs := PairSet.ToSparse(PCPat(p.head));
      po.pieces[i].pvs := PatVars(p.head);
      p := p.tail; INC(i)
    END (* WHILE *);
    VAR a := PvRlArr{NIL, ..}; BEGIN
      p := po.p;
      VAR i := 0; BEGIN
        WHILE p # NIL DO
          TYPECASE p.head OF <*NOWARN*>
          | RefList.T(pat) =>
              TermPVParents(a, pat)
          | Enode.T => (*SKIP*)
          END (* TYPECASE *);
          p := p.tail; INC(i)
        END (* WHILE *)
      END (* BEGIN *);
      po.pvs := NEW(PVRecArr, NUMBER(rule.vars^));
      FOR k := 0 TO LAST(rule.vars^) DO
        IF a[k].tail # NIL THEN
          VAR tmp := PairSet.Empty; BEGIN
            AddVarPPPairs(tmp, a[k]);
            po.pvs[k].pps := PairSet.ToSparse(tmp)
          END (* BEGIN *);
          po.multiplePVs := po.multiplePVs + PatVarNumSet{k}
        ELSE
          po.pvs[k].pps := PairSet.EmptySparse
        END (* IF *);
      END (* FOR *)
    END (* BEGIN *)
  END ComputePiecesAndPvs;

PROCEDURE TermPVParents(VAR a: PvRlArr; pat: RefList.T) =
  VAR fsym: Enode.T := pat.head; BEGIN
    pat := pat.tail;
    WHILE pat # NIL DO
      TYPECASE pat.head OF
      | PatVar(pv) =>
          a[pv^] := RefList.Cons(fsym, a[pv^])
      | RefList.T(rl) =>
          TermPVParents(a, rl)
      ELSE
      END (* TYPECASE *);
      pat := pat.tail
    END (* WHILE *)
  END TermPVParents;

(* Converts atoms and numbers in the "pats" and "template" of "mr" into
   enodes. *)
PROCEDURE PreInternMR(mr: T) =
  BEGIN
    FOR i := 0 TO LAST(mr.pats^) DO
      VAR mpat: RefList.T := mr.pats[i].p;
          newMpat: RefList.T := NIL; 
      BEGIN
        WHILE mpat # NIL DO
          newMpat := RefList.Cons(PreIntern(mpat.head, TRUE), newMpat);
          mpat := mpat.tail
        END (* WHILE *);
        mr.pats[i].p := RefList.ReverseD(newMpat);
      END (* BEGIN *);
    END (* FOR *);
    mr.template := PreIntern(mr.template, FALSE)
  END PreInternMR;

PROCEDURE PreIntern(tmpl: Template; isPat: BOOLEAN): REFANY =
  BEGIN
    TYPECASE tmpl OF <*NOWARN*>
    | NULL => RETURN NIL
    | Enode.T => RETURN tmpl
    | Atom.T(at) => RETURN Enode.FromSym(at)
    | REF INTEGER(ri) => RETURN Enode.FromInt(ri^)
    | REF LONGREAL(rlr) => RETURN Enode.FromLongReal(rlr^)
    | PatVar => RETURN tmpl
    | RefList.T(rl) =>
        <*ASSERT rl.head # PredSx.existsSym *>
        IF rl.head = PredSx.forallSym THEN
          RETURN rl
        ELSIF PredSx.boolOps.member(rl.head) OR PredSx.relOps.member(rl.head)
          AND NOT (rl.head = PredSx.diffSym AND isPat) THEN
          VAR newArgs := PreInternArgList(rl.tail, isPat); BEGIN
            IF newArgs = rl.tail THEN
              RETURN rl
            ELSE
              RETURN RefList.Cons(rl.head, newArgs)
            END (* IF *)
          END (* BEGIN *)
        ELSIF rl.head = PredSx.labelSym OR rl.head = PredSx.lblPosSym
          OR rl.head = PredSx.lblNegSym THEN
          VAR arg := PreIntern(rl.tail.tail.head, isPat); BEGIN
            IF arg = rl.tail.tail.head THEN
              RETURN rl
            ELSE
              RETURN RefList.List3(rl.head, rl.tail.head, arg)
            END (* IF *)
          END (* BEGIN *)
        ELSE
	  VAR fsym: Enode.FSym; args := rl.tail; BEGIN
	    TYPECASE rl.head OF <*NOWARN*>
	    | Atom.T(at) => fsym := Enode.FromSym(at, fsym := TRUE);
	    | Enode.FSym(fs) => fsym := fs
	    END (* TYPECASE *);
            TYPECASE PreInternArgs(args, isPat, TRUE) OF <*NOWARN*>
            | RefList.T(argsRL) =>
                RETURN RefList.Cons(fsym, argsRL)
            | Enode.T(argsT) =>
                RETURN Enode.Cons(fsym, argsT)
            END (* TYPECASE *)
	  END (* BEGIN *)
        END (* IF *)
    END (* TYPECASE *)
  END PreIntern;

PROCEDURE PreInternArgs(args: RefList.T; isPat, parentsOK: BOOLEAN): REFANY =
  BEGIN
    IF args = NIL THEN
      IF parentsOK THEN RETURN Enode.enil
      ELSE RETURN NIL
      END (* IF *)
    ELSE
      VAR hd := PreIntern(args.head, isPat);
          tl := PreInternArgs(args.tail, isPat, parentsOK AND
                ISTYPE(hd, Enode.T));
      BEGIN
        IF tl # NIL AND ISTYPE(tl, Enode.T) THEN
          RETURN Enode.Cons(hd, tl)
        ELSIF hd = args.head AND tl = args.tail THEN
          RETURN args
        ELSE
          RETURN RefList.Cons(hd, tl)
        END (* IF *)
      END (* BEGIN *)
    END (* IF *)
  END PreInternArgs;

PROCEDURE PreInternArgList(args: RefList.T; isPat: BOOLEAN): RefList.T =
  BEGIN
    IF args = NIL THEN RETURN NIL
    ELSE
      VAR hd := PreIntern(args.head, isPat);
          tl := PreInternArgList(args.tail, isPat);
      BEGIN
        IF hd = args.head AND tl = args.tail THEN
          RETURN args
        ELSE
          RETURN RefList.Cons(hd, tl)
        END (* IF *)
      END (* BEGIN *)
    END (* IF *)
  END PreInternArgList;

PROCEDURE GenSym(at: Atom.T): Atom.T =
  (* Returns an Atom that has not been used in the current proof, and records
     it (in "ClausePrivate.atoms") as being used in the current proof.  The
     implementation actually bases the name of the new atom on that of
     "at".
  *)
  VAR i := Context.rs.inc(at); BEGIN
    RETURN Atom.FromText(Atom.ToText(at) & PredSx.SkolemSep & Fmt.Int(i))
  END GenSym;

VAR
  matchAFUndoStack: RefSeq.T;

PROCEDURE PatToPrintableSx(pat: REFANY): REFANY =
  BEGIN
    TYPECASE pat OF
    | NULL => RETURN NIL
    | RefList.T(rl) =>
        RETURN RefList.Cons(PatToPrintableSx(rl.head),
                            PatToPrintableSx(rl.tail))
    | PatVar(pv) =>
        RETURN Atom.FromText("$$" & Fmt.Int(pv^))
    | Enode.T(e) =>
        RETURN Enode.DbgToSx(e)
    ELSE
        RETURN pat
    END (* TYPECASE *)
  END PatToPrintableSx;

PROCEDURE PatVars(t: Template): PatVarNumSet =
  VAR pvs := PatVarNumSet{}; BEGIN
    PatVarsWork(t, pvs); RETURN pvs
  END PatVars;

PROCEDURE PatVarsWork(t: Template; VAR pvs: PatVarNumSet) =
  BEGIN
    TYPECASE t OF
    | NULL =>
        (*SKIP*)
    | PatVar(pv) =>
        pvs := pvs + PatVarNumSet{pv^}
    | RefList.T(rl) =>
        PatVarsWork(rl.head, pvs); PatVarsWork(rl.tail, pvs)
    ELSE
    END (* TYPECASE *)
  END PatVarsWork;

(* Special allocation for substitutions. *)
VAR freeHead: RefList.T := NIL;
    ncells, nfree := 0;

PROCEDURE SubstSxCons(READONLY hd: REFANY; tail: RefList.T): RefList.T =
  BEGIN
    IF freeHead = NIL THEN
      INC(ncells);
      RETURN RefList.Cons(hd, tail)
    ELSE
      DEC(nfree);
      VAR cons := freeHead; BEGIN
        freeHead := freeHead.tail;
        cons.head := hd; cons.tail := tail;
        RETURN cons
      END (* BEGIN *)
    END (* IF *)
  END SubstSxCons;

PROCEDURE SubstSxFree(sx: REFANY) =
  BEGIN
    TYPECASE sx OF
    | NULL => (*SKIP*)
    | RefList.T(rl) =>
        VAR hd := rl.head;
            tl := rl.tail;
        BEGIN
          rl.head := NIL;
          rl.tail := freeHead;
          freeHead := rl;
          SubstSxFree(hd);
          SubstSxFree(tl);
          INC(nfree)
        END (* BEGIN *)
    ELSE
    END (* TYPECASE *)
  END SubstSxFree;

VAR sxS: RefList.T := NIL;

TYPE
  AllocProc = PROCEDURE(READONLY hd: REFANY; tail: RefList.T): RefList.T;

PROCEDURE OurApplySubst(pat: REFANY; READONLY s: Substitution): REFANY =
  BEGIN 
    IF s = EmptySub THEN RETURN pat
    ELSE RETURN ApplySubst(pat, s, TRUE)
    END (* IF *)
  END OurApplySubst;


PROCEDURE ApplySubst(pat: REFANY;
                     READONLY s: Substitution; perm := FALSE): REFANY =
  BEGIN
    sxS := NIL;
    IF perm THEN
      RETURN ApplySubstWork(pat, s, RefList.Cons)
    ELSE
      RETURN ApplySubstWork(pat, s, SubstSxCons)
    END (* IF *)
  END ApplySubst;

PROCEDURE ApplySubstWork(pat: REFANY; READONLY s: Substitution;
                         alloc: AllocProc): REFANY =
  BEGIN
    TYPECASE pat OF <*NOWARN*>
    | NULL =>
        RETURN NIL
    | RefList.T(l) =>
(* REMOVE        
        IF l.head = PredSx.lblPosSym OR l.head = PredSx.lblNegSym THEN
          VAR ln := l.tail.head; BEGIN
            IF sxS = NIL THEN
              VAR i := 0; BEGIN
                WHILE i < NUMBER(s) AND s[i] # NIL DO
                  sxS := RefList.Cons(s[i], sxS); INC(i)
                END (* WHILE *);
              END (* BEGIN *)
            END (* IF *);
            RETURN alloc(l.head,
                         alloc(LNParameterize(ln, alloc),
                               alloc(ApplySubstWork(l.tail.tail.head,
                                                    s, alloc),
                                     NIL)))
          END (* BEGIN *)
        ELSE
*)
	  VAR hd := ApplySubstWork(l.head, s, alloc);
	      tl := ApplySubstWork(l.tail, s, alloc);
	  BEGIN
            RETURN alloc(hd, tl)
	  END (* BEGIN *)
(* REMOVE
        END (* IF *)
*)
    | PatVar(ri) =>
        RETURN s[ri^]
    ELSE
        RETURN pat
    END (* TYPECASE *)
  END ApplySubstWork;

(* REMOVE
PROCEDURE LNParameterize(ln: Sx.T; alloc: AllocProc): Sx.T = 
  BEGIN
    TYPECASE ln OF <*NOWARN*>
    | Atom.T(at) =>
        RETURN alloc(PredSx.lblNameQuantSym,
                     alloc(at, alloc(CopyAlloc(sxS, alloc), NIL)))
    | RefList.T(rl) =>
        IF rl.head = PredSx.lblNameQuantSym THEN
          RETURN alloc(PredSx.lblNameQuantSym,
                       alloc(CopyAlloc(rl.tail.head, alloc),
                             alloc(
                                 AppendAlloc(sxS,
                                             CopyAlloc(
                                                 rl.tail.tail.head,
                                                 alloc),
                                             alloc),
                                 NIL)))
        ELSIF rl.head = PredSx.lblNameAndSym THEN
          RETURN alloc(PredSx.lblNameAndSym,
                       alloc(LNParameterize(rl.tail.head, alloc),
                             CopyAlloc(rl.tail.tail, alloc)))
        ELSIF rl.head = PredSx.lblNameOrSym THEN
          RETURN alloc(PredSx.lblNameOrSym,
                       alloc(LNParameterize(rl.tail.head, alloc),
                             CopyAlloc(rl.tail.tail, alloc)))
        ELSE
          <*ASSERT FALSE*>
        END (* IF *)
    END (* TYPECASE *)
  END LNParameterize;
*)

PROCEDURE CopyAlloc(sx: REFANY; alloc: AllocProc): REFANY =
  BEGIN
    TYPECASE sx OF
    | NULL =>
        RETURN NIL
    | RefList.T(rl) =>
        RETURN alloc(CopyAlloc(rl.head, alloc), CopyAlloc(rl.tail, alloc))
    ELSE
        RETURN sx
    END (* TYPECASE *)
  END CopyAlloc;

PROCEDURE AppendAlloc(l1, l2: RefList.T; alloc: AllocProc): RefList.T =
  BEGIN
    IF l1 = NIL THEN
      RETURN l2
    ELSE
      RETURN alloc(l1.head, AppendAlloc(l1.tail, l2, alloc))
    END (* IF *)
  END AppendAlloc;

PROCEDURE Init() =
  BEGIN
    stats := StatRec{};

    mrToAFTbl := NEW(MRAFTbl).init();
    matchAFUndoStack := NEW(RefSeq.T).init();
    mrId := 0;

    VAR idFile := Env.Get("PROVER_MRID_TABLE"); BEGIN
      IF idFile # NIL THEN
        IF Text.Length(idFile) = 0 THEN
          idFile := "/tmp/mrid.tbl"
        END (* IF *);
        idWr := FileWr.Open(idFile) <*NOWARN*>
      ELSE
        idWr := NIL
      END (* IF *)
    END (* BEGIN *)

  END Init;

PROCEDURE Push() =
  BEGIN
    matchAFUndoStack.addhi(NIL)
  END Push;

PROCEDURE Pop() =
  VAR ra: REFANY; BEGIN
    LOOP
      VAR mr: T := matchAFUndoStack.remhi(); BEGIN
	IF mr = NIL THEN
          EXIT
	ELSE
          VAR b := mrToAFTbl.delete(mr, ra); BEGIN
            <*ASSERT b*>
          END (* BEGIN *)
        END (* IF *)
      END (* BEGIN *)
    END (* LOOP *)
  END Pop;

TYPE
  RulePQ = RefPQ.Default BRANDED OBJECT
   OVERRIDES
    pCompare := RefRealComp;
  END (* OBJECT *);
  RulePQElt = RefPQ.Elt OBJECT
    rule: T
  END (* OBJECT *);

PROCEDURE RefRealComp(<*UNUSED*> pq: RulePQ;
                      READONLY p1, p2: REFANY): [-1..1] =
  VAR r1: REF REAL := p1; r2: REF REAL := p2; BEGIN
    IF r1^ < r2^ THEN RETURN -1
    ELSIF r1^ > r2^ THEN RETURN 1
    ELSE RETURN 0
    END (* IF *)
  END RefRealComp;

PROCEDURE Stats() =
  BEGIN
    VAR iter := mrToAFTbl.iterate(); ruleRA: REFANY; af: REFANY;
        pq := NEW(RulePQ).init(mrToAFTbl.size());
    BEGIN
      WHILE iter.next(ruleRA, af) DO
        VAR rule: T := ruleRA;
            pri := NEW(REF REAL);
        BEGIN
          pri^ := -rule.score;
          pq.insert(NEW(RulePQElt, priority := pri, rule := rule))
        END (* BEGIN *)
      END (* WHILE *);
      VAR i := 0; BEGIN
        WHILE i < 5 AND pq.size() > 0 DO
          VAR min: RulePQElt := pq.deleteMin(); <*NOWARN*>
              rule := min.rule;
          BEGIN
            IF rule.score > 0.0 THEN
              Wr.PutText(Stdio.stdout,
                         Fmt.Pad(Fmt.Real(
                                     rule.score, prec := 2,
                                     style := Fmt.Style.Fix), 10) & "   ");
              Sx.Print(Stdio.stdout, rule.toSx());
              Wr.PutText(Stdio.stdout, "\n")
            END (* IF *)
          END (* BEGIN *);
          INC(i)
        END (* WHILE *)
      END (* FOR *)
    END (* BEGIN *)
  END Stats;

PROCEDURE PC(mpat: RefList.T): PairSet.Sparse =
  VAR res := PairSet.Empty; BEGIN
    WHILE mpat # NIL DO
      PairSet.UnionD(res, PCPat(mpat.head));
      mpat := mpat.tail
    END;
    RETURN PairSet.ToSparse(res)
  END PC;

PROCEDURE PCPat(pat: Sx.T): PairSet.T =
  VAR res := PairSet.Empty; BEGIN
    TYPECASE pat OF <*NOWARN*>
    | Enode.T => (*SKIP*)
    | RefList.T (rl) =>
        PCPatRD(res, rl.head, rl.tail)
    END;
    RETURN res
  END PCPat;

PROCEDURE PCPatRD(VAR res: PairSet.T; parLabel: Enode.T; args: RefList.T) =
  BEGIN
    WHILE args # NIL DO
      TYPECASE args.head OF
      | RefList.T (term) =>
          IF term # NIL AND ISTYPE(term.head, Enode.T) THEN
            PairSet.AddPairD(res, parLabel.shortId(),
                             NARROW(term.head, Enode.T).shortId());
            PCPatRD(res, term.head, term.tail)
          END (* IF *)
      | Enode.T(term) =>
          PairSet.AddPairD(res, parLabel.shortId(), term.shortId());
          Enode.MakePreInterned(term)
      ELSE (*SKIP*)
      END (* TYPECASE *);
      args := args.tail
    END (* WHILE *)
  END PCPatRD;

PROCEDURE PP(mpat: RefList.T): PairSet.Sparse =
  VAR a := PvRlArr{NIL, ..};
      res := PairSet.Empty;
  BEGIN
    WHILE mpat # NIL DO
      TYPECASE mpat.head OF <*NOWARN*>
      | RefList.T(rl) =>
          PPPat(a, rl)
      | Enode.T => (*SKIP*)
      END (* TYPECASE *);
      mpat := mpat.tail
    END (* WHILE *);
    FOR i := 0 TO LAST(a) DO
      AddVarPPPairs(res, a[i])
    END (* FOR *);
    RETURN PairSet.ToSparse(res)
  END PP;

PROCEDURE PPPat(VAR a: PvRlArr; pat: RefList.T) =
  VAR fsym: Enode.T := pat.head; BEGIN
    pat := pat.tail;
    WHILE pat # NIL DO
      TYPECASE pat.head OF
      | PatVar(pv) =>
          a[pv^] := RefList.Cons(fsym, a[pv^])
      | RefList.T(rl) =>
          PPPat(a, rl)
      ELSE
      END (* TYPECASE *);
      pat := pat.tail
    END (* WHILE *)
  END PPPat;

PROCEDURE AddVarPPPairs(VAR res: PairSet.T; rl: RefList.T) =
  BEGIN
    WHILE rl # NIL DO
      VAR first: Enode.T := rl.head;
          rest := rl.tail;
      BEGIN
        WHILE rest # NIL DO
          VAR second: Enode.T := rest.head;
              a := first.shortId(); b := second.shortId();
          BEGIN
            IF a > b THEN
              PairSet.AddPairD(res, b, a)
            ELSE
              PairSet.AddPairD(res, a, b)
            END (* IF *)
          END (* BEGIN *);
          rest := rest.tail
        END (* WHILE *)
      END (* BEGIN *);
      rl := rl.tail
    END (* WHILE *)
  END AddVarPPPairs;

(* REMOVE
PROCEDURE Parents(mpat: RefList.T): ParentSet.T =
  VAR res := ParentSet.Empty(); BEGIN
    WHILE mpat # NIL DO
      TYPECASE mpat.head OF <*NOWARN*>
      | RefList.T(pat) =>
          IF IsTrivial(pat) THEN
            ParentSet.AddParentD(res, pat.head)
          END (* IF *)
      | Enode.T => (*SKIP*)
      END (* TYPECASE *);
      mpat := mpat.tail
    END (* WHILE *);
    RETURN res
  END Parents;
*)
PROCEDURE Parents(po: PatObj): ParentSet.T =
  VAR res := ParentSet.Empty(); BEGIN
    FOR i := 0 TO LAST(po.pieces^) DO
      VAR pieceRec := po.pieces[i]; BEGIN
        TYPECASE pieceRec.piece OF <*NOWARN*>
        | RefList.T(pat) =>
            IF IsTrivial(pat) THEN
              VAR j := 0; BEGIN
                WHILE j # NUMBER(po.pieces^) AND
                  (j = i OR
                   pieceRec.pvs * po.pieces[j].pvs = EmptyPatVarNumSet) DO
                  INC(j)
                END (* WHILE *);
                IF j = NUMBER(po.pieces^) THEN
                  ParentSet.AddParentD(res, pat.head)
                END (* IF *)
              END (* BEGIN *)
            END (* IF *)
        | Enode.T => (*SKIP*)
        END (* TYPECASE *)
      END (* BEGIN *)
    END (* FOR *);
    RETURN res
  END Parents;


BEGIN
  FOR i := 0 TO MaxPatVars-1 DO
    pv[i] := NEW(PatVar); pv[i]^ := i
  END (* FOR *)
END MatchingRule.

