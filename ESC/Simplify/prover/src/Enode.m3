(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright 1992 DEC *)
(* Last modified on Tue Jun  4 12:18:20 PDT 2002 by saxe    *)
(*      modified on Wed Aug 16 17:02:52 PDT 2000 by rustan  *)
(*      modified on Mon Nov  4 16:12:29 PST 1996 by detlefs *)

MODULE Enode EXPORTS Enode, Match;

IMPORT Signature, SigTab, PredSx, Context, Simplex, Clause, AF,
       ClausePrivate, Prover, Rat, Orders, ProxyProp, ContextPrivate,
       PredDefs, PairSet, ParentSet, IdSet, MatchingRule, LabelName;
IMPORT Atom, Text, Sx, Fmt, IntPair, Word, Env, Scan, FPrint,
       FileWr, Process;
IMPORT RefList, AtomRefTbl, IntPairArraySort, IntIntTbl, IntRefTbl,
       RefRefTbl, RefListSort, AtomIntTbl, FPRefTbl, IntSetDef, RefSeq, 
       IdSetSeq, FPSeq, ParentSetSeq;
IMPORT RTHeapDebug, RTCollector;
(* For debugging *)
IMPORT Stdio, Wr, Thread, IO, Perf, Time;

<*FATAL Prover.Error, Wr.Failure, Thread.Alerted, Sx.PrintError*>

CONST
  TwoTo30 = Word.Shift(1,30);
  LastCard = TwoTo30 + (TwoTo30-1);
  MaxDistClasses = 32;
  EmptyDistClassSet = DistClassSet{};
  EmptyPropSet = SET OF Prop{};
  ActiveProp = SET OF Prop{Prop.Active};
  IsPredSymProp = SET OF Prop{Prop.IsPredSym};
  HasOrdTermProp = SET OF Prop{Prop.HasOrdTerm};
  HasPredTermProp = SET OF Prop{Prop.HasPredTerm};
  HasPredMapTermProp = SET OF Prop{Prop.HasPredMapTerm};
  HasSelPredMapTermProp = SET OF Prop{Prop.HasSelPredMapTerm};
  DiffFromTrueProp = SET OF Prop{Prop.DiffFromTrue};
  ConstProp = SET OF Prop{Prop.Const};
  MarkedProp = SET OF Prop{Prop.Marked};
  PreInternedProp = SET OF Prop{Prop.PreInterned};
  IsPredTermProp = SET OF Prop{Prop.HasPredTerm, Prop.HasSelPredMapTerm};

TYPE
  Card32 = BITS 32 FOR [0..LastCard];
  Prop = { Active, HasOrdTerm,
           IsPredSym, HasPredTerm, HasPredMapTerm, HasSelPredMapTerm,
           DiffFromTrue,
           Const,
           Marked,
           PreInterned };
  DistClassSet = SET OF [0..MaxDistClasses-1];

REVEAL
  Misc = OrdersMisc BRANDED OBJECT
    distClasses: (* BITS MaxDistClasses FOR *) DistClassSet :=
        EmptyDistClassSet;
    forbids: TList := NIL;
  END (* OBJECT *);

  T = Public BRANDED OBJECT
    root, next: T;
    parent: Parent;
    minHt: T;
    fp: FPrint.T;
    props: (* BITS 32 FOR *) SET OF Prop;
    size, parentSize, modTime, ht: Card32;
    misc: Misc;
   METHODS
    init(): T := InitT;
   OVERRIDES
    getId := GetId;
    getFP := GetFP;
    getMisc := GetMisc;
    shortId := ShortId;
  END (* OBJECT *);

TYPE
  Leaf = T BRANDED OBJECT
    sym: REFANY (* An S-expression *);
   METHODS
    init(sym: REFANY): Leaf := InitLeaf;
  END (* OBJECT *);

REVEAL
  FSym = Leaf BRANDED OBJECT
    terms: CarParent;
   METHODS
    init(sym: REFANY): FSym := InitFSym;
  END (* OBJECT *);

TYPE
  NonFSym = Leaf BRANDED OBJECT
    parentLabels, childLabels := IdSet.T{}
  END (* OBJECT *);

  Parent = T BRANDED OBJECT
    car, cdr: T;
    same: ARRAY BOOLEAN OF Parent;
    cgPtr: Parent;
   METHODS
    init(car, cdr: T): Parent := InitParent;
    (* REQUIRES "car" and "cdr" are roots. *)
  END (* OBJECT *);
  CdrParent = Parent BRANDED OBJECT END (* OBJECT *);
  CarParent = Parent BRANDED OBJECT
    link, prev: CarParent;
    parentLabels, childLabels := IdSet.T{}
   METHODS
    init(car, cdr: T): CarParent := InitCarParent;
  END (* OBJECT *);
    

VAR timer := 0;
    lastUMatchTime := -1;
    lastNUMatchTime := -1;

TYPE
  TList = REF RECORD t: T; next: TList END (* RECORD *);
  (* "TList"'s are either "NIL" or circular. *)

(* A function symbol or variable is represented by a "Leaf" enode; an
   "FSym" "Leaf" represents a function symbol, and a non-"FSym" "Leaf"
   a variable; we assume symbols for function symbols and variables
   are drawn from distinct sets, and distinct symbols are represented
   by distinct enodes.  The table "symTab" maps the textual
   representation of a functions symbol or variable to the enode that
   represents it, if such an enode exists.

   The term "f(x1, ... xn)" is represented by a "Parent" enode whose
   left child ("e.car") is an enode representing the function symbol
   "f", and whose right child ("e.cdr") represents the argument list
   "(x1, ..., xn)."  A non-empty argument list is represented as an
   enode whose "car" is an enode representing the first term, and
   whose "cdr" is an enode representing the rest of the list.  An
   empty argument list is represented by the distinguished "Leaf"
   enode "enil".

   The "root", "next", and "size" fields represent an equivalence
   relation on "S" as follows:  for all enodes "e" and "f", "e" is
   equivalent to "f" iff "e.root = f.root"; also "e.root.root =
   e.root", that is every root is element of the class that it
   defines; also "e" is equivalent to "f" iff "e" is reachable from
   "f" following the "next" links; and finally "e.size" is the size of
   "e"'s equivalence class.

   The "minHt" field of a root points to a node in the equivalence
   class of minimal height.  If "e" is a node, "e.ht" is its
   height, and "e.fp" its fingerprint.  The height of "e" is set at
   the time "e" is created, and unaffected by changes to the
   equivalence relation thereafter; similarly for the fingerprint.

   Distinct enodes have distinct "id" fields.

   The "parent" field of a root points to some parent of its
   equivalence class.  The "same[TRUE]" field is a circular list of all
   parents whose car's are equivalent; The "same[FALSE]" field is a
   circular list of all parents whose cdr's are equivalent.
   The "size" field is the number of nodes of the enode's
   equivalence class.  The "parentSize" of an enode "e" is equal to
   the number of enodes in "e.parent"'s "same[TRUE]" or "same[FALSE]"
   circular list, depending on whether "e" is a car node or a cdr
   node.  The "cgPtr" field of a parent points to another member of
   the parent's congruence class.  The members of a congruence class
   are structured as a tree; the "cgPtr" field of the root of the tree
   (the node at depth 0) points to itself, and the "cgPtr" fields of
   congruence class members at depth $n > 0$ point to members at depth
   $n-1$.

   If "e" is an enode and "e.misc" is non-NIL, then "e.misc.forbids"
   is a list of all enodes "f" such that the context includes the
   distinction "e # f".  {\it Distinction classes} are represented by
   small integers (< 32); if "e" is an enode with a non-NIL "misc"
   field, then "e.misc.distClasses" is the set of distinction classes
   to which it belongs.  No two members of a distinction class may be
   be merged.  An enode "e" with "e.misc = NIL" has no forbidden merges.

   The "active" bit of an enode is set iff it has participated in an
   asserting, in which case it is eligible to be matched.  The global
   variable "timer" advances (along a search path; advances in "timer"
   are undone by pops) every time some event that might make new
   matches possible, such as a merge or an enode activation, occurs;
   the enodes that might participate in such new matches have their
   "modTime" fields updated to the new value of "timer".  These
   modifications of "modTime" are undone appropriately when
   backtracking.

   The "terms" field of an "FSym" enode "f", if non-"NIL", is the
   "CarParent" of "f" with maximum "modTime".  The "link" field links
   "CarParent" in descending order of "modTime"; the "prev" field is
   the inverse of "link".  (These lists are circular.)
*)

(* "lastUMatchTime" and "lastNUMatchTime" are the values of "timer" at
   the start of the last round of unit and non-unit matching,
   respectively, or else -1.  Updates to these variables are undone
   appropriately. *)
   
VAR symTab: AtomRefTbl.T;

PROCEDURE FromSym(sym: Atom.T; fsym := FALSE): T =
  VAR res: Leaf; resRA: REFANY; BEGIN
    IF symTab.get(sym, resRA) THEN
      res := resRA;
    ELSE
      IF fsym THEN
        res := NEW(FSym).init(sym)
      ELSE
        res := NEW(NonFSym).init(sym)
      END (* IF *);
      EVAL symTab.put(sym, res);
      UndoStackPush(UndoType.Leaf, res);
      INC(Prover.actTestNumber);
      IF Prover.noActivate OR
          (Prover.forceActivateStart <= Prover.actTestNumber AND
           Prover.actTestNumber < Prover.forceActivateEnd) THEN
        IF Prover.forceActivateStart <= Prover.actTestNumber THEN
           Wr.PutText(Stdio.stdout, "Forcing activation of ");
	   Sx.Print(Stdio.stdout, DbgToSx(res));
           Wr.PutText(Stdio.stdout, "\n");
           Wr.Flush(Stdio.stdout);
        END;
        Activate(res)
      END (* IF *);
      INC(stats.nLeaf);
      stats.maxLeaf := MAX(stats.maxLeaf, stats.nLeaf);
      stats.maxTotnodes := MAX(stats.maxTotnodes, stats.nLeaf + stats.nCons)
    END (* IF *);
    RETURN res
  END FromSym;

PROCEDURE FromInt(i: INTEGER): T =
  VAR res: Leaf; resRA: REFANY; iAtom := Atom.FromText(Fmt.Int(i)); BEGIN
    IF symTab.get(iAtom, resRA) THEN
      res := resRA;
    ELSE
      VAR ri := NEW(REF INTEGER); BEGIN
        ri^ := i; res := NEW(NonFSym).init(ri);
        IF Prover.intStatus THEN
          res.size := 1000001    (* Favor this enode to be a root *)
        END;
        res.props := res.props + ConstProp
      END (* BEGIN *);
      Simplex.EnodeIsLit(res, Rat.T{i, 1});
      EVAL symTab.put(iAtom, res);
      UndoStackPush(UndoType.Leaf, res);
      INC(Prover.actTestNumber);
      IF Prover.noActivate OR
          (Prover.forceActivateStart <= Prover.actTestNumber AND
           Prover.actTestNumber < Prover.forceActivateEnd) THEN
        IF Prover.forceActivateStart <= Prover.actTestNumber THEN
           Wr.PutText(Stdio.stdout, "Forcing activation of ");
	   Sx.Print(Stdio.stdout, DbgToSx(res));
           Wr.PutText(Stdio.stdout, "\n");
           Wr.Flush(Stdio.stdout);
        END;
        Activate(res)
      END (* IF *);
      INC(stats.nLeaf); stats.maxLeaf := MAX(stats.maxLeaf, stats.nLeaf);
      stats.maxTotnodes := MAX(stats.maxTotnodes, stats.nLeaf + stats.nCons)
    END (* IF *);
    RETURN res
  END FromInt;

PROCEDURE IsConst(e: T) =
  VAR l: Leaf := e; BEGIN
    l.props := l.props + ConstProp
  END IsConst;

PROCEDURE IntHasEnode(i: INTEGER): BOOLEAN =
  VAR resRA: REFANY; iAtom := Atom.FromText(Fmt.Int(i)); BEGIN
    RETURN symTab.get(iAtom, resRA)
  END IntHasEnode;

PROCEDURE FromLongReal(lr: LONGREAL): T =
  VAR res: Leaf; resRA: REFANY; iAtom := Atom.FromText(Fmt.LongReal(lr)); BEGIN
    IF symTab.get(iAtom, resRA) THEN
      res := resRA;
    ELSE
      VAR rlr := NEW(REF LONGREAL); BEGIN
        rlr^ := lr; res := NEW(NonFSym).init(rlr)
      END (* BEGIN *);
      EVAL symTab.put(iAtom, res);
      UndoStackPush(UndoType.Leaf, res);
      INC(Prover.actTestNumber);
      IF Prover.noActivate OR
          (Prover.forceActivateStart <= Prover.actTestNumber AND
           Prover.actTestNumber < Prover.forceActivateEnd) THEN
        IF Prover.forceActivateStart <= Prover.actTestNumber THEN
           Wr.PutText(Stdio.stdout, "Forcing activation of ");
	   Sx.Print(Stdio.stdout, DbgToSx(res));
           Wr.PutText(Stdio.stdout, "\n");
           Wr.Flush(Stdio.stdout);
        END;
        Activate(res)
      END (* IF *);
      INC(stats.nLeaf); stats.maxLeaf := MAX(stats.maxLeaf, stats.nLeaf);
      stats.maxTotnodes := MAX(stats.maxTotnodes, stats.nLeaf + stats.nCons)
    END (* IF *);
    RETURN res
  END FromLongReal;

VAR
  idCntr: CARDINAL;
  nDistClasses: CARDINAL;

PROCEDURE InitT(t: T): T =
  BEGIN
    t.root := t; t.next := t; t.minHt := t;
    t.parent := NIL;
    t.id := idCntr;
    INC(idCntr);
    t.size := 1;
    t.parentSize := 0;
    t.modTime := 0;
    t.props := SET OF Prop{};
    t.misc := NIL;
    RETURN t
  END InitT;

PROCEDURE GetId(t: T): INTEGER =
  BEGIN RETURN t.root.id END GetId;
    
PROCEDURE GetFP(t: T): FPrint.T =
  BEGIN RETURN t.fp END GetFP;

PROCEDURE GetMisc(t: T; alloc := TRUE): Misc =
  BEGIN
    IF alloc AND t.misc = NIL THEN
      t.misc := NEW(Misc);
      UndoStackPush(UndoType.Misc, t)
    END (* IF *);
    RETURN t.misc
  END GetMisc;

VAR sigTab: SigTab.T;

(* Q0: "sigTab" maps each signature to the {\em root} of the
   congruence class associated with that signature; that is, the
   unique parent node with that signature whose "cgPtr" field points
   to itself. *)

PROCEDURE IsCar(e: T): BOOLEAN =
  (* REQUIRES e.parent # NIL AND e = e.root *)
  BEGIN RETURN e = e.parent.car.root END IsCar;

PROCEDURE Root(e: T): T =
  BEGIN RETURN e.root END Root;

PROCEDURE Cons(e, f: T): T =
  (* Return an enode whose "car" is equivalent to "e", and whose
     "cdr" is equivalent to "f", creating it if necessary. *)
  VAR res: Parent; resRA: REFANY; BEGIN
    e := e.root; f := f.root;
    IF sigTab.get(Signature.T{e.id, f.id}, resRA) THEN
      res := resRA
    ELSE
      TYPECASE e OF
      | FSym(fs) =>
          res := NEW(CarParent).init(e, f);
          UndoStackPush(UndoType.Cons, res);
          TermList_AppendToRear(fs, res);
          NARROW(res, CarParent).childLabels :=
              PairSet.Singleton(fs.shortId());
          <*ASSERT fs.terms # NIL*>
          IF Orders.IsRel(fs.sym) THEN
            res.props := res.props + HasOrdTermProp
          END (* IF *);
          IF PredDefs.IsPredSym(fs.sym) THEN
            res.props := res.props + HasPredTermProp
          ELSIF PredDefs.IsPredMapSym(fs.sym) THEN
            res.props := res.props + HasPredMapTermProp
          END (* IF *);
          IF fs.sym = PredSx.selectSym THEN
            VAR f2 := f; BEGIN
              REPEAT
                TYPECASE f2 OF
                | Parent(fp) =>
                    IF Prop.HasPredMapTerm IN fp.car.root.props THEN
                      res.props := res.props + HasSelPredMapTermProp;
                      EXIT
                    END (* IF *)
                ELSE
                END (* TYPECASE *);
                f2 := f2.next
              UNTIL f2 = f
            END (* BEGIN *)
          END (* IF *);
          INC(stats.nCarPars);
          stats.maxCarPars := MAX(stats.maxCarPars, stats.nCarPars)
      ELSE          
          res := NEW(CdrParent).init(e, f);
          UndoStackPush(UndoType.Cons, res);
      END (* TYPECASE *);
      EVAL sigTab.put(Signature.T{e.id, f.id}, res);
      INC(Prover.actTestNumber);
      IF Prover.noActivate OR
          (Prover.forceActivateStart <= Prover.actTestNumber AND
           Prover.actTestNumber < Prover.forceActivateEnd) THEN
        IF Prover.forceActivateStart <= Prover.actTestNumber THEN
           Wr.PutText(Stdio.stdout, "Forcing activation of ");
	   Sx.Print(Stdio.stdout, DbgToSx(res));
           Wr.PutText(Stdio.stdout, "\n");
           Wr.Flush(Stdio.stdout);
        END;
        Activate(res)
      END (* IF *);
      INC(stats.nCons); stats.maxCons := MAX(stats.maxCons, stats.nCons);
      stats.maxTotnodes := MAX(stats.maxTotnodes, stats.nLeaf + stats.nCons);
      IF Prop.Active IN res.props THEN
        Context.opsEnabled[Context.Ops.UnitMatch] := TRUE;
        Context.opsEnabled[Context.Ops.RestrictedNUMatch] := TRUE
      END (* IF *)
    END (* IF *);
    RETURN res
  END Cons;

<*UNUSED*>
PROCEDURE Valid(e: T): BOOLEAN =
  VAR ra: REFANY; BEGIN
    TYPECASE e OF <*NOWARN*>
    | Leaf(lf) =>
        VAR at: Atom.T; BEGIN
          TYPECASE lf.sym OF <*NOWARN*>
          | NULL => RETURN lf = enil
          | Atom.T(at2) => at := at2
          | REF INTEGER(ri) => at := Atom.FromText(Fmt.Int(ri^))
          | REF LONGREAL(rlr) => at := Atom.FromText(Fmt.LongReal(rlr^))
          END (* TYPECASE *);
          RETURN symTab.get(at, ra) AND ra = lf
        END (* BEGIN *)
    | Parent(p) =>
        RETURN sigTab.get(Signature.T{p.car.root.id, p.cdr.root.id}, ra)
               AND NARROW(ra, T).root = p.root
    END (* TYPECASE *)
  END Valid;

PROCEDURE MakePreInterned(n: T) =
  BEGIN
    IF NOT Prop.PreInterned IN n.props THEN
      n.props := n.props + PreInternedProp;
      UndoStackPush(UndoType.PreInterned, n);
      VAR nrw := GetChildLabels(n.root);
          fw := PairSet.Singleton(n.shortId());
          new := nrw + fw;
      BEGIN
        SetChildLabels(n.root, new);
        IF NOT Prover.noPatElems THEN
          PairSet.AddSetCrossElemD(unitPcPairSet,
                                   GetParentLabels(n.root),
                                   n.shortId())
        END (* IF *)
      END (* BEGIN *)
    END (* IF *)
  END MakePreInterned;

PROCEDURE Activate(n: T) =
  BEGIN ActivateWork(n.root);
  END Activate;

PROCEDURE Cdr(e: T): T =
  VAR p: Parent := e; BEGIN
    RETURN p.cdr
  END Cdr;

PROCEDURE Car(e: T): T =
  VAR p: Parent := e; BEGIN
    RETURN p.car
  END Car;

PROCEDURE SelStoreDist(): BOOLEAN =
  VAR sel: Parent := SelectFSym.parent; res := FALSE; BEGIN
    IF sel # NIL THEN
      REPEAT
	IF sel.cgPtr = sel THEN
	  VAR st := Car(Cdr(sel)).root;
	      i := Car(Cdr(Cdr(sel))).root;
	      dumCdr := Cdr(Cdr(Cdr(sel)));
	  BEGIN
            IF StoreIdSet <= GetChildLabels(st) THEN
	      VAR steq := st; BEGIN
		REPEAT
		  IF ISTYPE(steq, CarParent) AND Car(steq) = StoreFSym THEN
		    VAR jPar := Cdr(Cdr(steq));
                        j := Car(jPar).root;
                    BEGIN
		      IF MergeIsForbidden(i, j) THEN
			VAR m := Car(Cdr(steq));
			    newSel := Cons(SelectFSym,
					   Cons(m, Cons(i, dumCdr)));
			BEGIN
			  IF sel.root # newSel.root THEN
			    res := TRUE;
			    ContextPrivate.Propagate(NewEq(sel, newSel))
			  END (* IF *)
			END (* BEGIN *)
		      ELSIF i.root = j.root THEN
			VAR x := Car(Cdr(jPar)); BEGIN
			  IF sel.root # x.root THEN
			    res := TRUE;
			    ContextPrivate.Propagate(NewEq(sel, x))
			  END (* IF *)
			END (* BEGIN *)
		      END (* IF *)
		    END (* BEGIN *)
		  END (* IF *);
		  steq := steq.next
		UNTIL steq = st
	      END (* BEGIN *)
	    END (* IF *)
	  END (* BEGIN *)
	END (* IF *);
	sel := sel.same[TRUE]
      UNTIL sel = SelectFSym.parent
    END (* IF *);
    RETURN res
  END SelStoreDist;

PROCEDURE ActivateWork(n: T) =
  BEGIN
    IF Prop.Active IN n.props THEN RETURN END;

    UndoStackPush(UndoType.Activate, n);
    n.props := n.props + ActiveProp;
    INC(stats.nActive);
    stats.maxActive := MAX(stats.maxActive, stats.nActive);

    VAR neq := n; BEGIN
      REPEAT
        IF NOT ContextPrivate.inD1P AND neq.modTime # timer THEN
          TYPECASE neq OF
          | CarParent(cp) =>
              IF NARROW(cp.car, FSym).terms # cp THEN
                UndoStackPush(UndoType.UpdateMTAndMoveToFront,
                              cp.prev, cp.modTime);
                <*ASSERT cp.prev # cp *>
                TermList_MoveToFront(cp.car, cp)
              ELSE
                UndoStackPush(UndoType.UpdateMT, neq, neq.modTime)
              END (* IF *)
          ELSE
              UndoStackPush(UndoType.UpdateMT, neq, neq.modTime)
          END (* TYPECASE *);
          neq.modTime := timer
        END (* IF *);
        TYPECASE neq OF
        | Parent(p) =>
            ActivateWork(p.car); ActivateWork(p.cdr)
        ELSE
        END (* TYPECASE *);
        IF NOT ContextPrivate.inD1P THEN
          IF Prover.lazySimplexIntern THEN SimplexIntern(neq) END (* IF *);
          TYPECASE neq OF
            CarParent (par) =>
              IF par.cgPtr = par THEN
                UpdatePairsAndBitsOfArgs(par);
                IF NOT Prover.noPatElems THEN
                  ParentSet.AddParentD(unitPSet, par.car)
                END (* IF *)
              END (* IF *)
          ELSE (*SKIP*)
          END (* TYPECASE *)
        END (* IF *);
        neq := neq.next
      UNTIL neq = n;
      IF NOT ContextPrivate.inD1P AND NOT Prover.noModTimes THEN
        SetModTimes(neq)
      END (* IF *)
    END (* BEGIN *);
    IF NOT ContextPrivate.inD1P THEN
      Context.opsEnabled[Context.Ops.UnitMatch] := TRUE;
      Context.opsEnabled[Context.Ops.RestrictedNUMatch] := TRUE
    END (* IF *)
  END ActivateWork;

PROCEDURE UpdatePairsAndBitsOfArgs(par: CarParent) =
  VAR f: FSym := par.car;
      fid := f.shortId();
      fw := PairSet.Singleton(fid);
      args := par.cdr;
  BEGIN
    WHILE args # enil DO
      VAR argsP: CdrParent := args; BEGIN
        VAR arg := argsP.car.root;
            argPars := GetParentLabels(arg);
            argChildren := GetChildLabels(arg);
        BEGIN
          IF NOT Prover.noPatElems THEN
            PairSet.AddElemCrossSetD(unitPcPairSet, fid, argChildren);
            PairSet.AddElemCrossSetD(unitPpPairSet, fid, argPars)
          END (* IF *);
          SetParentLabels(arg, argPars + fw)
        END (* BEGIN *);
        args := argsP.cdr
      END (* BEGIN *)
    END (* WHILE *)
  END UpdatePairsAndBitsOfArgs;

PROCEDURE GetParentLabels(x: T): IdSet.T =
  BEGIN
    TYPECASE x OF <*NOWARN*>
    | NonFSym(nfs) => RETURN nfs.parentLabels
    | CarParent(cp) => RETURN cp.parentLabels
    END (* TYPECASE *)
  END GetParentLabels;

PROCEDURE SetParentLabels(x: T; i: IdSet.T; undo := TRUE) =
  BEGIN
    TYPECASE x OF <*NOWARN*>
    | NonFSym(nfs) =>
        IF nfs.parentLabels # i THEN
	  IF undo THEN
	    UndoStackPush(UndoType.ParentLabels, nfs);
            pSetStack.addhi(nfs.parentLabels)
	  END (* IF *);
	  nfs.parentLabels := i
        END (* IF *)
    | CarParent(cp) =>
        IF cp.parentLabels # i THEN
	  IF undo THEN
	    UndoStackPush(UndoType.ParentLabels, cp);
            pSetStack.addhi(cp.parentLabels)
	  END (* IF *);
	  cp.parentLabels := i
        END (* IF *)
    END (* TYPECASE *)
  END SetParentLabels;

(* Requires that "x" is a root of a term-reprenting class; returns the
   child label set of "x". *)
PROCEDURE GetChildLabels(x: T): IdSet.T =
  BEGIN
    TYPECASE x OF <*NOWARN*>
    | NonFSym(nfs) => RETURN nfs.childLabels
    | CarParent(cp) => RETURN cp.childLabels
    END (* TYPECASE *)
  END GetChildLabels;

(* Requires that "x" is a root of a term-reprenting class; sets the
   child label set of "x" to be "x".  This change is undoable iff
   "undo" is "TRUE". *)
PROCEDURE SetChildLabels(x: T; i: IdSet.T; undo := TRUE) =
  BEGIN
    TYPECASE x OF <*NOWARN*>
    | NonFSym(nfs) =>
        IF nfs.childLabels # i THEN
	  IF undo THEN
	    UndoStackPush(UndoType.ChildLabels, nfs);
            cSetStack.addhi(nfs.childLabels)
	  END (* IF *);
	  nfs.childLabels := i
        END (* IF *)
    | CarParent(cp) =>
        IF cp.childLabels # i THEN
	  IF undo THEN
	    UndoStackPush(UndoType.ChildLabels, cp);
            cSetStack.addhi(cp.childLabels)
	  END (* IF *);
	  cp.childLabels := i
        END (* IF *)
    END (* TYPECASE *)
  END SetChildLabels;

PROCEDURE SimplexIntern(n: T) =
  BEGIN
    TYPECASE n OF
    | Parent(p) =>
        IF p.misc = NIL OR p.misc.unknown = NIL THEN
          TYPECASE p.car OF
          | FSym(fs) =>
              IF fs = plusFS OR fs = minusFS OR fs = timesFS THEN
		VAR b: BOOLEAN;
		    cdr: Parent := p.cdr;
		    cddr: Parent := cdr.cdr;
		    arg1 := cdr.car;
		    arg2 := cddr.car;
		BEGIN
		  IF fs.sym = PredSx.plusSym THEN
		    b := Simplex.IsSum(p, arg1, arg2)
		  ELSIF fs.sym = PredSx.minusSym THEN
		    b := Simplex.IsDiff(p, arg1, arg2)
		  ELSIF fs.sym = PredSx.timesSym THEN
		    b := Simplex.IsProd(p, arg1, arg2)
		  ELSE
		    <*ASSERT FALSE*>
		  END (* IF *);
		END (* BEGIN *)
	      END (* IF *)
	  ELSE
	  END (* TYPECASE *)
	END (* IF *)
    ELSE
    END (* TYPECASE *)
  END SimplexIntern;

PROCEDURE InitLeaf(self: Leaf; sym: REFANY): Leaf =
  BEGIN
    EVAL T.init(self);
    self.sym := sym;
    self.ht := 1;
    TYPECASE self.sym OF <*NOWARN*>
    | NULL =>
        self.fp := FPrint.Zero
    | Atom.T(at) =>
        self.fp := FPrint.FromText(Atom.ToText(at))
    | REF INTEGER(ri) =>
        self.fp := FPrint.FromInt(ri^)
    | REF LONGREAL(rlr) =>
        self.fp := FPrint.FromLongReal(rlr^)
    END (* TYPECASE *);
    RETURN self
  END InitLeaf;

PROCEDURE InitFSym(self: FSym; sym: REFANY): FSym =
  BEGIN
    EVAL InitLeaf(self, sym);
    self.terms := NIL;
    RETURN self
  END InitFSym;

(* Requires that "e = e.root" and "f = f.root". *)
PROCEDURE InitParent(self: Parent; e, f: T): Parent =
  BEGIN
    EVAL T.init(self);
    self.car := e;
    IF e.parent = NIL THEN
      e.parent := self;
      self.same[TRUE] := self
    ELSE
      self.same[TRUE] := e.parent.same[TRUE];
      e.parent.same[TRUE] := self
    END (* IF *);
    INC(e.parentSize);
    self.cdr := f;
    IF f.parent = NIL THEN
      f.parent := self;
      self.same[FALSE] := self
    ELSE
      self.same[FALSE] := f.parent.same[FALSE];
      f.parent.same[FALSE] := self
    END (* IF *);
    INC(f.parentSize);
    self.cgPtr := self;

    self.ht := MAX(e.minHt.ht, f.minHt.ht) + 1;
    self.fp := FPrint.Combine(e.minHt.fp, f.minHt.fp);
    RETURN self
  END InitParent;

PROCEDURE InitCarParent(self: CarParent; e, f: T): CarParent =
  BEGIN
    EVAL InitParent(self, e, f);
    (*
    VAR fsym: FSym := self.car; BEGIN
      CheckWellFormed(fsym.terms)
    END (* BEGIN *);
    *)
    self.link := NIL;
    self.prev := NIL;
    (*
    VAR fsym: FSym := self.car; BEGIN
      CheckWellFormed(fsym.terms)
    END (* BEGIN *);
    *)
    RETURN self
  END InitCarParent;

REVEAL
  Equality = AF.T BRANDED OBJECT
    e, f: T
   OVERRIDES
    assert := AssertEq;
    equalInContext := EqualityEqualInContext;
    hashInContext := EqualityHashInContext;
    toSx := EqToSx;
    fingerprint := EqFP;
  END (* OBJECT *);

VAR
  identTab: SigTab.T;

PROCEDURE NewEq(e, f: T; sense := TRUE): AF.Lit =
  VAR af: Equality; afRA: REFANY; BEGIN
    IF e.root = f.root THEN
      IF Prover.eqOptActivate THEN Activate(e); Activate(f) END;
      IF sense THEN RETURN AF.trueLit
      ELSE RETURN AF.falseLit
      END (* IF *)
    ELSIF MergeIsForbidden(e.root, f.root) THEN
      IF Prover.eqOptActivate THEN Activate(e); Activate(f) END;
      IF sense THEN RETURN AF.falseLit
      ELSE RETURN AF.trueLit
      END (* IF *)
    ELSIF FPGT(e.fp, f.fp) THEN
      VAR tmp := e; BEGIN e := f; f := tmp END (* BEGIN *)
    END (* IF *);
    VAR sig := Signature.T{e.root.id, f.root.id}; BEGIN
      IF identTab.get(sig, afRA) THEN
        af := afRA
      ELSE
        af := NEW(Equality, e := e, f := f).init();
        EVAL identTab.put(sig, af);
        UndoStackPush(UndoType.Ident, af)
      END (* IF *);
    END (* BEGIN *);
    RETURN NEW(AF.Lit, af := af, sense := sense);
  END NewEq;

TYPE
  DistClassTab = RefRefTbl.Default OBJECT
   OVERRIDES
    keyEqual := TermListEqual;
    keyHash := TermListHash;
  END (* OBJECT *);

REVEAL
  DistClass = AF.T BRANDED OBJECT
    terms: RefList.T;
   OVERRIDES
    assert := AssertDistClass;
    toSx := DistClassToSx;
    fingerprint := DistClassFP;
  END (* OBJECT *);

PROCEDURE TermListEqual(<*UNUSED*> tbl: DistClassTab;
                        READONLY k1, k2: REFANY): BOOLEAN =
  VAR k1l: RefList.T := k1; k2l: RefList.T := k2; BEGIN
    WHILE k1l # NIL AND k2l # NIL DO
      IF k1l.head # k2l.head THEN RETURN FALSE
      ELSE k1l := k1l.tail; k2l := k2l.tail
      END (* IF *)
    END (* WHILE *);
    RETURN k1l = NIL AND k2l = NIL
  END TermListEqual;

PROCEDURE TermListHash(<*UNUSED*> tbl: DistClassTab;
                       READONLY k: REFANY): Word.T =
  VAR kl: RefList.T := k; res: Word.T := 0; BEGIN
    WHILE kl # NIL DO
      VAR hd: T := kl.head; BEGIN
        res := Word.Plus(res, FPrint.Hash(hd.fp))
      END (* BEGIN *);
      kl := kl.tail
    END (* WHILE *);
    RETURN res
  END TermListHash;

PROCEDURE TermCompare(e1RA, e2RA: REFANY): [-1..1] =
  VAR e1: T := e1RA; e2: T := e2RA; BEGIN
    IF FPGT(e1.fp, e2.fp) THEN RETURN 1
    ELSIF FPGT(e2.fp, e1.fp) THEN RETURN -1
    ELSE RETURN 0
    END (* IF *)
  END TermCompare;

VAR distClassTab: DistClassTab;

PROCEDURE NewDist(terms: RefList.T; sense: BOOLEAN): AF.Lit =
  VAR afRA: REFANY; af: DistClass; BEGIN
    terms := RefListSort.SortD(terms, TermCompare);
    IF distClassTab.get(terms, afRA) THEN
      af := afRA
    ELSE
      af := NEW(DistClass, terms := terms).init();
      EVAL distClassTab.put(terms, af);
      UndoStackPush(UndoType.DistClass, af)
    END (* IF *);
    RETURN NEW(AF.Lit, af := af, sense := sense)
  END NewDist;

TYPE
  UndoType = {Merge, MergeSimpUnknown, MergeOrdUnknown, MergeSimpOrdUnknown, 
              ForbidMerge, Cons, Leaf, Ident, DistClass, Misc,
              JoinDistClass, IncDistClasses,
              TLAppendToRear, UpdateMT, UpdateMTAndMoveToFront,
              Activate, PatternMatched, HasOrdTerm,
              IsPredSym, HasPredTerm, HasPredMapTerm, HasSelPredMapTerm,
              DiffFromTrue, AddMatchFP,
              PreInterned, ParentLabels, ChildLabels,
              Mark};
  UndoRec = RECORD
    type: UndoType;
    data: REFANY := NIL;
    i := 0;
  END (* RECORD *);
  UndoStack = REF ARRAY OF UndoRec;
  UndoStatArr = ARRAY UndoType OF CARDINAL;
CONST
  UndoTypeText = ARRAY UndoType OF TEXT{
    "Merge", "MergeSimpUnknown", "MergeOrdUnknown", "MergeSimpOrdUnknown", 
    "ForbidMerge", "Cons", "Leaf", "Ident", "DistClass", "Misc",
    "JoinDistClass", "IncDistClasses",
    "TLAppendToRear", "UpdateMT", "UpdateMTAndMoveToFront",
    "Activate", "PatternMatched", "HasOrdTerm",
    "IsPredSym", "HasPredTerm", "HasPredMapTerm", "HasSelPredMapTerm",
    "DiffFromTrue", "AddMatchFP", "PreInterned", "ParentLabels", "ChildLabels",
    "Mark"};
VAR
  undoStack: UndoStack;
  undoFP: FPSeq.T;
  undoSP: CARDINAL;
  nullUndoRec := UndoRec{type := VAL(0, UndoType), data := NIL, i := 0};
  undoStats: UndoStatArr;
CONST
  MinUndoStackSize = 100;
VAR
  quarterSize: CARDINAL;

PROCEDURE UndoStackPush(type: UndoType;
                        data: REFANY := NIL; i := 0) =
  BEGIN
    INC(undoStats[type]);
    IF undoSP = NUMBER(undoStack^) THEN
      VAR new := NEW(UndoStack, 2 * undoSP); BEGIN
        SUBARRAY(new^, 0, undoSP) := undoStack^;
        undoStack := new;
        quarterSize := NUMBER(undoStack^) DIV 4;
      END (* BEGIN *)
    END (* IF *);
    undoStack[undoSP].type := type;
    undoStack[undoSP].data := data;
    undoStack[undoSP].i := i;
    INC(undoSP);
  END UndoStackPush;
  
PROCEDURE EqToSx(self: Equality; normForm := FALSE): REFANY =
  BEGIN
    IF normForm THEN
      RETURN RefList.List3(PredSx.eqSym, ToSx(self.e), ToSx(self.f))
    ELSE
      RETURN RefList.List3(PredSx.eqSym, DbgToSx(self.e), DbgToSx(self.f))
    END (* IF *)
  END EqToSx;

PROCEDURE EqFP(self: Equality): FPrint.T =
  BEGIN RETURN FPrint.Combine(FingerP(self.e), FingerP(self.f))
  END EqFP;

PROCEDURE DistClassToSx(self: DistClass; normForm: BOOLEAN): REFANY =
  VAR args: RefList.T := NIL; ts: RefList.T := self.terms; BEGIN
    WHILE ts # NIL DO
      IF normForm THEN
        args := RefList.Cons(ToSx(ts.head), args)
      ELSE
        args := RefList.Cons(DbgToSx(ts.head), args)
      END (* IF *);
      ts := ts.tail
    END (* WHILE *);
    RETURN RefList.Cons(PredSx.distClassSym, args)
  END DistClassToSx;

PROCEDURE DistClassFP(self: DistClass): FPrint.T =
  VAR res := FPrint.Zero;
      terms := self.terms;
  BEGIN
    WHILE terms # NIL DO
      res := FPrint.Combine(res, FingerP(terms.head));
      terms := terms.tail
    END (* WHILE *);
    RETURN res
  END DistClassFP;

PROCEDURE DbgToSx(e: T): REFANY =
  BEGIN
    IF e = NIL THEN RETURN NIL END;
    TYPECASE e OF <*NOWARN*>
    | Leaf(l) =>
        RETURN l.sym;
    | Parent(p) =>
        RETURN RefList.Cons(DbgToSx(p.car), DbgToSx(p.cdr))
    END (* TYPECASE *)
  END DbgToSx;

PROCEDURE AssertEq(eq: Equality; plit: AF.Lit): BOOLEAN =
  PROCEDURE AddNeqNode(e, f: T) =
    VAR neqNode := Cons(eNeq, Cons(e, Cons(f, enil)));
        b: BOOLEAN;
    BEGIN
      IF plit.activate THEN Activate(neqNode) END (* IF *);
      IF neqNode.root # eTrue.root THEN
        b := MergeCC(neqNode.root, eTrue.root, FALSE); <*ASSERT b *>
      END (* IF *)
    END AddNeqNode;
  VAR x, y: T; BEGIN
    x := eq.e.root; y := eq.f.root;
    Activate(x); Activate(y);
    IF plit.sense THEN
      IF x = y THEN
        RETURN TRUE
      ELSE
        RETURN MergeCC(x, y, plit.rightMost)
      END (* IF *)
    ELSE
      IF ForbidMergeEqTest(x, y) THEN
        RETURN FALSE
      ELSIF NOT DoPredDefs(x, y, FALSE, plit.rightMost) THEN <*NOWARN*>
        VAR res := ForbidMerge(x, y); BEGIN
          IF res AND x.root # eTrue.root AND y.root # eTrue.root THEN
            AddNeqNode(x, y); AddNeqNode(y, x)
          END (* IF *);
          RETURN res
        END (* BEGIN *)
      ELSE
        RETURN TRUE
      END (* IF *)
    END (* IF *)
  END AssertEq;

(* Requires "x" and "y" to be distinct roots.
   If the equivalence classes of "x" and "y" have been forbidden to
   merge, returns "FALSE".  Otherwise, merges the equivalence classes
   of "x" and "y", possibly propagating new equalities, and returns
   "TRUE".
*)
PROCEDURE AssertEqPos(x, y: T; rm: BOOLEAN): BOOLEAN =
  BEGIN
    <*ASSERT x = x.root AND y = y.root AND x # y *>
    IF MergeIsForbidden(x, y) THEN RETURN FALSE END (* IF *);
    EVAL DoPredDefs(x, y, TRUE, rm); <*NOWARN*>
    IF Prop.Active IN x.props AND NOT Prop.Active IN y.props THEN
      Activate(y)
    ELSIF  Prop.Active IN y.props AND NOT Prop.Active IN x.props THEN
      Activate(x)
    END (* IF *); 

    Merge(x, y);
    RETURN TRUE
  END AssertEqPos;

TYPE TermListArr = ARRAY [0..Word.Size-1] OF RefList.T;
VAR distClassTerms: TermListArr;

PROCEDURE AssertDistClass(self: DistClass; plit: AF.Lit): BOOLEAN =
  BEGIN
    VAR trms := self.terms; BEGIN
      IF plit.activate THEN
        WHILE trms # NIL DO
          Activate(trms.head); trms := trms.tail
        END (* WHILE *)
      END (* IF *)
    END (* BEGIN *);
    IF plit.sense THEN
      (* For a while... *)
      <*ASSERT nDistClasses < MaxDistClasses *>
      IF nDistClasses < MaxDistClasses AND
        RefList.Length(self.terms) >= Prover.minDistClassSize THEN
        (* Check that self.terms may be made distinct, and propogate
           merges with NEQ terms. *)
        distClassTerms[nDistClasses] := self.terms;
        VAR trms := self.terms; BEGIN
          WHILE trms # NIL DO
            VAR trm1: T := trms.head; rest := trms.tail; BEGIN
              WHILE rest # NIL DO
                VAR trm2: T := rest.head; BEGIN
                  IF trm1.root = trm2.root THEN
                    RETURN FALSE
                  ELSIF Prover.distClassNeqNodes THEN
                    (* $$$ *)
                    ContextPrivate.Propagate(
                        NewEq(Cons(eNeq, Cons(trm1, Cons(trm2, enil))), eTrue));
                    ContextPrivate.Propagate(
                        NewEq(Cons(eNeq, Cons(trm2, Cons(trm1, enil))), eTrue))
                  END (* IF *)
                END (* BEGIN *);
                rest := rest.tail
              END (* WHILE *)
            END (* BEGIN *);
            trms := trms.tail
          END (* WHILE *);
          (* Set the appropriate distinction class bits. *)
          trms := self.terms;
          WHILE trms # NIL DO
            VAR trm: T := trms.head;
                misc := trm.root.getMisc();
            BEGIN
              misc.distClasses := misc.distClasses + DistClassSet{nDistClasses};
              UndoStackPush(UndoType.JoinDistClass, trm.root, nDistClasses)
            END (* BEGIN *);
            trms := trms.tail
          END (* WHILE *);
          INC(nDistClasses);
          UndoStackPush(UndoType.IncDistClasses);
          RETURN TRUE
        END (* BEGIN *)
      ELSE
        (* Do the n(n-1)/2 distinctions... *)
        VAR trms := self.terms; BEGIN
          WHILE trms # NIL DO
            VAR trm1: T := trms.head; rest := trms.tail; BEGIN
              WHILE rest # NIL DO
                VAR trm2: T := rest.head; BEGIN
                  IF NOT ForbidMerge(trm1.root, trm2.root) THEN
                    RETURN FALSE
                  END (* IF *)
                END (* BEGIN *);
                rest := rest.tail
              END (* WHILE *)
            END (* BEGIN *);
            trms := trms.tail
          END (* WHILE *);
          RETURN TRUE
        END (* BEGIN *)
      END (* IF *)
    ELSE
      (* Add a clause to "l" asserting that some pair of terms is equivalent. *)
      VAR trms := self.terms; lits: AF.LitList := NIL; BEGIN
        WHILE trms # NIL DO
          VAR trm1: T := trms.head; rest := trms.tail; BEGIN
            WHILE rest # NIL DO
              VAR trm2: T := rest.head; BEGIN
                lits := RefList.Cons(NewEq(trm1, trm2, TRUE), lits)
              END (* BEGIN *);
              rest := rest.tail
            END (* WHILE *)
          END (* BEGIN *);
          trms := trms.tail
        END (* WHILE *);
        ContextPrivate.AddClause(NEW(Clause.T, lits := lits).init());
        RETURN TRUE
      END (* BEGIN *)
    END (* IF *)
  END AssertDistClass;

PROCEDURE EqualityEqualInContext(eq1: Equality; af2: AF.T): BOOLEAN =
  BEGIN
    TYPECASE af2 OF
    | Equality(eq2) =>
        VAR e1 := eq1.e.root; f1 := eq1.f.root;
            e2 := eq2.e.root; f2 := eq2.f.root;
        BEGIN
          IF e1.id > f1.id THEN
            VAR tmp := e1; BEGIN e1 := f1; f1 := tmp; END (* BEGIN *)
          END (* IF *);
          IF e2.id > f2.id THEN
            VAR tmp := e2; BEGIN e2 := f2; f2 := tmp; END (* BEGIN *)
          END (* IF *);
          RETURN e1 = e2 AND f1 = f2
        END (* BEGIN *)
    ELSE
        RETURN FALSE
    END (* TYPECASE *)
  END EqualityEqualInContext;

PROCEDURE EqualityHashInContext(eq: Equality): Word.T =
  BEGIN RETURN Word.Xor(eq.e.root.id, eq.f.root.id)
  END EqualityHashInContext;

VAR eNeq: Leaf;
    SelectFSym, StoreFSym: FSym;
    StoreIdSet: IdSet.T;
  
PROCEDURE ForbidMerge(e, f: T): BOOLEAN =
  (* Requires "e" and "f" to be roots.  If "e" and "f" are equivalent,
     returns "FALSE"; otherwise, forbids the merge of "e" and "f" in
     the egraph and returns "TRUE". *)
  PROCEDURE AddForbid(e, f: T) =
    VAR tl := NEW(TList, t := f); BEGIN
      IF e.misc = NIL OR e.misc.forbids = NIL THEN
        e.getMisc().forbids := tl; tl.next := tl
      ELSE
        tl.next := e.misc.forbids.next;
        e.misc.forbids.next := tl
      END (* IF *);
      IF f = eTrue.root AND NOT Prop.DiffFromTrue IN e.props THEN
        e.props := e.props + DiffFromTrueProp;
        UndoStackPush(UndoType.DiffFromTrue, e)
      END (* IF *)
    END AddForbid;
  BEGIN
    IF ForbidMergeEqTest(e, f) THEN
      RETURN FALSE
    ELSE
      AddForbid(e, f); AddForbid(f, e);
      UndoStackPush(type := UndoType.ForbidMerge, data := e);
      RETURN TRUE
    END (* IF *)
  END ForbidMerge;

PROCEDURE ForbidMergeEqTest(e, f: T): BOOLEAN = 
  BEGIN RETURN e = f OR
               (e.misc # NIL AND e.misc.unknown # NIL AND
                f.misc # NIL AND f.misc.unknown # NIL AND
                Simplex.UnknownEqual(e.misc.unknown, f.misc.unknown))
  END ForbidMergeEqTest;

VAR pushDepth := 0;

TYPE RefPairSet = REF PairSet.T;

PROCEDURE Push() =
  BEGIN
    IF NOT Prover.noPatElems THEN
      IF PairSet.IsEmpty(unitPcPairSet) THEN
	unitPcUndoStack.addhi(NIL)
      ELSE
	VAR rps := NEW(RefPairSet); BEGIN
	  rps^ := unitPcPairSet;
	  unitPcUndoStack.addhi(rps)
	END (* BEGIN *)
      END (* IF *);
      IF PairSet.IsEmpty(nuPcPairSet) THEN
	nuPcUndoStack.addhi(NIL)
      ELSE
	VAR rps := NEW(RefPairSet); BEGIN
	  rps^ := nuPcPairSet;
	  nuPcUndoStack.addhi(rps)
	END (* BEGIN *)
      END (* IF *);
      IF PairSet.IsEmpty(unitPpPairSet) THEN
	unitPpUndoStack.addhi(NIL)
      ELSE
	VAR rps := NEW(RefPairSet); BEGIN
	  rps^ := unitPpPairSet;
	  unitPpUndoStack.addhi(rps)
	END (* BEGIN *)
      END (* IF *);
      IF PairSet.IsEmpty(nuPpPairSet) THEN
	nuPpUndoStack.addhi(NIL)
      ELSE
	VAR rps := NEW(RefPairSet); BEGIN
	  rps^ := nuPpPairSet;
	  nuPpUndoStack.addhi(rps)
	END (* BEGIN *)
      END (* IF *);
      unitPUndoStack.addhi(unitPSet);
      nuPUndoStack.addhi(nuPSet)
    END (* IF *);
    IF pushDepth = 0 THEN InitUndoStack() END (* IF *);
    UndoStackPush(UndoType.Mark, NIL, timer);
    INC(pushDepth);
  END Push;
  
PROCEDURE Pop() =
  (* Restore "C" to its last saved state.  That is, set "C :=
     SC:hipop()". *)
  BEGIN
    IF NOT ContextPrivate.inD1P THEN
      INC(nPops);
      IF snapshotIntrvl # -1 AND nPops MOD snapshotIntrvl = 1 THEN
	EgraphStatSample()
      END (* IF *)
    END (* IF *);

    IF NOT Prover.noPatElems THEN
      VAR top: RefPairSet := unitPcUndoStack.remhi(); BEGIN
	IF top = NIL THEN
	  PairSet.MakeEmpty(unitPcPairSet)
	ELSE
	  unitPcPairSet := top^
	END (* IF *)
      END (* BEGIN *);
      VAR top: RefPairSet := nuPcUndoStack.remhi(); BEGIN
	IF top = NIL THEN
	  PairSet.MakeEmpty(nuPcPairSet)
	ELSE
	  nuPcPairSet := top^
	END (* IF *)
      END (* BEGIN *);
      VAR top: RefPairSet := unitPpUndoStack.remhi(); BEGIN
	IF top = NIL THEN
	  PairSet.MakeEmpty(unitPpPairSet)
	ELSE
	  unitPpPairSet := top^
	END (* IF *)
      END (* BEGIN *);
      VAR top: RefPairSet := nuPpUndoStack.remhi(); BEGIN
	IF top = NIL THEN
	  PairSet.MakeEmpty(nuPpPairSet)
	ELSE
	  nuPpPairSet := top^
	END (* IF *)
      END (* BEGIN *);
      unitPSet := unitPUndoStack.remhi();
      nuPSet := nuPUndoStack.remhi()
    END (* IF *);
    
    IF pushDepth = 1 THEN
      Init()
    ELSE
      LOOP
	DEC(undoSP);
	TRY
	  WITH top = undoStack[undoSP] DO
	    CASE top.type OF
	    | UndoType.Merge, UndoType.MergeSimpUnknown,
	      UndoType.MergeOrdUnknown, UndoType.MergeSimpOrdUnknown =>
		UnMerge(top.data, top.type)
	    | UndoType.ForbidMerge =>
		UnForbid(top.data)
	    | UndoType.Leaf =>
		VAR l: Leaf := top.data; dumRA: REFANY; at: Atom.T; BEGIN
		  TYPECASE l.sym OF <*NOWARN*>
		  | Atom.T(at2) => at := at2;
		  | REF INTEGER(ri) => at := Atom.FromText(Fmt.Int(ri^))
		  | REF LONGREAL(rlr) => at := Atom.FromText(Fmt.LongReal(rlr^))
		  END (* TYPECASE *);
		  EVAL symTab.delete(at, dumRA); DEC(stats.nLeaf)
		END (* BEGIN *)
	    | UndoType.Cons =>
		VAR par: Parent := top.data;
		    e := par.car; f := par.cdr;
		BEGIN
		  IF par.same[TRUE] = par THEN
		    e.parent := NIL
		  ELSE
		    e.parent.same[TRUE] := e.parent.same[TRUE].same[TRUE]
		  END (* IF *);
		  DEC(e.parentSize);
		  IF par.same[FALSE] = par THEN
		    f.parent := NIL
		  ELSE
		    f.parent.same[FALSE] := f.parent.same[FALSE].same[FALSE]
		  END (* IF *);
		  <*ASSERT par.cgPtr = par*>
		  DEC(f.parentSize);
		  VAR b: BOOLEAN; dum: REFANY; BEGIN
		    b := sigTab.delete(Signature.T{e.id, f.id}, dum);
		    <*ASSERT b*>
		  END (* BEGIN *);
		  (* $$$ *)
		  IF parentLeakDebug THEN
		    RTHeapDebug.Free(par)
		  END (* IF *);
		  DEC(stats.nCons);
		  IF ISTYPE(par, CarParent) THEN
		    DEC(stats.nCarPars)
		  END (* IF *)
		END (* BEGIN *);
	    | UndoType.Ident =>
		VAR eq: Equality := top.data; ra: REFANY;
		    sig := Signature.T{eq.e.root.id, eq.f.root.id};
		BEGIN
		  EVAL identTab.delete(sig, ra)
		END (* BEGIN *)
	    | UndoType.DistClass =>
		VAR dc: DistClass := top.data; ra: REFANY; BEGIN
		  EVAL distClassTab.delete(dc.terms, ra)
		END (* BEGIN *)
	    | UndoType.Misc =>
		NARROW(top.data, T).misc := NIL
	    | UndoType.JoinDistClass =>
		VAR e: T := top.data; cls: INTEGER := top.i; BEGIN
		  e.misc.distClasses := e.misc.distClasses - DistClassSet{cls};
		END (* BEGIN *)
	    | UndoType.IncDistClasses =>
		distClassTerms[nDistClasses] := NIL; DEC(nDistClasses)
	    | UndoType.Activate =>
		VAR e: T := top.data; BEGIN
		  e.props := e.props - ActiveProp;
		  DEC(stats.nActive)
		END (* BEGIN *)
	    | UndoType.PatternMatched =>
		NARROW(top.data, MatchingRule.PatObj).matched := FALSE
	    | UndoType.TLAppendToRear =>
		VAR p: CarParent := top.data; BEGIN
                  (*
                  VAR fsym: FSym := p.car; BEGIN
                    CheckWellFormed(fsym.terms)
                  END (* BEGIN *);
                  *)
		  IF p.link = p THEN
		    VAR fsym: FSym := p.car; BEGIN
		      fsym.terms := NIL
		    END (* BEGIN *)
		  ELSE
		    p.prev.link := p.link;
		    p.link.prev := p.prev;
		    p.link := NIL; p.prev := NIL;
		  END (* IF *);
                  (*
                  VAR fsym: FSym := p.car; BEGIN
                    CheckWellFormed(fsym.terms)
                  END (* BEGIN *)
                  *)
		END (* BEGIN *)
	    | UndoType.UpdateMT =>
		NARROW(top.data, T).modTime := top.i
	    | UndoType.UpdateMTAndMoveToFront =>
		VAR prevPar: CarParent := top.data;
		    fsym: FSym := prevPar.car;
		    headPar := fsym.terms;
		BEGIN
                  (*
                  CheckWellFormed(fsym.terms);
                  *)
                  headPar.modTime := top.i;
		  (* delete *)
		  headPar.prev.link := headPar.link;
		  headPar.link.prev := headPar.prev;
		  fsym.terms := headPar.link;
		  (*
		  CheckWellFormed(fsym.terms);
		  *)
		  (* re-insert *)
		  headPar.link := prevPar.link;
		  headPar.prev := prevPar;
		  prevPar.link.prev := headPar;
		  prevPar.link := headPar;
                  (*
                  CheckWellFormed(fsym.terms);
                  *)
		END (* BEGIN *)
	    | UndoType.IsPredSym =>
		VAR e: T := top.data; BEGIN
		  e.props := e.props - IsPredSymProp
		END (* BEGIN *)
	    | UndoType.HasOrdTerm =>
		VAR e: T := top.data; BEGIN
		  e.props := e.props - HasOrdTermProp
		END (* BEGIN *)
	    | UndoType.HasPredTerm =>
		VAR e: T := top.data; BEGIN
		  e.props := e.props - HasPredTermProp
		END (* BEGIN *)
	    | UndoType.HasPredMapTerm =>
		VAR e: T := top.data; BEGIN
		  e.props := e.props - HasPredMapTermProp
		END (* BEGIN *)
	    | UndoType.HasSelPredMapTerm =>
		VAR e: T := top.data; BEGIN
		  e.props := e.props - HasSelPredMapTermProp
		END (* BEGIN *)
	    | UndoType.DiffFromTrue =>
		VAR e: T := top.data; BEGIN
		  e.props := e.props - DiffFromTrueProp
		END (* BEGIN *)
	    | UndoType.AddMatchFP =>
                VAR ra: REFANY;
                    b := matchFPTable.delete(undoFP.remhi(), ra);
                BEGIN
                  <*ASSERT b*>
                END (* BEGIN *)
            | UndoType.PreInterned =>
		VAR e: T := top.data; BEGIN
		  e.props := e.props - PreInternedProp
		END (* BEGIN *)
            | UndoType.ParentLabels =>
		VAR e: T := top.data; BEGIN
                  SetParentLabels(e, pSetStack.remhi(), undo := FALSE)
		END (* BEGIN *)
            | UndoType.ChildLabels =>
		VAR e: T := top.data; BEGIN
                  SetChildLabels(e, cSetStack.remhi(), undo := FALSE)
		END (* BEGIN *)
	    | UndoType.Mark =>
		timer := top.i;
		IF parentLeakDebug THEN
		  Wr.PutText(Stdio.stdout, "Starting checkheap...");
		  Wr.Flush(Stdio.stdout);
		  RTHeapDebug.CheckHeap();
		  Wr.PutText(Stdio.stdout, "done.\n");
		  Wr.Flush(Stdio.stdout)
		END (* IF *);
		IF undoSP < quarterSize AND
		  NUMBER(undoStack^) > MinUndoStackSize THEN
		  VAR newSize := MAX(NUMBER(undoStack^) DIV 2, MinUndoStackSize);
		      new := NEW(UndoStack, newSize);
		  BEGIN
		    SUBARRAY(new^, 0, undoSP) := SUBARRAY(undoStack^, 0, undoSP);
		    undoStack := new;
		    quarterSize := NUMBER(undoStack^) DIV 4
		  END (* BEGIN *)
		END (* IF *);
                DEC(pushDepth);
		RETURN
	    END (* CASE *)
	  END (* WITH *)
	FINALLY
	  undoStack[undoSP] := nullUndoRec;
	END (* TRY *)
      END (* LOOP *)
    END (* IF *)
  END Pop;

PROCEDURE ReRoot() = BEGIN END ReRoot;

TYPE FSymProc = PROCEDURE(fsym: FSym);

PROCEDURE MapPredLabels(P: FSymProc; x: T; useCar: BOOLEAN) =
(* Call "P(f)" for each "FSym f" labelling a dag-predecessor of "x". 
   Requires that "useCar = (x is a term-representing node)",
   and that "x" is a root.  *)
  VAR p := x.parent; BEGIN
    IF p # NIL THEN
      REPEAT
        IF ISTYPE(p, CarParent) THEN
          P(p.car)
        ELSIF p = p.cgPtr THEN
          MapPredLabels(P, p, FALSE)
        END (* IF *);
        p := p.same[useCar]
      UNTIL p = x.parent 
    END
  END MapPredLabels;

VAR mergeStack := NEW(RefSeq.T).init(20);

PROCEDURE MergeCC(x, y: T; rm: BOOLEAN): BOOLEAN =
  (* Requires "x" and "y" to be roots.  Merges "x" and "y" if
     possible, as well as any enodes made congruent by this merge.
     If "rm" is "TRUE", the top-level merge is from a rightmost
     literal, and any resulting predicate instantiations are made
     rightmost.  Returns "FALSE" if any merge is forbidden;
     otherwise, returns, "TRUE".
  *)
  BEGIN
    <*ASSERT mergeStack.size() = 0 *>
    mergeStack.addhi(x); mergeStack.addhi(y);
    WHILE mergeStack.size() > 0 DO
      VAR x: T := mergeStack.remhi();
          y: T := mergeStack.remhi();
      BEGIN
        x := x.root; y := y.root;
        IF x # y AND NOT AssertEqPos(x, y, rm) THEN
          EVAL mergeStack.init(20);
          RETURN FALSE
        END (* IF *);
        rm := FALSE;
      END (* BEGIN *)
    END (* WHILE *);
    RETURN TRUE
  END MergeCC;

PROCEDURE Merge(x, y: T) =
  (* Requires "x" and "y" to be distinct roots whose merge has not
     been forbidden.  Merges the equivalence classes of "x" and "y",
     possibly propagating new equalities.
  *)
  VAR undoTag: UndoType; BEGIN
    IF x.size < y.size THEN
      VAR t := x; BEGIN x := y; y := t END
    END (* IF *);
    IF NOT Prover.noMinHt THEN
      IF x.minHt.ht > y.minHt.ht
        OR x.minHt.ht = y.minHt.ht AND FPGT(x.minHt.fp, y.minHt.fp) THEN
        VAR tmp := x.minHt; BEGIN
          x.minHt := y.minHt; y.minHt := tmp;
        END (* BEGIN *)
      END (* IF *)
    END (* IF *);
    undoTag := UndoType.Merge;
    IF y.misc # NIL AND y.misc.unknown # NIL THEN
      IF x.misc = NIL OR x.misc.unknown = NIL THEN
        undoTag := UndoType.MergeSimpUnknown
      ELSE
        ContextPrivate.Propagate(Simplex.NewEQ(x.misc.unknown, y.misc.unknown))
      END (* IF *)
    END (* IF *);
    IF y.misc # NIL AND y.misc.ordNode # NIL THEN
      IF x.misc = NIL OR x.misc.ordNode = NIL THEN
        IF undoTag = UndoType.MergeSimpUnknown THEN
          undoTag := UndoType.MergeSimpOrdUnknown
        ELSE
          undoTag := UndoType.MergeOrdUnknown
        END (* IF *)
      ELSE
        Orders.PropEqs(y.misc.ordNode, x.misc.ordNode)
      END (* IF *)
    END (* IF *);

    IF NOT ContextPrivate.inD1P THEN
      IF ISTYPE(x, NonFSym) OR ISTYPE(x, CarParent) THEN
	VAR px := GetParentLabels(x);
	    py := GetParentLabels(y);
	    cx := GetChildLabels(x);
	    cy := GetChildLabels(y);
	BEGIN
	  SetParentLabels(x, px + py);
	  SetChildLabels(x, cx + cy);
          IF NOT Prover.noPatElems THEN
            PairSet.AddSetCrossSetD(unitPcPairSet, px, cy);
            PairSet.AddSetCrossSetD(unitPcPairSet, py, cx);
            PairSet.AddSetCrossSetD(unitPpPairSet, px, py)
          END (* IF *)
	END (* BEGIN *)
      END (* IF *)
    END (* IF *);

    PropOrdersAsserts(x, y);
    MergeProp(x, y, Prop.HasPredTerm, UndoType.HasPredTerm);
    VAR xHasPredMapTerm := Prop.HasPredMapTerm IN x.props; BEGIN
      MergeProp(x, y, Prop.HasPredMapTerm, UndoType.HasPredMapTerm);
      IF Prop.HasPredMapTerm IN x.props THEN
        VAR z: T := NIL; BEGIN
          IF NOT xHasPredMapTerm THEN z := x
          ELSIF NOT Prop.HasPredMapTerm IN y.props THEN z := y
          END (* IF *);
          IF z # NIL THEN SetSelPredMaps(z) END (* IF *)
        END (* BEGIN *)
      END (* IF *)
    END (* BEGIN *);
    MergeProp(x, y, Prop.HasSelPredMapTerm, UndoType.HasSelPredMapTerm);
    MergeProp(x, y, Prop.DiffFromTrue, UndoType.DiffFromTrue);

    IF (x.misc = NIL OR x.misc.forbids = NIL) AND
      y.misc # NIL AND y.misc.forbids # NIL THEN
      x.getMisc().forbids := y.misc.forbids
    ELSIF y.misc # NIL AND y.misc.forbids # NIL THEN       
      VAR t := x.getMisc().forbids.next; BEGIN       
        x.misc.forbids.next := y.misc.forbids.next;  
        y.misc.forbids.next := t
      END (* BEGIN *)                      
    END (* IF *);                          
    VAR l: Parent; isCar: BOOLEAN; BEGIN
      IF x.parentSize < y.parentSize THEN
        l := x.parent;
        IF x.parent # NIL THEN isCar := IsCar(x) END (* IF *)
      ELSE
        l := y.parent;
        IF y.parent # NIL THEN isCar := IsCar(y) END (* IF *)
      END;
      (* A: y's class is no larger than x's *)
      (* B: l is the shorter of the parent lists of x and y, or
            y's parent list if the lengths are the same. *)
      IF l # NIL THEN
        VAR lp := l; BEGIN
          REPEAT
            IF lp.cgPtr = lp THEN SigRemove(lp) END (* IF *);
            lp := lp.same[isCar]
          UNTIL lp = l
        END (* BEGIN *)
      END (* IF *);
      (* C: No congruence class root on the circular list "l" is
            present in "sigTab". *)
      VAR yp := y; BEGIN
        REPEAT
          ReRoot(); (* $$$ *)
          yp.root := x;
          yp := yp.next
        UNTIL yp = y
      END;
      VAR t := x.next; BEGIN x.next := y.next; y.next := t END (* BEGIN *);
      INC(x.size, y.size);
      (* C, B *)
      IF x.parentSize < y.parentSize THEN
        VAR t := x.id; BEGIN x.id := y.id; y.id := t END (* BEGIN *)
      END (* IF *);
      IF l # NIL THEN
        VAR lp := l; BEGIN
          REPEAT
            IF lp.cgPtr = lp THEN SigInsert(lp) END (* IF *);
            lp := lp.same[isCar]
          UNTIL lp = l
        END (* BEGIN *)
      END (* IF *);

      (* prnts[x] = prnts[y] => prnts[x] = NIL, since
         parent lists of distinct roots are distinct. *)
      IF x.parent = NIL THEN
        x.parent := y.parent
      ELSIF y.parent # NIL THEN
        VAR t := x.parent.same[isCar]; BEGIN
          x.parent.same[isCar] := y.parent.same[isCar];
          y.parent.same[isCar] := t
        END (* BEGIN *)
      END (* IF *)
    END (* BEGIN *);
    INC(x.parentSize, y.parentSize);

    IF NOT ContextPrivate.inD1P AND NOT Prover.noModTimes THEN 
      SetModTimes(x)
    END (* IF *);

    IF y.misc # NIL AND y.misc.distClasses # EmptyDistClassSet THEN
      VAR xmisc := x.getMisc(); BEGIN
        xmisc.distClasses := xmisc.distClasses + y.misc.distClasses
      END (* BEGIN *)
    END (* IF *);
    IF (x.misc = NIL OR x.misc.unknown = NIL) AND y.misc # NIL THEN
      x.getMisc().unknown := y.misc.unknown
    END (* IF *);
    IF (x.misc = NIL OR x.misc.ordNode = NIL) AND y.misc # NIL THEN
      x.getMisc().ordNode := y.misc.ordNode
    END (* IF *);
    (* x = y.root *)
    UndoStackPush(type := undoTag, data := y)
  END Merge;

(*  Next, we derive the inverse of "Merge", piece by piece.

First, note that the merge was rejected if "x.distClasses" and
"y.distClasses" had a non-empty-intersection, so the union

| x.distClasses := x.distClasses + y.distClasses

is undone by the set difference

| x.distClasses := x.distClasses - y.distClasses

The inverse of

| (* prnts[x] = prnts[y] => prnts[x] = NIL, since
|    parent lists of distinct roots are distinct. *)
| IF x.parent = NIL THEN
|    x.parent := y.parent
| ELSIF y.parent # NIL THEN
|   VAR t := x.parent.same[isCar]; BEGIN
|     x.parent.same[isCar] := y.parent.same[isCar];
|     y.parent.same[isCar] := t
|   END (* BEGIN *)
| END;
| INC(x.parentSize, y.parentSize);
| (* x = y.root *)

is 

| x := y.root;
| DEC(x.parentSize, y.parentSize);
| IF x.parent = y.parent THEN
|   x.parent := NIL;
|   IF y.parent # NIL THEN isCar := (x = y.parent.car.root) END (* IF *)
| ELSE
|   IF y.parent # NIL THEN
|     isCar := IsCar(x);
|     VAR t := x.parent.same[isCar]; BEGIN
|       x.parent.same[isCar] := y.parent.same[isCar];
|       y.parent.same[isCar] := t
|     END (* BEGIN *)
|   END (* IF *)
| END (* IF *)

In addition, this inverse sets "isCar" correctly.

Next, consider

| (* C, B *)
| IF x.parentSize < y.parentSize THEN
|   VAR t := x.id; BEGIN x.id := y.id; y.id := t END (* BEGIN *)
| END (* IF *);
| IF l # NIL THEN
|   VAR lp := l; BEGIN
|     REPEAT
|       IF lp.cgPtr = lp THEN SigInsert(lp) END (* IF *);
|       lp := lp.same[isCar]
|     UNTIL lp = l
|   END (* BEGIN *)
| END (* IF *)

A partial inverse is

| IF x.parentSize < y.parentSize THEN
|   l := x.parent
| ELSE
|   l := y.parent
| END (* IF *);
| IF l # NIL THEN
|   VAR lp := l; BEGIN
|     REPEAT
|       IF lp.cgPtr = lp THEN SigRemove(lp) END (* IF *);
|       lp := lp.same[isCar]
|     UNTIL lp = l
|   END (* BEGIN *)
| END (* IF *);
| IF x.parentSize < y.parentSize THEN
|   VAR t := x.id; BEGIN x.id := y.id; y.id := t END (* BEGIN *)
| END (* IF *)

The forward program calls "SigInsert" for all congruence class
roots "r" in "l".  "SigInsert" inserts "r" in "sigTab" if there is no
entry for that signature; if there is an entry "s", it merges "r"'s
congruence class into the equivalence class rooted "s" (as well as
propagating an equality), so that "r" is no longer an equivalence
class root.  The inverse code considers the same set of nodes as the
forward code, and removes all nodes that the forward code left as
equivalence class roots.  This leaves a modification whose inverse is
still pending: the merging of congruence classes for congruence
classes whose signatures were already present in "sigTab".  Call this
modification "MOD"; its inverse will be appear later in "UnMerge".

Note that the partial inverse also sets "l" correctly.

Next, the inverse of

| (* C: No congruence class root on the circular list "l" is
|       present in "sigTab". *)
| VAR yp := y; BEGIN
|   REPEAT
|     yp.root := x;
|     yp := yp.next
|   UNTIL yp = y
| END;
| VAR t := x.next; BEGIN x.next := y.next; y.next := t END (* BEGIN *);
| INC(x.size, y.size)

is

| DEC(x.size, y.size);
| VAR t := x.next; BEGIN x.next := y.next; y.next := t END (* BEGIN *);
| VAR yp := y; BEGIN
|   REPEAT
|     yp.root := y;
|     yp := yp.next
|   UNTIL yp = y
| END

Next the inverse of

| (* A: y's class is no larger than x's *)
| (* B: l is the shorter of the parent lists of x and y, or
|       y's parent list if the lengths are the same. *)
| IF l # NIL THEN
|   VAR lp := l; BEGIN
|     REPEAT
|       IF lp.cgPtr = lp THEN SigRemove(lp) END (* IF *);
|       lp := lp.same[isCar]
|     UNTIL lp = l
|   END (* BEGIN *)
| END (* IF *)

(and also of the pending modification "MOD") is

| IF l # NIL THEN
|   VAR lp := l; BEGIN
|     REPEAT
|       VAR cg := lp.cgPtr; BEGIN
|         IF lp = cg OR 
|            (cg.cgPtr = cg AND
|             (lp.car.root # cg.car.root OR lp.cdr.root # cg.cdr.root)) THEN
|           SigRemoveUndo(lp)
|         END (* IF *)
|       END (* BEGIN *);
|       lp := lp.same[isCar];
|     UNTIL lp = l
|   END (* BEGIN *)
| END (* IF *)

"SigRemoveUndo(l)" adds "l" to "sigTab", and re-establishes the
invariant that nodes in "sigTab" are congruence class roots.
"SigRemoveUndo" is called under two conditions.  The first, the
condition that "lp" is itself a congruence class root, ensures that
the code shown here is inverted.  The second, the condition that
"lp.cgPtr" is a congruence class root whose signature is different
from that of "lp", ensures that the still-pending modification "MOD"
is inverted.

Next, the inverse of

| IF x.parentSize < y.parentSize THEN
|   l := x.parent;
|   IF x.parent # NIL THEN isCar := IsCar(x) END (* IF *)
| ELSE
|   l := y.parent;
|   IF y.parent # NIL THEN isCar := IsCar(y) END (* IF *)
| END

is

| skip

since "l" and "isCar" are local variables.  Next, the inverse of

| IF x.forbids = NIL THEN                 
|   x.forbids := y.forbids                
| ELSIF y.forbids # NIL THEN              
|   VAR t := x.forbids.next; BEGIN        
|     x.forbids.next := y.forbids.next;   
|     y.forbids.next := t                 
|   END (* BEGIN *)                       
| END (* IF *)

is

| IF x.forbids = y.forbids THEN
|   x.forbids := NIL
| ELSIF y.forbids # NIL THEN
|   VAR t := x.forbids.next; BEGIN
|     x.forbids.next := y.forbids.next;
|     y.forbids.next := t
|   END (* BEGIN *)
| END (* IF *)

Finally, the inverse of

| undoTag := UndoType.Merge;
| IF y.unknown # NIL THEN
|   IF x.unknown = NIL THEN
|     undoTag := UndoType.MergeUnknown
|   ELSE
|     ContextPrivate.Propagate(Simplex.NewEQ(x.unknown, y.unknown, identity := TRUE))
|   END (* IF *)
| END (* IF *)

is 

| IF undoTag = UndoType.MergeX THEN x.unknown := NIL END

This inverse relies on the fact that "unknown" fields of distinct
enodes are distinct, unless they are made the same by the assignment
"ASSGN".

This completes the inversion of merge.  The complete inverse is given
in "UnMerge", below.
*)


PROCEDURE UnMerge(y: T; undoTag: UndoType) =
  (* Merge; UnMerge = Skip (with respect to effects on the context). *)
  VAR isCar: BOOLEAN; x := y.root; l: Parent; BEGIN
    IF y.misc # NIL THEN
      x.misc.distClasses := x.misc.distClasses - y.misc.distClasses
    END (* IF *);
    DEC(x.parentSize, y.parentSize);
    IF x.parent = y.parent THEN
      x.parent := NIL;
      IF y.parent # NIL THEN isCar := (x = y.parent.car.root) END (* IF *)
    ELSE
      IF y.parent # NIL THEN
        isCar := IsCar(x);
        VAR t := x.parent.same[isCar]; BEGIN
          x.parent.same[isCar] := y.parent.same[isCar];
          y.parent.same[isCar] := t
        END (* BEGIN *)
      END (* IF *)
    END (* IF *);
    IF x.parentSize < y.parentSize THEN
      l := x.parent
    ELSE
      l := y.parent
    END (* IF *);
    IF l # NIL THEN
      VAR lp := l; BEGIN
        REPEAT
          IF lp.cgPtr = lp THEN SigRemove(lp) END (* IF *);
          lp := lp.same[isCar]
        UNTIL lp = l
      END (* BEGIN *)
    END (* IF *);
    IF x.parentSize < y.parentSize THEN
      VAR t := x.id; BEGIN x.id := y.id; y.id := t END (* BEGIN *)
    END (* IF *);
    DEC(x.size, y.size);
    <*ASSERT x.size > 0*>
    VAR t := x.next; BEGIN x.next := y.next; y.next := t END (* BEGIN *);
    VAR yp := y; BEGIN
      REPEAT
        yp.root := y;
        yp := yp.next
      UNTIL yp = y
    END;
    IF l # NIL THEN
      VAR lp := l; BEGIN
        REPEAT
          VAR cg := lp.cgPtr; BEGIN
            IF lp = cg OR 
               lp.car.root # cg.car.root OR lp.cdr.root # cg.cdr.root THEN
              SigRemoveUndo(lp)
            END (* IF *)
          END (* BEGIN *);
          lp := lp.same[isCar];
        UNTIL lp = l
      END (* BEGIN *)
    END (* IF *);
    IF x.misc # NIL AND y.misc # NIL AND x.misc.forbids = y.misc.forbids THEN
      x.misc.forbids := NIL
    ELSIF y.misc # NIL AND y.misc.forbids # NIL THEN
      VAR t := x.misc.forbids.next; BEGIN
        x.misc.forbids.next := y.misc.forbids.next;
        y.misc.forbids.next := t
      END (* BEGIN *)
    END (* IF *);
    CASE undoTag OF <*NOWARN*>
    | UndoType.Merge =>
        (* SKIP *)
    | UndoType.MergeSimpUnknown =>
        x.misc.unknown := NIL
    | UndoType.MergeOrdUnknown =>
        x.misc.ordNode := NIL
    | UndoType.MergeSimpOrdUnknown =>
        x.misc.unknown := NIL; x.misc.ordNode := NIL
    END (* CASE *);
    IF x.minHt.root # x THEN
      VAR tmp := x.minHt; BEGIN
        x.minHt := y.minHt; y.minHt := tmp
      END (* BEGIN *)
    END (* IF *)
  END UnMerge;

(* If one of "x" or "y" is "TRUE" and the other is an order symbol,
   propagate the correspond "Orders" literal.  Also, update the
   "HasOrdTerm" property of the new root. *)
PROCEDURE PropOrdersAsserts(x, y: T) =
  VAR z: T := NIL; BEGIN
    IF x.root = eTrue.root AND Prop.HasOrdTerm IN y.props THEN z := y
    ELSIF y.root = eTrue.root AND Prop.HasOrdTerm IN x.props THEN z := x
    END (* IF *);
    IF z # NIL THEN
      VAR zp := z; BEGIN
	REPEAT
	  OrdCheck(); (* $$$ *)
	  TYPECASE zp OF
	  | Parent(p) =>
	      TYPECASE p.car OF
	      | FSym(fs) =>
		  IF Orders.IsRel(fs.sym) THEN
		    VAR argList1: Parent := p.cdr;
			e1 := argList1.car;
			argList2: Parent := argList1.cdr;
			e2 := argList2.car;
			<*FATAL Prover.Error *>
		    BEGIN
		      <*ASSERT argList2.cdr = enil *>
		      ContextPrivate.Propagate(
			  Orders.NewEdgeLit(fs.sym, e1, e2))
		    END (* BEGIN *)
		  END (* IF *)
	      ELSE
	      END (* TYPECASE *);
	  ELSE
	  END (* TYPECASE *);
	  zp := zp.next
	UNTIL zp = z
      END (* BEGIN *)
    END (* IF *);
    VAR hasOrdTerm :=
        (Prop.HasOrdTerm IN x.props) OR (Prop.HasOrdTerm IN y.props);
    BEGIN
      IF hasOrdTerm AND NOT (Prop.HasOrdTerm IN x.props) THEN
        UndoStackPush(UndoType.HasOrdTerm, x)
      END (* IF *);
      x.props := x.props + HasOrdTermProp
    END (* BEGIN *)
  END PropOrdersAsserts;

PROCEDURE OrdCheck() = BEGIN END OrdCheck;

PROCEDURE MergeProp(x, y: T; prop: Prop; undo: UndoType) =
  VAR hasProp := (prop IN x.props) OR (prop IN y.props); BEGIN
    IF hasProp AND NOT (prop IN x.props) THEN
      x.props := x.props + SET OF Prop{prop};
      UndoStackPush(undo, x)
    END (* IF *)
  END MergeProp;

(* Requires that "z" is an equivalence class root without the property 
   "HasPredMapTerm", that is being merged into a new equivalence class
   that will have that property.  Ensures that all parents of parents
   of "z" whose car is "select" have the "HasSelPredMapTerm" property.
*)
PROCEDURE SetSelPredMaps(z: T) =
  BEGIN
    IF z.parent # NIL THEN
      VAR zp2 := z.parent; BEGIN
        REPEAT
          IF zp2.parent # NIL THEN
            VAR zpp := zp2.parent; BEGIN
              REPEAT
                TYPECASE zpp.car OF
                | FSym(fs) =>
                    IF fs.sym = PredSx.selectSym AND
                      NOT Prop.HasSelPredMapTerm IN zpp.props THEN
                      zpp.props := zpp.props + HasSelPredMapTermProp;
                      UndoStackPush(UndoType.HasSelPredMapTerm, zpp);
                      zpp.props := zpp.props + HasSelPredMapTermProp;
                      UndoStackPush(UndoType.HasSelPredMapTerm, zpp)
                    END (* IF *)
                ELSE
                END (* TYPECASE *);
                zpp := zpp.same[FALSE]
              UNTIL zpp = zp2.parent
            END (* BEGIN *)
          END (* IF *);
          zp2 := zp2.same[TRUE]
        UNTIL zp2 = z.parent
      END (* BEGIN *)
    END (* IF *)
  END SetSelPredMaps;

PROCEDURE UnForbid(e: T) =
  (* ForbidMerge; UnForbid = Skip (wrt the context.) *)
  PROCEDURE RemForbid(e: T) =
    BEGIN
      IF e.misc.forbids.next = e.misc.forbids THEN
        e.misc.forbids := NIL
      ELSE
        e.misc.forbids.next := e.misc.forbids.next.next
      END (* IF *)
    END RemForbid;
  BEGIN
    VAR f := e.misc.forbids.next.t; BEGIN
      RemForbid(f); RemForbid(e)
    END (* BEGIN *);
  END UnForbid;

VAR propMergeDebug := Env.Get("PROVER_PROP_MERGE_DEBUG") # NIL;
    
PROCEDURE SigInsert(t: Parent) =
  (* "t" is required to be a congruence class root.  If there is no
     entry in "sigTab" for the current signature of "t", insert it
     under that signature; otherwise, propagate an equality between
     "t" and the existing node "s" with its signature, merging "t"'s
     congruence class into the congruence class rooted at "s".  *)
  VAR res: REFANY; sig := Signature.T{t.car.root.id, t.cdr.root.id}; BEGIN
    IF sigTab.get(sig, res) THEN
      VAR resE: T := res; BEGIN
        INC(stats.nPropEq);
        IF propMergeDebug THEN
          Sx.Print(Stdio.stdout,
                   RefList.List3(PredSx.eqSym, DbgToSx(t), DbgToSx(resE)));
          Wr.PutText(Stdio.stdout, "\n"); Wr.Flush(Stdio.stdout)
        END (* IF *);
        mergeStack.addhi(t); mergeStack.addhi(resE);
        t.cgPtr := res
      END (* BEGIN *)
    ELSE
      EVAL sigTab.put(sig, t)
    END (* IF *)
  END SigInsert;

PROCEDURE SigRemove(t: Parent) =
  (* Removes any entry in "sigTab" for the signature of "t". *)
  VAR res: REFANY; BEGIN
    EVAL sigTab.delete(Signature.T{t.car.root.id, t.cdr.root.id}, res)
  END SigRemove;
  
PROCEDURE SigRemoveUndo(t: Parent) =
  (* Inserts an entry in "sigTab" for "t", and re-establishes the invariant
     that nodes in the "sigTab" are congruence class roots. *)
  VAR sig := Signature.T{t.car.root.id, t.cdr.root.id}; BEGIN
    EVAL sigTab.put(sig, t);
    t.cgPtr := t
  END SigRemoveUndo;

TYPE SxArr = REF ARRAY OF REFANY;

VAR sxSz: SxArr;
    sz: REF ARRAY OF CARDINAL;

(* Use outside of "ComputeSxSizes" requires that "C" has not changed since
   the last call to "ComputeSxSizes". *)
PROCEDURE Size(t: T): CARDINAL =
  VAR res: INTEGER; BEGIN
    TYPECASE t OF <*NOWARN*>
    | Leaf(lf) =>
        IF Prop.Const IN lf.props THEN RETURN 0 END (* IF *);
        TYPECASE lf.sym OF <*NOWARN*>
        | NULL => res := 1;
        | REF INTEGER(ri) =>
            VAR n := ABS(ri^); BEGIN
              res := 1; 
              WHILE n # 0 DO n := n DIV 10; INC(res) END (* WHILE *);
            END (* BEGIN *);
        | REF LONGREAL(rl) =>
            res := Text.Length(Fmt.LongReal(rl^));
        | Atom.T(s) =>
            res := Text.Length(Atom.ToText(s))
        END (* TYPECASE *)
    | Parent(p) =>
        res := 1 + sz[p.car.root.id] + sz[p.cdr.root.id]
    END (* TYPECASE *);
    RETURN res;
  END Size;

TYPE
  PQueueBucket = REF RECORD next: PQueueBucket; term: T END;
  PQueue = REF ARRAY OF PQueueBucket;

VAR sxSizeTime := Env.Get("PROVER_SX_SIZE_TIME") # NIL;
  
PROCEDURE ComputeSxSizes() =
  VAR done := NEW(REF ARRAY OF BOOLEAN, idCntr);
      startTime: Time.T;
  BEGIN
    TRY
    IF sxSizeTime THEN startTime := Time.Now() END (* IF *);
    (* P1: For all equivalence class roots "r", "done[r.id]" implies
       that "sxSz[r.id]" is a minimum-sized S-expression that
       represents "r", and "sz[r.id]" is the size of that
       S-expression.
         
       Let "Ready(v)" be the property that "v" is an atom, or both
       "done[v.car.root.id]" and "done[v.cdr.root.id]" are "TRUE".

       P2: For all "v" such that "Ready(v)", "v" is in "pq[Size(v)]".
    *)
    sxSz := NEW(REF ARRAY OF REFANY, idCntr);
    sz := NEW(REF ARRAY OF CARDINAL, idCntr);
    FOR i := 0 TO LAST(done^) DO done[i] := FALSE END (* FOR *);
    (* P1 *)
    (* P2 *)
    VAR pq := NEW(PQueue, 100); n := 1; BEGIN
      FOR i := 0 TO LAST(pq^) DO pq[i] := NIL END (* FOR *);
      VAR iter := symTab.iterate(); key: Atom.T; val: REFANY; BEGIN
        WHILE iter.next(key, val) DO
          pq[1] := NEW(PQueueBucket, term := val, next := pq[1])
        END (* WHILE *);
        pq[1] := NEW(PQueueBucket, term := enil, next := pq[1])
      END (* BEGIN *);
      (* P2 *)
      LOOP
        (* P3: For all "i < n", all terms "t" appearing in bucket
           "pq[i]" satisfy "done[t.root.id]". *)
        WHILE n # NUMBER(pq^) AND pq[n] = NIL DO INC(n) END;
        IF n = NUMBER(pq^) THEN 
          (* Checking code. *)
          PROCEDURE Done(e: T): BOOLEAN =
            BEGIN
              IF sxSz[e.root.id] = NIL AND 
                NOT (ISTYPE(e, Leaf) AND NARROW(e, Leaf).sym = NIL) THEN
                RETURN FALSE
              ELSE
                RETURN TRUE
              END (* IF *)
            END Done;
          BEGIN
            TRY
              EVAL MapEgraph(Done)
            EXCEPT 
              Prover.Timeout => RAISE Prover.Error(
                "Enode.ComputeSxSizes: unexpected Prover.Timeout")
            END
          END (* BEGIN *);
          RETURN
        END (* IF *);
        (* pq[n] # NIL *)
        VAR t := pq[n].term; BEGIN
          pq[n] := pq[n].next;
          IF NOT done[t.root.id] THEN
            done[t.root.id] := TRUE;
            TYPECASE t OF <*NOWARN*>
            | Leaf(lf) =>
                sz[lf.root.id] := Size(lf);
                sxSz[lf.root.id] := lf.sym
            | Parent(p) =>
                sz[p.root.id] := Size(p);
                sxSz[p.root.id] :=
                    RefList.Cons(sxSz[p.car.root.id], sxSz[p.cdr.root.id])
            END (* TYPECASE *);
            t := t.root;
            IF t.parent # NIL THEN
              VAR p := t.parent; isCar := IsCar(t); BEGIN
                REPEAT
                  IF done[p.car.root.id] AND done[p.cdr.root.id] THEN
                    VAR pSz := Size(p); BEGIN
                      <*ASSERT pSz >= n *>
                      IF pSz > LAST(pq^) THEN
                        VAR pqNew := NEW(PQueue, MIN(pSz + 1, 2 * NUMBER(pq^)));
                        BEGIN
                          SUBARRAY(pqNew^, 0, NUMBER(pq^)) := pq^;
                          FOR i := NUMBER(pq^) TO LAST(pqNew^) DO
                            pqNew[i] := NIL
                          END (* FOR *);
                          pq := pqNew
                        END (* BEGIN *)
                      END (* IF *);
                      pq[pSz] := NEW(PQueueBucket, term := p, next := pq[pSz])
                    END (* BEGIN *)
                  END (* IF *);
                  p := p.same[isCar];
                UNTIL p = t.parent
              END (* BEGIN *)
            END (* IF *)
          END (* IF *)
        END (* BEGIN *)
      END (* LOOP *)
    END (* BEGIN *);
    FINALLY
      IF sxSizeTime THEN
        Wr.PutText(Stdio.stdout, 
                   "ComputeSxSizes on " &
                   Fmt.Int(stats.nCons + stats.nLeaf) & " enodes took " &
                   Fmt.LongReal((Time.Now()-startTime)*1000.0D0, prec := 4) &
                   " ms.\n")
      END (* IF *)
    END (* TRY *)
  END ComputeSxSizes;

PROCEDURE ToSx(t: T): REFANY =
  BEGIN
    RETURN sxSz[t.root.id]
  END ToSx;

TYPE TArr = REF ARRAY OF T;
     
PROCEDURE Top(): RefList.T =
  VAR distTerms := NEW(TArr, idCntr);
      distSxs := NEW(SxArr, idCntr);
      equals: REFANY := NIL;
      disEquals: REFANY := NIL;
  PROCEDURE CongClassRep(t: T): REFANY =
    (* Returns the minimal-sized S-expression representing "t"'s congruence
       class. *)
    BEGIN
      TYPECASE t OF <*NOWARN*>
      | Leaf(l) =>
          RETURN l.sym
      | Parent(p) =>
          IF sxSz[p.car.root.id] = NIL THEN
            TYPECASE p.car.root OF
            | Leaf(lf) =>
                VAR b: BOOLEAN; ra: REFANY; BEGIN
                  b := symTab.get(lf.sym, ra);
                  <*ASSERT b *>
                END (* BEGIN *)
            ELSE
                <*ASSERT FALSE*>
            END (* TYPECASE *)
          END (* IF *);
          <*ASSERT sxSz[p.car.root.id] # NIL *>
          RETURN RefList.Cons(sxSz[p.car.root.id], sxSz[p.cdr.root.id])
      END (* TYPECASE *)
    END CongClassRep;
  BEGIN
    PROCEDURE PickDistTerm(t: T) =
      (* Adds "t"'s congruence class to "D", maintaining P0. *)
      BEGIN
        IF distTerms[t.root.id] = NIL OR
          Size(t) < Size(distTerms[t.root.id]) THEN
          distTerms[t.root.id] := t;
          distSxs[t.root.id] := CongClassRep(t)
        END (* IF *)
      END PickDistTerm;

    PROCEDURE Dist(t, u: T): PredSx.T =
      VAR s: T := NIL; BEGIN
        IF t.root = eTrue.root THEN s := u
        ELSIF u.root = eTrue.root THEN s := t
        END (* IF *);
        IF s # NIL THEN
          VAR trm := sxSz[s.root.id]; BEGIN
            TYPECASE trm OF
            | RefList.T(rl) =>
                IF PredDefs.IsPredSym(rl.head) THEN
                  RETURN PredSx.Not(trm)
                END (* IF *)
            ELSE
            END (* TYPECASE *)
          END (* BEGIN *)
        END (* IF *);
        RETURN RefList.List3(PredSx.diffSym,
                             sxSz[t.root.id],
                             sxSz[u.root.id])
      END Dist;

    PROCEDURE Equality(t, u: PredSx.T): PredSx.T =
      VAR s: PredSx.T := NIL; BEGIN
        IF t = trueSym THEN
          s := u; VAR tmp := t; BEGIN t := u; u := tmp END (* BEGIN *)
        ELSIF u = trueSym THEN s := t
        END (* IF *);
        IF s # NIL THEN
          TYPECASE s OF
          | NULL => <*ASSERT FALSE*>
          | RefList.T(srl) =>
              IF PredDefs.IsPredSym(srl.head) THEN RETURN s END (* IF *)
          ELSE
          END (* TYPECASE *)
        END (* IF *);
        RETURN RefList.List3(PredSx.eqSym, t, u)
      END Equality;

    PROCEDURE AddRelations(t: T) =
      (* Assumes PO for D1 = all congruence classes.  Adds "t"'s congruence
         class to "D2", maintaining P1 and P2. *) 
      VAR distT := distTerms[t.root.id];
          distSx := distSxs[t.root.id];
      BEGIN
        IF distT = t THEN
          IF t.root.misc # NIL THEN
            VAR tl := t.root.misc.forbids; BEGIN
              IF tl # NIL THEN
                REPEAT
		  VAR tlprev := t.root.misc.forbids; BEGIN
		    WHILE tlprev # tl AND tlprev.t.root # tl.t.root DO
		      tlprev := tlprev.next
		    END;
		    IF tlprev = tl AND tl.t.root.id < t.root.id THEN
                      disEquals := RefList.Cons(Dist(t, tl.t), disEquals)
		    END (* IF *)
		  END (* BEGIN *);
		  tl := tl.next
                UNTIL tl = t.root.misc.forbids
              END (* IF *)
            END (* BEGIN *)
            (* P2 *)
          END (* IF *)
        ELSE
          VAR rep := CongClassRep(t); BEGIN
            IF NOT (distT = eTrue AND ISTYPE(rep, RefList.T) AND
                    NARROW(rep, RefList.T).head = PredSx.diffSym) THEN
              equals := RefList.Cons(Equality(rep, distSx), equals)
            END (* IF *)
          END (* BEGIN *)
        END (* IF *)
      END AddRelations;
    BEGIN
      FOR i := 0 TO LAST(distTerms^) DO distTerms[i] := NIL END (* FOR *);
      (* The following two iterations over "symTab" and "sigTab"
	 establish the following postcondition:

         (P0) For all enodes "t", "distTerms[t.root.id]" is the the
         congruence class representative in "t"'s equivalence class
         whose S-expression has the smallest print length of any of
         the representatives of the congruence classes, and
         "distSxs[t.root.id]" is the S-expression of this term.
      *)
      VAR iter := symTab.iterate(); key: Atom.T; val: REFANY; BEGIN
        WHILE iter.next(key, val) DO
          PickDistTerm(val)
        END (* WHILE *);
        PickDistTerm(enil)
      END (* BEGIN *);
      VAR iter := sigTab.iterate(); key: Signature.T; val: REFANY; BEGIN
        WHILE iter.next(key, val) DO
          PickDistTerm(val)
        END (* WHILE *)
      END (* BEGIN *);
      (* The next pair of iterations over "symTab" and "sigTab"
	 establish this postcondition:

         (P1) "equals" expresses all equalities between congruence
         classes "e = f" where "e" is a minimal congruence class
         computed above, and "f" is another congruence class in the
         same equivalence class.

         (P2) "disEquals" expresses all disequalities "e # f" such that
         "e" and "f" are minimal congruence class representatives, and
         "f.root.id" is smaller than "e.root.id".
      *)
      (* D1 = all congruence classes, P0 *)
      (* D2 = empty, P1, P2 *)
      VAR iter := symTab.iterate(); key: Atom.T; val: REFANY; BEGIN
        WHILE iter.next(key, val) DO
          AddRelations(val)
        END (* WHILE *);
        AddRelations(enil)
      END (* BEGIN *);
      VAR iter := sigTab.iterate(); key: Signature.T; val: REFANY; BEGIN
        WHILE iter.next(key, val) DO
          AddRelations(val)
        END (* WHILE *)
      END (* BEGIN *);
      (* D2 = all congruence classes, P1, P2 *)

      (* Also form literals for all the asserted distinction classes. *)
      VAR distClasses: RefList.T := NIL; BEGIN
        FOR i := 0 TO nDistClasses-1 DO
          VAR trms := distClassTerms[i]; distTerms: RefList.T := NIL; BEGIN
            WHILE trms # NIL DO
              distTerms := RefList.Cons(CongClassRep(trms.head), distTerms);
              trms := trms.tail
            END (* WHILE *);
            distClasses := RefList.Cons(RefList.Cons(
                                            PredSx.distClassSym,
                                            RefList.ReverseD(distTerms)),
                                        distClasses)
          END (* BEGIN *)
        END (* FOR *);
        RETURN RefList.AppendD(distClasses, RefList.AppendD(equals, disEquals))
      END (* BEGIN *)
    END (* BEGIN *)
  END Top;

PROCEDURE Init() =
  BEGIN
    symTab := NEW(AtomRefTbl.Default).init(100);
    idCntr := 0;
    nDistClasses := 0;
    sigTab := NEW(SigTab.Default).init(1000);
    identTab := NEW(SigTab.Default).init(1000);
    distClassTab := NEW(DistClassTab).init(10);
    distClassTerms := TermListArr{NIL, ..};
    matchFPTable := NEW(FPRefTbl.Default).init(100);
    unitPcPairSet := PairSet.Empty;
    nuPcPairSet := PairSet.Empty;
    unitPpPairSet := PairSet.Empty;
    nuPpPairSet := PairSet.Empty;
    unitPSet := ParentSet.Empty();
    nuPSet := ParentSet.Empty();
    
    unitPcUndoStack := NEW(RefSeq.T).init();
    nuPcUndoStack := NEW(RefSeq.T).init();
    unitPpUndoStack := NEW(RefSeq.T).init();
    nuPpUndoStack := NEW(RefSeq.T).init();
    unitPUndoStack := NEW(ParentSetSeq.T).init();
    nuPUndoStack := NEW(ParentSetSeq.T).init();

    pSetStack := NEW(IdSetSeq.T).init();
    cSetStack := NEW(IdSetSeq.T).init();

    InitUndoStack();
    enil := NEW(Leaf).init(NIL);
    trueSym := Atom.FromText("@true");
    eNeq := FromSym(PredSx.diffSym, fsym := TRUE);
    eTrue := FromSym(trueSym);
    plusFS := FromSym(PredSx.plusSym, fsym := TRUE);
    minusFS := FromSym(PredSx.minusSym, fsym := TRUE);
    timesFS := FromSym(PredSx.timesSym, fsym := TRUE);
    IsConst(eTrue);
    SelectFSym := FromSym(PredSx.selectSym, fsym := TRUE);
    StoreFSym := FromSym(PredSx.storeSym, fsym := TRUE);
    StoreIdSet := PairSet.Singleton(StoreFSym.shortId());

    stats := StatRec{};

    VAR matchLogFN := Env.Get("PROVER_MATCH_LOG"); BEGIN
      IF matchLogFN # NIL THEN
        matchLog := FileWr.Open(matchLogFN); <*NOWARN*>
        Wr.PutText(matchLog, matchLegend)
      END (* IF *)
    END (* BEGIN *);
    Process.RegisterExitor(CloseLogs)
  END Init;

PROCEDURE CloseLogs() =
  BEGIN
    IF MatchingRule.idWr # NIL THEN Wr.Close(MatchingRule.idWr) END (* IF *);
    IF matchLog # NIL THEN Wr.Close(matchLog) END (* IF *)
  END CloseLogs;


PROCEDURE InitUndoStack() =
  BEGIN
    undoStack := NEW(UndoStack, MinUndoStackSize);
    undoStats := UndoStatArr{0, ..};
    quarterSize := NUMBER(undoStack^) DIV 4;
    undoSP := 0;
    undoFP := NEW(FPSeq.T).init(100)
  END InitUndoStack;

<*UNUSED*>
PROCEDURE DbgPrint(txt: TEXT) =
  BEGIN
    Wr.PutText(Stdio.stdout, txt);
    Wr.Flush(Stdio.stdout)
  END DbgPrint;

(* "Match" procedures. *)

(* If pat is a pattern and theta is a substitution then
   theta(pat) is the pattern obtained by replacing in pat all 
   occurrences of any variable v in the domain of theta
   by theta(v).
    
   We say "s1 <= s2" if "s2(x)" is equivalent (in the egraph) to "s1(x)" 
   whenever "s1(x)" is defined.   If S and T are sets of substitutions, we 
   say S <= T if for all t in T, there exists an s in S such that s <= t. In 
   particular, the empty set <= the empty set (only), and the 
   set containing the empty substitution <= any set of substitutions. 

   If "S <= T", we say that "S" generalizes "T". 

   The nodes in an egraph are of three types: car-nodes, which represent
   terms, cdr-nodes, which represent lists of terms, and symbol nodes,
   which represent function symbols.
   
   If "nd" is a car-type node in the egraph, we write "nd[i]" to 
   denote the node "nd.cdr.cdr...cdr.car" where "i" is the number
   of "cdr"s. 

   Here is a recursive definition of the set of terms represented
   by a car-type node in an egraph: If "nd" is a car-type node of 
   the e-graph, then "nd" represents "(f t1 ... tn)" if "nd.car"
   represents "f" and "nd.cdr" represents "(t1 ... tn)".

   If "nd" is a cdr-type node, it represents "(t1 ... tn)" if
   "nd.car" is equivalent to some node that represents "t1" and "nd.cdr" 
   represents "(t2 ... tn)", or if "n = 0" and "nd" is "enil".
*)

VAR
  matchFPTable: FPRefTbl.T;
  unitPcPairSet, nuPcPairSet: PairSet.T;
  unitPpPairSet, nuPpPairSet: PairSet.T;
  unitPSet, nuPSet: ParentSet.T;

  unitPcUndoStack, nuPcUndoStack,
  unitPpUndoStack, nuPpUndoStack: RefSeq.T;
  unitPUndoStack, nuPUndoStack: ParentSetSeq.T;

  pSetStack, cSetStack: IdSetSeq.T;

TYPE
  MatchAction = PROCEDURE(VAR s: MatchingRule.Substitution;
                          lhs: T; data: REFANY): BOOLEAN
                  RAISES {Prover.Timeout};

PROCEDURE Match(po: MatchingRule.PatObj; allCommits: MatchingRule.CommittedArr;
                a: MatchAction; data: REFANY;
                modlim := -1): BOOLEAN RAISES {Prover.Timeout} =
  (* "pats" must be a non-empty list of "Pattern"s, containing "npvs"
     pattern variables.  "Match(pats, a)" calls "a(theta, lhs, data)"
     for each "theta" in some set that generalizes the set of all
     "theta" such that "theta(pat)", for each "Pattern" "pat" in
     "pats", is represented by some car-type node "lhs" in the egraph.
     "Match" stops and returns "FALSE" if a call to "a" returns
     "FALSE"; otherwise, "Match" returns "TRUE".  "Match" requires
     that each pattern in "pats" be non-atomic.  *)
  VAR theta := ARRAY [0..MatchingRule.MaxPatVars-1] OF T{NIL, ..};
      pats := po.p;
      patLen := RefList.Length(pats);
  PROCEDURE MatchLogAction(VAR s: MatchingRule.Substitution; lhs: T;
                           data: REFANY): BOOLEAN
              RAISES {Prover.Timeout} =
    BEGIN
      (* MatchLog("!"); *)RETURN a(s, lhs, data)
    END MatchLogAction;
  BEGIN
    FOR i := 0 TO patLen-1 DO
      IF (NOT ISTYPE(po.pieces[i].piece, T) AND po.pieces[i].enabled)
        OR modlim = -1 THEN
	IF matchLog # NIL THEN
	  IF NOT MatchWork(pats.head, pats.tail, allCommits[i],
                           MatchLogAction,
                           data, theta, modlim) THEN
	    RETURN FALSE
	  END (* IF *);
          IF modlim = -1 THEN RETURN TRUE END (* IF *)
	ELSE
	  IF NOT MatchWork(pats.head, pats.tail, allCommits[i], a,
                           data, theta, modlim) THEN
            RETURN FALSE
	  END (* IF *);
          IF modlim = -1 THEN RETURN TRUE END (* IF *)
	END (* IF *)
      END (* IF *);
      IF i < patLen-1 THEN
	pats := RefList.Append(pats.tail, RefList.List1(pats.head))
      END (* IF *)
    END (* FOR *);
    RETURN TRUE
  END Match;

VAR enablePieceLog: Wr.T;

PROCEDURE ComputeEnabledPieces(rule: MatchingRule.T; po: MatchingRule.PatObj;
                               READONLY gpc, gpp: PairSet.T;
                               gp: ParentSet.T) =
  VAR enabledPVs := MatchingRule.PatVarNumSet{};
      dischargedPVs := MatchingRule.PatVarNumSet{};
  BEGIN
    IF Prover.noPatElems THEN
      FOR i := 0 TO LAST(po.pieces^) DO
        po.pieces[i].enabled := TRUE
      END (* FOR *);
      RETURN
    END (* IF *);
    IF NUMBER(po.pieces^) = 1 THEN
      po.pieces[0].enabled := TRUE; RETURN
    END (* IF *);
    FOR i := 0 TO LAST(po.pvs^) DO
      po.pvs[i].enabled := PairSet.Overlap(po.pvs[i].pps, gpp);
      IF po.pvs[i].enabled THEN 
        enabledPVs := enabledPVs + MatchingRule.PatVarNumSet{i}
      END (* IF *)
    END (* FOR *);
    FOR i := 0 TO LAST(po.pieces^) DO
      TYPECASE po.pieces[i].piece OF <*NOWARN*>
      | RefList.T(rl) =>
	IF NUMBER(po.pieces[i].pcs^) # 0 THEN
	  po.pieces[i].enabled := PairSet.Overlap(po.pieces[i].pcs, gpc)
	ELSE
	  po.pieces[i].enabled :=
	      ParentSet.Member(gp, NARROW(rl.head, FSym).shortId()) AND
	      (po.pieces[i].pvs * po.multiplePVs) <= enabledPVs
	END (* IF *);
	IF po.pieces[i].enabled THEN
	  dischargedPVs := dischargedPVs + po.pieces[i].pvs
	END (* IF *)
      | T => (*SKIP*)
      END (* TYPECASE *)
    END (* FOR *);
    IF enablePieceLog # NIL THEN
      Wr.PutText(enablePieceLog, Fmt.Int(rule.id) & "|\n");
      Wr.PutText(enablePieceLog, Fmt.Int(rule.id) & "|");
      FOR i := 0 TO LAST(po.pieces^) DO
        IF po.pieces[i].enabled THEN
          Wr.PutText(enablePieceLog, "*")
        ELSE
          Wr.PutText(enablePieceLog, "-")
        END (* IF *)
      END (* FOR *);
      Wr.PutText(enablePieceLog, "\n");
      FOR k := 0 TO LAST(po.pvs^) DO
        IF k IN enabledPVs THEN
          Wr.PutText(enablePieceLog, Fmt.Int(rule.id) & "|");
          FOR i := 0 TO LAST(po.pieces^) DO
            IF k IN po.pieces[i].pvs THEN
              Wr.PutText(enablePieceLog, "0")
            ELSE
              Wr.PutText(enablePieceLog, ".")
            END (* IF *)
          END (* FOR *);
          Wr.PutText(enablePieceLog, "\n")
        END (* IF *)
      END (* FOR *)
    END (* IF *);
    enabledPVs := enabledPVs - dischargedPVs;
    FOR i := 0 TO LAST(po.pieces^) DO
      IF po.pieces[i].pvs * enabledPVs # MatchingRule.EmptyPatVarNumSet THEN
        po.pieces[i].enabled := TRUE;
        enabledPVs := enabledPVs - po.pieces[i].pvs
      END (* IF *);
    END (* FOR *);
    IF enablePieceLog # NIL THEN
      Wr.PutText(enablePieceLog, Fmt.Int(rule.id) & "|");
      FOR i := 0 TO LAST(po.pieces^) DO
        IF po.pieces[i].enabled THEN
          Wr.PutText(enablePieceLog, "*")
        ELSE
          Wr.PutText(enablePieceLog, "-")
        END (* IF *)
      END (* FOR *);
      Wr.PutText(enablePieceLog, "\n")
    END (* IF *);
    VAR n := 0; BEGIN
      FOR i := 0 TO LAST(po.pieces^) DO
        IF po.pieces[i].enabled THEN INC(n) END (* IF *)
      END (* FOR *);
      IF n = 0 THEN
        INC(stats.m.avoidedPieces, NUMBER(po.pieces^))
      ELSE
        INC(stats.m.unavoidedPieces, NUMBER(po.pieces^));
        INC(stats.m.unavoidedEnabled, n)
      END (* IF *)
    END (* BEGIN *)
  END ComputeEnabledPieces;

PROCEDURE AllEnodePiecesActive(po: MatchingRule.PatObj): BOOLEAN =
  VAR res := TRUE; i := 0; BEGIN
    WHILE res AND i < NUMBER(po.pieces^) DO
      TYPECASE po.pieces[i].piece OF
      | T(e) =>
          res := Prop.Active IN e.props
      ELSE (*SKIP*)
      END (* TYPECASE *);
      INC(i)
    END (* WHILE *);
    RETURN res
  END AllEnodePiecesActive;

PROCEDURE EtpMatchStart(<*UNUSED*> rule, pRule: INTEGER;
                        <*UNUSED*> triv: BOOLEAN;
                        <*UNUSED*> ruleWidth: INTEGER) =
  BEGIN END EtpMatchStart;
PROCEDURE EtpMatchEnd() =
  BEGIN END EtpMatchEnd;
PROCEDURE EtpMatchFound(<*UNUSED*> rule, pRule: INTEGER) =
  BEGIN END EtpMatchFound;
PROCEDURE EtpRedundantMatch(<*UNUSED*> id, parId: INTEGER) =
  BEGIN END EtpRedundantMatch;

CONST
  matchLegend =
    "!    Successful match\n" &
    "[]   begin/end tail of multipattern\n" &
    "<>   begin/end tail of argument list\n" &
    "()   begin/end of equivalence class traversal\n" &
    "$    bound a pattern variable\n" &
    ".    enode not congruence class root with correct function symbol\n" &
    ",    fsym parent not congruence class root in right equiv class\n" &

    "V    matched a bound pattern variable\n" &
    "v    failed to match a bound pattern variable\n" &

    "A    matched an atom\n" &
    "a    failed to match an atom\n" &

    "E    matched an enode\n" &
    "e    failed to match an enode\n" &

    "I    matched an integer\n" &
    "i    failed to match an integer\n" &

    "R    matched a real\n" &
    "r    failed to match a real\n" &

    "T    matched committed term\n" &
    "t    failed to match committed term\n\n";

<*UNUSED*>
PROCEDURE MatchLog(t: TEXT) =
  BEGIN 
    IF matchLog # NIL THEN Wr.PutText(matchLog, t) END (* IF *);
    <*ASSERT FALSE*>
  END MatchLog;

PROCEDURE MatchWork(pat: REFANY; remPats: RefList.T;
                    READONLY commits: MatchingRule.BoolArr;
                    a: MatchAction; data: REFANY;
                    VAR theta: MatchingRule.Substitution;
                    modlim := -1): BOOLEAN RAISES {Prover.Timeout} =
  VAR initmatches := stats.m.mwmatchiters;
      inititers := stats.m.mwiters;
      patRL: RefList.T;
  BEGIN
    VAR i := LAST(commits)-RefList.Length(remPats); BEGIN
      IF commits[i] THEN
	IF PatRepresented(pat, theta) # NIL THEN
          (* MatchLog("T"); *)
	  IF remPats = NIL THEN
	    RETURN a(theta, NIL, data)
	  ELSE
	    RETURN MatchWork(remPats.head, remPats.tail, commits,
                             a, data, theta)
	  END (* IF *)
	ELSE
          (* MatchLog("t"); *) RETURN TRUE
	END (* IF *)
      END (* IF *)
    END (* BEGIN *);
    INC(stats.m.mwcalls);
    TYPECASE pat OF <*NOWARN*>
    | T(e) =>
        IF Prop.Active IN e.props THEN
          RETURN MatchWork(remPats.head, remPats.tail, commits,
                           a, data, theta)
        ELSE
          RETURN TRUE
        END (* IF *)
    | MatchingRule.PatVar(pv) =>
        PROCEDURE MatchAny(e: T): BOOLEAN RAISES {Prover.Timeout} =
          PROCEDURE P(VAR theta: MatchingRule.Substitution;
                      lhs: T; data: REFANY): BOOLEAN RAISES {Prover.Timeout}=
            (* Call "a(theta, lhs, data)" for each "theta" in some set of
               substitutions that generalizes the set of all "theta" that
               extend "s" and are such that "theta(pat.tail)" is
               represented by "ep.cdr". *)
            BEGIN
              RETURN MatchArgs(NIL, remPats, commits, e, theta, a, lhs, data)
            END P;
          BEGIN
            RETURN MatchEq(pv, commits, e, theta, P, e, data)
          END MatchAny;
        BEGIN
          RETURN MapEgraph(MatchAny);
        END (* BEGIN *)
    | RefList.T(rl) =>
        IF Thread.TestAlert() THEN RAISE Prover.Timeout END;
        patRL := rl;
        VAR func: FSym := rl.head;
            term := func.terms;
            lastModTime := LAST(INTEGER);
        BEGIN
          IF term # NIL THEN
            REPEAT
              <*ASSERT term.car = func *>
              INC(stats.m.mwiters); INC(stats.m.enodesExamined);
              IF Prop.Active IN term.root.props AND
                term.modTime > modlim AND
                term.cgPtr = term THEN
                INC(stats.m.mwmatchiters);
                (*
                IF RefList.Length(remPats)+1 = NUMBER(commits) THEN
                  MatchLog(Atom.ToText(func.sym) & ":")
                END (* IF *);
                *)
                VAR res := EtpMatchArgs(patRL.tail, remPats, commits,
                                        term.cdr, theta, a, term, data);
                BEGIN
                  (*
                  IF RefList.Length(remPats)+1 = NUMBER(commits) THEN
                    MatchLog("\n")
                  END (* IF *);
                  *)
                  IF NOT res THEN
                    RETURN FALSE
                  END (* IF *)
                END (* BEGIN *)
              END (* IF *);
              <*ASSERT term.modTime <= lastModTime *>
              lastModTime := term.modTime;
              term := term.link
            UNTIL term = func.terms OR term.modTime <= modlim
          ELSE
            <*ASSERT func.parent = NIL *>
          END (* IF *);
          VAR i: INTEGER; BEGIN
            IF NOT stats.m.symMWIterTable.get(func.sym, i) THEN
              i := 0
            END (* IF *);
            INC(i, stats.m.mwiters - inititers);
            EVAL stats.m.symMWIterTable.put(func.sym, i)
          END (* IF *)
        END (* BEGIN *);
        IF stats.m.mwmatchiters = initmatches THEN
          INC(stats.m.mwnullcalls);
          INC(stats.m.mwnulliters, stats.m.mwiters - inititers)
        END (* IF *);
    END (* TYPECASE *);
    RETURN TRUE
  END MatchWork;

PROCEDURE PatRepresented(pat: REFANY; READONLY theta: MatchingRule.Substitution): T =
  VAR ra: REFANY; BEGIN
    TYPECASE pat OF <*NOWARN*>
    | NULL => RETURN enil
    | T(e) => RETURN e
    | Atom.T(at) => 
        IF NOT symTab.get(at, ra) THEN ra := NIL END (* IF *);
        RETURN ra
    | REF INTEGER(ri) =>
        IF NOT symTab.get(Atom.FromText(Fmt.Int(ri^)), ra) THEN
          ra := NIL
        END (* IF *);
        RETURN ra
    | REF LONGREAL(rl) =>
        IF NOT symTab.get(Atom.FromText(Fmt.LongReal(rl^)), ra) THEN
          ra := NIL
        END (* IF *);
        RETURN ra
    | MatchingRule.PatVar(pv) =>
        <*ASSERT theta[pv^] # NIL*>
        RETURN theta[pv^]
    | RefList.T(rl) =>
        VAR car := PatRepresented(rl.head, theta);
        BEGIN
          IF car # NIL THEN
            VAR cdr := PatRepresented(rl.tail, theta); BEGIN
              IF cdr # NIL THEN
                IF sigTab.get(Signature.T{car.root.id, cdr.root.id}, ra) THEN
                  RETURN ra
                END (* IF *)
              END (* IF *)
            END (* BEGIN *)
          END (* IF *);
          (* Otherwise *)
          RETURN NIL
        END (* BEGIN *)
    END (* TYPECASE *)
  END PatRepresented;

(* If the nocde represents the cons of "car" and "cdr" exists, returns
   it, else returns "NIL". *)
<*UNUSED*>
PROCEDURE ConsExists(car, cdr: T): T =
  VAR ra: REFANY; BEGIN
    IF sigTab.get(Signature.T{car.root.id, cdr.root.id}, ra) THEN
      RETURN ra
    ELSE
      RETURN NIL
    END (* IF *)
  END ConsExists;


<*UNUSED*>
PROCEDURE EtpMatchWorkIter(<*UNUSED*> n: INTEGER) = BEGIN END EtpMatchWorkIter;

VAR matchLog: Wr.T := NIL;

PROCEDURE EtpMatchArgs(pat, remPats: RefList.T;
                       READONLY commits: MatchingRule.BoolArr;
                       e: T;
                       VAR theta: MatchingRule.Substitution;
                       a: MatchAction; lhs: T; data: REFANY): BOOLEAN 
          RAISES {Prover.Timeout} =
  BEGIN RETURN MatchArgs(pat, remPats, commits, e, theta, a, lhs, data)
  END EtpMatchArgs;

PROCEDURE MatchArgs(pat, remPats: RefList.T; 
                    READONLY commits: MatchingRule.BoolArr;
                    e: T;
                    VAR theta: MatchingRule.Substitution;
                    a: MatchAction; lhs: T; data: REFANY): BOOLEAN
          RAISES {Prover.Timeout} =
  (* "MatchArgs(pat, remPats, e, s, a, lhs, data)" calls
     "a(theta, lhs', data)" for each "theta" in some set of
     substitutions that generalizes the set of all "theta" that extend
     "s" and are such that "theta(pat)" is represented by "e", and
     there exist other enodes "e$_n$" such that "theta(rp)" is also
     represented, for the other elements "rp" of "remPats".  In such a
     call to "a", "lhs'" is equal to "lhs" if "remPats" is "NIL", and
     is otherwise undefined.  (The "lhs" argument is therefore
     probably only useful for single-pattern multi-patterns.)
     "MatchArgs" stops and returns "FALSE" if a call to "a" returns
     "FALSE"; otherwise, it returns "TRUE".  Requires that "e" is a
     cdr-type node whose "length" is equal to the length of "pat", and
     that "remPats" is a list of "Pattern"s.  MatchArgs leaves "theta"
     unchanged. *)
  BEGIN
    IF pat = NIL THEN
      IF remPats = NIL THEN
        RETURN a(theta, lhs, data)
      ELSE
        (*
        VAR remPatHead: RefList.T := remPats.head;
            remPatHeadFunc: FSym := remPatHead.head;
        BEGIN
          MatchLog("[" & Atom.ToText(remPatHeadFunc.sym) & ":")
        END (* BEGIN *);
        *)
        VAR res := MatchWork(remPats.head, remPats.tail, commits,
                             a, data, theta);
        BEGIN
          (* MatchLog("]"); *) RETURN res
        END (* BEGIN *);
      END (* IF *)
    ELSE
      VAR ep := NARROW(e, Parent); BEGIN
        PROCEDURE P(VAR theta: MatchingRule.Substitution; lhs: T; data: REFANY):
          BOOLEAN RAISES {Prover.Timeout} =
          (* Call "a(theta, lhs, data)" for each "theta" in some set of
             substitutions that generalizes the set of all "theta" that
             extend "s" and are such that "theta(pat.tail)" is
             represented by "ep.cdr". *)
          BEGIN
            RETURN MatchArgs(pat.tail, remPats, commits,
                             ep.cdr, theta, a, lhs, data);
          END P;
        BEGIN
          (* MatchLog("<"); *)
          VAR res := MatchEq(pat.head, commits, ep.car, theta, P, lhs, data);
          BEGIN
            (* MatchLog(">"); *) RETURN res
          END (* BEGIN *)
        END (* BEGIN *)
      END (* BEGIN *)
    END (* IF *)
  END MatchArgs;

PROCEDURE MatchEq(pat: REFANY; READONLY commits: MatchingRule.BoolArr;
                  e: T; VAR theta: MatchingRule.Substitution;
                  a: MatchAction; lhs: T; data: REFANY): BOOLEAN
          RAISES {Prover.Timeout} =
  (* "MatchEq(pat, remPats, e, theta, a, lhs, data)" calls
     "a(theta, lhs, data)" for each "theta" in some set of
     substitutions that generalizes the set of all "theta" that extend
     "s" and are such that "theta(pat)" is equivalent to "e".
     MatchEq stops and returns "FALSE" if a call to "a" returns
     "FALSE"; otherwise, it returns "TRUE".  Requires that "e" is a
     car-type node.  MatchEq leaves "s" unchanged. *)
  BEGIN
    INC(stats.m.enodesExamined);
    TYPECASE pat OF <*NOWARN*>
    | MatchingRule.PatVar(ri) =>
        <*ASSERT e.parent = NIL OR IsCar(e.root) *>
        VAR b := theta[ri^]; BEGIN
          IF b = NIL THEN
            theta[ri^] := e.root;
            (* MatchLog("$"); *)
            VAR res := a(theta, lhs, data); BEGIN
              theta[ri^] := NIL; RETURN res
            END (* BEGIN *)
          ELSIF b.root = e.root THEN
            (* MatchLog("V"); *) RETURN a(theta, lhs, data)
          ELSE
            (* MatchLog("v"); *) RETURN TRUE
          END
        END (* BEGIN *)
    | Atom.T(at) =>
        IF e.root = FromSym(at).root THEN
          (* MatchLog("A"); *) RETURN a(theta, lhs, data)
        ELSE
          (* MatchLog("a"); *) RETURN TRUE
        END (* IF *)
    | T(e2) =>
        IF e.root = e2.root THEN
          (* MatchLog("E"); *) RETURN a(theta, lhs, data)
        ELSE
          (* MatchLog("e"); *) RETURN TRUE
        END (* IF *)
    | REF INTEGER(ri) =>
        IF e.root = FromInt(ri^).root THEN
          (* MatchLog("I"); *) RETURN a(theta, lhs, data)
        ELSE
          (* MatchLog("i"); *) RETURN TRUE
        END (* IF *)
    | REF LONGREAL(rl) =>
        IF e.root = FromLongReal(rl^).root THEN
          (* MatchLog("R"); *) RETURN a(theta, lhs, data)
        ELSE
          (* MatchLog("r"); *) RETURN TRUE
        END (* IF *)
    | RefList.T(patl) =>
        VAR func: FSym := patl.head; er := e.root; BEGIN
          (* MatchLog("(" & Atom.ToText(func.sym) & ":"); *)
          IF func.parentSize < er.size THEN
            IF func.parent # NIL THEN
	      VAR p := func.parent; BEGIN
		REPEAT
                  INC(stats.m.enodesExamined);
		  IF p.root = er AND p.cgPtr = p THEN
		    IF NOT MatchArgs(patl.tail, NIL, commits, p.cdr,
				     theta, a, lhs, data) THEN
		      (* MatchLog(")"); *) RETURN FALSE
		    END (* IF *)
		  ELSE
		    (* MatchLog(",") *)
		  END (* IF *);
		  p := p.same[TRUE]
		UNTIL p = func.parent;
                DEC(stats.m.enodesExamined)
	      END (* BEGIN *)
            END (* IF *)
          ELSE
            VAR eq := e; BEGIN
	      REPEAT
                INC(stats.m.enodesExamined);
		IF ISTYPE(eq, Parent) THEN
		  VAR eqPar := NARROW(eq, Parent); BEGIN
		    IF eqPar.car = func AND eqPar.cgPtr = eqPar THEN
		      IF NOT MatchArgs(patl.tail, NIL, commits, eqPar.cdr,
                                       theta, a, lhs, data) THEN
			(* MatchLog(")"); *) RETURN FALSE
		      END (* IF *)
		    ELSE
		      (* MatchLog(".") *)
                    END (* IF *)
		  END (* BEGIN *)
		ELSE
                  (* MatchLog(".") *)
		END (* IF *);
		eq := eq.next
	      UNTIL eq = e;
              DEC(stats.m.enodesExamined)
            END (* BEGIN *)
          END (* IF *);
          (* MatchLog(")"); *) RETURN TRUE
        END (* BEGIN *)
    END (* TYPECASE *)
  END MatchEq;

VAR matchDebug: CARDINAL := 0;

PROCEDURE UnitMatch() RAISES {Prover.Timeout} =
  PROCEDURE MatchUnitClause(VAR s: MatchingRule.Substitution;
                            lhs: T;
                            data: REFANY): BOOLEAN =
    VAR mr: MatchingRule.T := data;
        lit: AF.Lit;
        template: Sx.T;
    BEGIN
      VAR fp := FingerprintMatch(mr.id, s); BEGIN
	IF matchFPTable.put(fp, NIL) THEN
	  EtpRedundantMatch(mr.id, mr.parId);
          INC(stats.m.unitRedundant);
	  RETURN TRUE
	ELSE
	  undoFP.addhi(fp);
	  UndoStackPush(UndoType.AddMatchFP)
	END (* IF *)
      END (* BEGIN *);
      EtpMatchFound(mr.id, mr.parId);
      IF mr.opSym # NIL THEN
        VAR rhs := ClausePrivate.Intern(mr.template, s);
            litSx := MatchingRule.SubstSxCons(
                         mr.opSym,
                         MatchingRule.SubstSxCons(
                             lhs, MatchingRule.SubstSxCons(rhs, NIL)));
        BEGIN
          lit := ClausePrivate.SxToLiteral(           <*NOWARN*>
                     litSx, MatchingRule.EmptySub);
          MatchingRule.SubstSxFree(litSx)
        END (* BEGIN *)
      ELSE
        MatchingRule.parentRule := mr;
        lit := ClausePrivate.SxToLiteral(mr.template, s); <*NOWARN*>
        MatchingRule.parentRule := NIL;
      END (* IF *);

      IF matchDebug > 0 THEN
        Wr.PutText(Stdio.stdout, "Found Unit match:\n");
        IF matchDebug > 1 THEN
          Wr.PutText(Stdio.stdout, "  Pattern:\n");
          Sx.Print(Stdio.stdout, MatchingRule.PatToPrintableSx(mr.pats[0].p));
          Wr.PutText(Stdio.stdout, "\n");
          Wr.PutText(Stdio.stdout, "  Substitution:\n");
          FOR i := 0 TO LAST(s) DO
            IF s[i] # NIL THEN
              Wr.PutText(Stdio.stdout, "$$" & Fmt.Int(i) & "  <- ");
              Sx.Print(Stdio.stdout, MatchingRule.PatToPrintableSx(s[i]));
              Wr.PutText(Stdio.stdout, "\n")
            END (* IF *)
          END (* FOR *)
        END (* IF *);
        Wr.PutText(Stdio.stdout, "    ");
        Sx.Print(Stdio.stdout, MatchingRule.PatToPrintableSx(template));
        Wr.PutText(Stdio.stdout, "\n");
        Wr.Flush(Stdio.stdout)
      END (* IF *);
      (* Add the literal to the result. *)
      INC(stats.m.unit);
      VAR n: INTEGER; BEGIN
        IF NOT stats.m.uMatchCountTable.get(mr.id, n) THEN n := 0 END (* IF *);
        EVAL stats.m.uMatchCountTable.put(mr.id, n+1)
      END (* BEGIN *);
      ContextPrivate.Propagate(lit);
      RETURN TRUE
    END MatchUnitClause;
  BEGIN
    INC(stats.m.uRounds);

    IF matchDebug > 0 THEN
      Wr.PutText(Stdio.stdout,
                 "*** In AssertUnits(" & Fmt.Int(stats.m.uRounds) & ").\n");
      Wr.Flush(Stdio.stdout)
    END (* IF *);
    Context.opsEnabled[Context.Ops.UnitMatch] := FALSE;

    IF NOT Prover.noPatElems THEN
      PairSet.SymClose(unitPpPairSet);
      stats.m.maxPSetOcc := MAX(stats.m.maxPSetOcc, ParentSet.Size(unitPSet))
    END (* IF *);

    VAR unitRules := unitMatchRules;
        thisMatchTime := timer;
    BEGIN
      INC(timer);
      WHILE unitRules # NIL DO
        VAR rule: MatchingRule.T := unitRules.head;
            commits := rule.commits;
            initIters := stats.m.mwiters;
        BEGIN
          IF matchDebug > 1 THEN
            Wr.PutText(Stdio.stdout, "+++ Working on rule:\n");
            Sx.Print(Stdio.stdout, rule.toSx());
            Wr.PutText(Stdio.stdout, "\n");
            Wr.Flush(Stdio.stdout)
          END;
          FOR i := 0 TO LAST(rule.pats^) DO
            IF Thread.TestAlert() THEN RAISE Prover.Timeout END;
            VAR po: MatchingRule.PatObj := rule.pats[i]; BEGIN
	      IF matchDebug > 1 THEN
		Wr.PutText(Stdio.stdout, "--- Working on pattern:\n");
		Sx.Print(Stdio.stdout, MatchingRule.PatToPrintableSx(po.p));
		Wr.PutText(Stdio.stdout, "\n");
		Wr.Flush(Stdio.stdout)
	      END (* IF *);
	      EtpMatchStart(rule.id, rule.parId, rule.trivial, rule.width);
              VAR matchTime: INTEGER; BEGIN
                IF po.matched AND NOT Prover.noModTimes THEN
                  matchTime := lastNUMatchTime
                ELSE
                  matchTime := -1
                END (* IF *);
                IF NOT po.matched OR Prover.noPatElems OR
                  PairSet.Overlap(po.pcPairs, unitPcPairSet) OR
                  PairSet.Overlap(po.ppPairs, unitPpPairSet) OR
                  ParentSet.Overlap(po.pars, unitPSet) THEN
                  IF AllEnodePiecesActive(po) THEN
                    ComputeEnabledPieces(rule, po, unitPcPairSet, unitPpPairSet,
                                         unitPSet);
                    IF matchDebug > 1 THEN
                      Wr.PutText(Stdio.stdout, "; Enode.UnitMatch: about to call Match\n");
                      Wr.Flush(Stdio.stdout);
                    END;
                    EVAL Match(po, commits.head, MatchUnitClause,
                               rule, matchTime);
                    IF NOT po.matched THEN
                      UndoStackPush(UndoType.PatternMatched, po);
                      po.matched := TRUE
                    END (* IF *)
                  END (* IF *)
                END (* IF *);
	      END (* IF *);
	      EtpMatchEnd();
            END (* BEGIN *);
            commits := commits.tail
          END (* FOR *);
          CreditMWItersToRule(rule.id, stats.m.mwiters-initIters)
        END (* BEGIN *);
        unitRules := unitRules.tail
      END (* WHILE *);
      IF NOT Prover.noPatElems THEN
	PairSet.UnionD(nuPcPairSet, unitPcPairSet);
	PairSet.MakeEmpty(unitPcPairSet);
	PairSet.UnionD(nuPpPairSet, unitPpPairSet);
	PairSet.MakeEmpty(unitPpPairSet);

	ParentSet.UnionD(nuPSet, unitPSet);
	ParentSet.MakeEmpty(unitPSet)
      END (* IF *);
      lastUMatchTime := thisMatchTime
    END (* BEGIN *)
  END UnitMatch;

PROCEDURE CreditMWItersToRule(id, iters: INTEGER) =
  VAR i: INTEGER; BEGIN
    IF iters > 0 THEN
      IF NOT stats.m.ruleMWIterTable.get(id, i) THEN i := 0 END (* IF *);
      INC(i, iters);
      EVAL stats.m.ruleMWIterTable.put(id, i)
    END (* IF *)
  END CreditMWItersToRule;

PROCEDURE NonUnitMatch(): Clause.T RAISES {Prover.Timeout} =
  VAR res: Clause.T := NIL;
  PROCEDURE MatchNonUnitRule(VAR s: MatchingRule.Substitution;
                             <*UNUSED*> lhs: T;
                             data: REFANY): BOOLEAN =
    VAR mr: MatchingRule.T := data; BEGIN
      VAR fp := FingerprintMatch(mr.id, s); BEGIN
	IF matchFPTable.put(fp, NIL) THEN
	  EtpRedundantMatch(mr.id, mr.parId);
          INC(stats.m.nonUnitRedundant);
	  RETURN TRUE
	ELSE
	  undoFP.addhi(fp); 
	  UndoStackPush(UndoType.AddMatchFP)
	END (* IF *)
      END (* BEGIN *);
      IF matchDebug > 0 THEN
        Wr.PutText(Stdio.stdout, "Found Non-unit match:\n");
        IF matchDebug >= 1 THEN
          Wr.PutText(Stdio.stdout, "  Rule id# " & Fmt.Int(mr.id) & "\n");
          Wr.PutText(Stdio.stdout, "  Pattern:\n");
          Sx.Print(Stdio.stdout, MatchingRule.PatToPrintableSx(mr.pats[0].p));
          Wr.PutText(Stdio.stdout, "\n");
          Wr.PutText(Stdio.stdout, "  Substitution:\n");
          FOR i := 0 TO LAST(s) DO
            IF s[i] # NIL THEN
              Wr.PutText(Stdio.stdout, "$$" & Fmt.Int(i) & "  <- ");
              Sx.Print(Stdio.stdout, MatchingRule.PatToPrintableSx(s[i]));
              Wr.PutText(Stdio.stdout, "\n")
            END (* IF *)
          END (* FOR *)
        END (* IF *);
        Wr.PutText(Stdio.stdout, "    ");
        Sx.Print(Stdio.stdout, MatchingRule.PatToPrintableSx(mr.template));
        Wr.PutText(Stdio.stdout, "\n");
        Wr.Flush(Stdio.stdout)
      END (* IF *);
      VAR cl: Clause.T; <*FATAL Prover.Error*> BEGIN
        MatchingRule.parentRule := mr;
        
        IF mr.clausal THEN
          cl := ClausePrivate.SxToClause(mr.template, s);
        ELSE
          (*ASSERT PredSx.IsAStar(sx)*)
          VAR plit := Clause.CNF(mr.template, s); BEGIN
            cl := NIL;
            TYPECASE plit.af OF
            | ProxyProp.T(p) =>
		VAR c, w: CARDINAL; BEGIN
		  ProxyProp.CNFSize(p, plit.sense, c, w);
		  IF (c-1) * (w-1) < Prover.maxProxyPropCNF THEN
		    VAR cnf := ProxyProp.CNF(p, plit.sense); BEGIN
                      WHILE cnf # NIL DO
                        cl := ClausePrivate.ListAppendD(
                                  NEW(Clause.T, lits := cnf.head,
                                      mr := mr).init(),
                                  cl);
                        cnf := cnf.tail
                      END (* WHILE *)
                    END (* BEGIN *)
                  ELSE
                    cl := NEW(Clause.T, lits := RefList.List1(plit),
                              mr := mr).init();
		  END (* IF *)
		END (* BEGIN *)
            ELSE
                cl := NEW(Clause.T, lits := RefList.List1(plit),
                          mr := mr).init();
            END (* TYPECASE *)
          END (* BEGIN *)
        END (* IF *);
        MatchingRule.parentRule := NIL;
        cl.mr := mr;
        res := ClausePrivate.ListAppendD(cl, res)
      END (* BEGIN *);
      INC(stats.m.nonUnit);
      EtpMatchFound(mr.id, mr.parId);
      VAR n: INTEGER; BEGIN
        IF NOT stats.m.nuMatchCountTable.get(mr.id, n) THEN
          n := 0
        END (* IF *);
        EVAL stats.m.nuMatchCountTable.put(mr.id, n+1)
      END (* BEGIN *);
      RETURN TRUE
    END MatchNonUnitRule;
  BEGIN
    IF NOT Prover.noPatElems THEN
      PairSet.UnionD(nuPcPairSet, unitPcPairSet);
      PairSet.UnionD(nuPpPairSet, unitPpPairSet);
      PairSet.SymClose(nuPpPairSet);
      ParentSet.UnionD(nuPSet, unitPSet);
      stats.m.maxPSetOcc := MAX(stats.m.maxPSetOcc, ParentSet.Size(nuPSet))
    END (* IF *);

    INC(stats.m.nuRounds);
    IF matchDebug > 0 THEN
      Wr.PutText(Stdio.stdout,
                 "*** In FindNonUnits(" & Fmt.Int(stats.m.nuRounds) & ").\n");
      Wr.Flush(Stdio.stdout)
    END (* IF *);

    IF nuconsTest THEN RTCollector.Disable() END (* IF *);

    VAR nonUnitRules := nonUnitMatchRules;
        thisMatchTime := timer;
    BEGIN
      INC(timer);
      WHILE nonUnitRules # NIL DO
        IF matchDebug > 1 THEN
          Wr.PutText(Stdio.stdout, "+++ Working on rule:\n");
          Sx.Print(Stdio.stdout,
                   NARROW(nonUnitRules.head, MatchingRule.T).toSx());
          Wr.PutText(Stdio.stdout, "\n");
          Wr.Flush(Stdio.stdout)
        END;
        VAR rule: MatchingRule.T := nonUnitRules.head;
            commits := rule.commits;
            initIters := stats.m.mwiters;
        BEGIN
          FOR i := 0 TO LAST(rule.pats^) DO
            IF Thread.TestAlert() THEN RAISE Prover.Timeout END;
            VAR po: MatchingRule.PatObj := rule.pats[i]; BEGIN
	      IF matchDebug > 1 THEN
		Wr.PutText(Stdio.stdout, "--- Working on pattern:\n");
		Sx.Print(Stdio.stdout, MatchingRule.PatToPrintableSx(po.p));
		Wr.PutText(Stdio.stdout, "\n");
		Wr.Flush(Stdio.stdout)
	      END (* IF *);
	      EtpMatchStart(rule.id, rule.parId, rule.trivial, rule.width);
              VAR matchTime: INTEGER; BEGIN
                IF po.matched AND NOT Prover.noModTimes THEN
                  matchTime := lastNUMatchTime
                ELSE
                  matchTime := -1
                END (* IF *);
                IF NOT po.matched OR Prover.noPatElems OR
                  PairSet.Overlap(po.pcPairs, nuPcPairSet) OR
                  PairSet.Overlap(po.ppPairs, nuPpPairSet) OR
                  ParentSet.Overlap(po.pars, nuPSet) THEN
                  IF AllEnodePiecesActive(po) THEN
                    ComputeEnabledPieces(rule, po, nuPcPairSet, nuPpPairSet,
                                         nuPSet);
                    IF matchDebug > 1 THEN
                      Wr.PutText(Stdio.stdout, "; Enode.NonUnitMatch: about to call Match\n");
                      Wr.Flush(Stdio.stdout);
                    END;
                    EVAL Match(po, commits.head, MatchNonUnitRule,
                               rule, matchTime);
                    IF NOT po.matched THEN
                      UndoStackPush(UndoType.PatternMatched, po);
                      po.matched := TRUE
                    END (* IF *)
                  END (* IF *)
                END (* IF *)
              END (* BEGIN *);
	      EtpMatchEnd()
            END (* BEGIN *);
            commits := commits.tail
          END (* FOR *);
          CreditMWItersToRule(rule.id, stats.m.mwiters-initIters)
        END (* BEGIN *);
        nonUnitRules := nonUnitRules.tail
      END (* WHILE *);
      IF NOT Prover.noPatElems THEN
        PairSet.MakeEmpty(nuPcPairSet);
        PairSet.MakeEmpty(nuPpPairSet);
        ParentSet.MakeEmpty(nuPSet)
      END (* IF *);
      lastNUMatchTime := thisMatchTime
    END (* BEGIN *);
    Context.opsEnabled[Context.Ops.RestrictedNUMatch] := FALSE;
    VAR cl1 := res; BEGIN
      WHILE cl1 # NIL DO
        INC(stats.m.totNUClauses);
        VAR lits := cl1.lits; opaque := TRUE; BEGIN
          WHILE lits # NIL DO
            INC(stats.m.totNULits);
            VAR lit: AF.Lit := lits.head; BEGIN
              IF ISTYPE(lit.af, ProxyProp.T) THEN
                INC(stats.m.ppvNULits)
              ELSE
                opaque := FALSE
              END (* IF *)
            END (* BEGIN *);
            lits := lits.tail
          END (* WHILE *);
          IF opaque THEN INC(stats.m.opaqueNUClauses) END (* IF *)
        END (* BEGIN *);
        cl1 := cl1.succ
      END (* WHILE *)
    END (* BEGIN *);
    IF nuMatchStatIntrvl > 0 AND
      stats.m.nuRounds MOD nuMatchStatIntrvl = 0 THEN
      Stats()
    END (* IF *);
    IF nuconsTest THEN RTCollector.Enable() END (* IF *);
    RETURN res
  END NonUnitMatch;

<*UNUSED*>
PROCEDURE PrintSubst(wr: Wr.T; READONLY s: MatchingRule.Substitution) =
  BEGIN
    FOR i := 0 TO LAST(s) DO
      IF s[i] = NIL THEN RETURN END (* IF *);
      Wr.PutText(wr, Fmt.Int(s[i].root.id));
      IF i # LAST(s) THEN Wr.PutText(wr, " ") END (* IF *)
    END (* FOR *)
  END PrintSubst;

PROCEDURE FingerprintMatch(ruleId: INTEGER;
                           READONLY s: MatchingRule.Substitution): FPrint.T =
  VAR res := FPrint.FromInt(ruleId); BEGIN
    VAR j := 0; BEGIN
      WHILE s[j] # NIL AND j < NUMBER(s) DO
        res := FPrint.AddInt(res, s[j].root.id);
        INC(j)
      END (* WHILE *)
    END (* BEGIN *);
    RETURN res
  END FingerprintMatch;

TYPE
  MatchUndoRec = RECORD
    unitMatchRules, nonUnitMatchRules: RefList.T;
    lastUMatchTime, lastNUMatchTime: INTEGER;
  END (* RECORD *);
  MatchUndoStack = REF ARRAY OF MatchUndoRec;
CONST
  NullMatchUndoRec = MatchUndoRec{NIL, NIL, 0, 0};
VAR
  matchUndoStack: MatchUndoStack;
  matchUndoSP: CARDINAL;

TYPE
  MatchStatRec = RECORD
    unit, nonUnit, unitRedundant, nonUnitRedundant: CARDINAL := 0;
    uRounds, nuRounds: CARDINAL := 0;
    uMatchCountTable, nuMatchCountTable, matchSplitTable: IntIntTbl.T := NIL;
    mwcalls, mwnullcalls, mwiters, mwmatchiters, mwnulliters: INTEGER := 0;
    ruleMWIterTable: IntIntTbl.T := NIL;
    symMWIterTable: AtomIntTbl.T := NIL;
    totNUClauses, opaqueNUClauses: INTEGER := 0;
    totNULits, ppvNULits: INTEGER := 0;
    avoidedPieces, unavoidedPieces, unavoidedEnabled := 0;
    maxPSetOcc := 0;
    enodesExamined := 0;
  END (* RECORD *);
  MiscRecStatRec = RECORD
    nsForbids, nsDCs, nsProps, nsOrdNode, nsSimpUnknown: CARDINAL := 0;
  END (* RECORD *);
  StatRec = RECORD
    m := MatchStatRec{};
    misc := MiscRecStatRec{};
    nLeaf, maxLeaf, maxTotnodes: CARDINAL := 4;
    nCons, nActive, nCarPars: CARDINAL := 0;
    maxCons, maxActive, maxCarPars: CARDINAL := 0;
    nPredInsts, nPredMapInsts := 0;
    nRedPredInsts, nRedPredMapInsts := 0;
    nPropEq := 0;
  END (* RECORD *);
VAR
  stats: StatRec;

PROCEDURE MInit() =
  BEGIN
    unitMatchRules := NIL;
    nonUnitMatchRules := NIL;
    matchUndoStack := NEW(MatchUndoStack, 100);
    matchUndoSP := 0;
    depth := 0;
    maxDepthSeen := 0;
    stats.m := MatchStatRec{};

    stats.m.uMatchCountTable := NEW(IntIntTbl.Default).init();
    stats.m.nuMatchCountTable := NEW(IntIntTbl.Default).init();
    stats.m.matchSplitTable := NEW(IntIntTbl.Default).init();
    stats.m.ruleMWIterTable := NEW(IntIntTbl.Default).init();
    stats.m.symMWIterTable := NEW(AtomIntTbl.Default).init();

    VAR idFile := Env.Get("PROVER_ENABLE_LOG"); BEGIN
      IF idFile # NIL THEN
        IF Text.Length(idFile) = 0 THEN
          idFile := "/tmp/enable.log"
        END (* IF *);
        enablePieceLog := FileWr.Open(idFile) <*NOWARN*>
      ELSE
        enablePieceLog := NIL
      END (* IF *)
    END (* BEGIN *);
    MatchingRule.Init()
  END MInit;

PROCEDURE MPush() =
  BEGIN
    matchUndoStack[matchUndoSP].unitMatchRules := unitMatchRules;
    matchUndoStack[matchUndoSP].nonUnitMatchRules := nonUnitMatchRules;
    matchUndoStack[matchUndoSP].lastUMatchTime := lastUMatchTime;
    matchUndoStack[matchUndoSP].lastNUMatchTime := lastNUMatchTime;
    INC(matchUndoSP);
    IF matchUndoSP = NUMBER(matchUndoStack^) THEN
      VAR new := NEW(MatchUndoStack, 2 * matchUndoSP); BEGIN
        SUBARRAY(new^, 0, matchUndoSP) := matchUndoStack^;
        matchUndoStack := new
      END (* BEGIN *)
    END (* IF *);
    MatchingRule.Push()
  END MPush;

PROCEDURE MPop() =
  BEGIN
    MatchingRule.Pop();
    matchUndoStack[matchUndoSP] := NullMatchUndoRec;
    DEC(matchUndoSP);
    unitMatchRules := matchUndoStack[matchUndoSP].unitMatchRules;
    nonUnitMatchRules := matchUndoStack[matchUndoSP].nonUnitMatchRules;
    lastUMatchTime := matchUndoStack[matchUndoSP].lastUMatchTime;
    lastNUMatchTime := matchUndoStack[matchUndoSP].lastNUMatchTime
  END MPop;

(* Ensures that the modification time of every parent node from which
   "e" is {\it reachable} (by iterating: follow a car pointer and
   choose any any member of the resulting equivalence class, or follow
   a cdr pointer) is equal to "timer". *)
PROCEDURE SetModTimes(e: T) =
  BEGIN
    IF e.root.parent # NIL THEN
      Context.opsEnabled[Context.Ops.UnitMatch] := TRUE;
      Context.opsEnabled[Context.Ops.RestrictedNUMatch] := TRUE;
      SetParentModTimes(e.parent, IsCar(e.root))
    END (* IF *)
  END SetModTimes;

PROCEDURE SetParentModTimes(p: Parent; isCar: BOOLEAN) =
  VAR lp := p; BEGIN
    REPEAT
      IF lp.modTime # timer THEN
        TYPECASE lp OF
        | CarParent(cp) =>
            IF NARROW(cp.car, FSym).terms # cp THEN
              UndoStackPush(UndoType.UpdateMTAndMoveToFront,
                            cp.prev, cp.modTime);
              <*ASSERT cp.prev # cp *>
              TermList_MoveToFront(cp.car, cp)
            ELSE
              UndoStackPush(UndoType.UpdateMT, lp, lp.modTime)
            END (* IF *)
        ELSE
            UndoStackPush(UndoType.UpdateMT, lp, lp.modTime)
        END (* TYPECASE *);
        lp.modTime := timer;
        IF lp.root.parent # NIL THEN
          SetParentModTimes(lp.root.parent, IsCar(lp.root))
        END (* IF *);
      END (* IF *);
      lp := lp.same[isCar]
    UNTIL lp = p
  END SetParentModTimes;

(* Add "p", whose modification time is assumed to be 0, to the tail
   of the term list of "fs". *)
PROCEDURE TermList_AppendToRear(fs: FSym; p: CarParent) =
  BEGIN
    (*
    CheckWellFormed(fs.terms);
    *)
    IF fs.terms = NIL THEN
      p.link := p; p.prev := p; fs.terms := p
    ELSE
      p.link := fs.terms;
      p.prev := fs.terms.prev;
      fs.terms.prev.link := p;
      fs.terms.prev := p
    END (* IF *);
    (*
    CheckWellFormed(fs.terms);
    *)
    UndoStackPush(UndoType.TLAppendToRear, p)
  END TermList_AppendToRear;

(* Requires "p" different from "fs.terms".  Ensures that "p", whose
   modification time must be equal to "timer", is at the head
   "fs.terms". *)
PROCEDURE TermList_MoveToFront(fs: FSym; p: CarParent) =
  BEGIN
    <*ASSERT fs.terms # NIL AND p # NIL AND fs.terms # p *>
    (*
    CheckWellFormed(fs.terms);
    *)
    (* delete *)
    p.prev.link := p.link;
    p.link.prev := p.prev;
    (* insert *)
    p.prev := fs.terms.prev;
    p.link := fs.terms;
    fs.terms.prev.link := p;
    fs.terms.prev := p;
    fs.terms := p;
    (*
    CheckWellFormed(fs.terms);
    *)
  END TermList_MoveToFront;

<*UNUSED*>
(* Check the well-formedness of the term list "node". *)
PROCEDURE CheckWellFormed(node: CarParent) =
  VAR firstNode := node; car: T; i := 0; BEGIN
    IF node = NIL THEN RETURN END (* IF *);
    car := firstNode.car;
    REPEAT
      IF NOT (node.prev.link = node AND node.link.prev = node AND
              node.car = car) THEN
        <*ASSERT FALSE*>
      END (* IF *);
      node := node.link; INC(i)
    UNTIL node = firstNode OR i = 1000;
    <*ASSERT i < 1000 *>
  END CheckWellFormed;

PROCEDURE AttributeCaseSplit(id: CARDINAL; <*UNUSED*> hash: INTEGER) =
  VAR n: INTEGER; BEGIN
    IF NOT stats.m.matchSplitTable.get(id, n) THEN n := 0 END (* IF *);
    EVAL stats.m.matchSplitTable.put(id, n+1)
  END AttributeCaseSplit;

PROCEDURE Stats() =
  BEGIN
    Wr.PutText(Stdio.stdout,
               "\n\nIn " & Fmt.Int(stats.m.uRounds) &
               " rounds of unit matching, " &
               Fmt.Int(stats.m.nuRounds) & " rounds of non-unit matching,\n" &
               Fmt.Int(MatchingRule.stats.ruleActivations) &
               " rule activations,\n" &
               "Matches found: " & Fmt.Int(stats.m.unit) & " by unit rules" &
               " (plus " & Fmt.Int(stats.m.unitRedundant) & " redundant),\n" &
               "               " & Fmt.Int(stats.m.nonUnit) &
               " by non-unit rules" &
               " (plus " & Fmt.Int(stats.m.nonUnitRedundant) &
               " redundant)\n" &
               "               Max match depth = " & Fmt.Int(maxDepthSeen) &
               ".\n");
    Wr.PutText(Stdio.stdout,
               "Examined " & Fmt.Int(stats.m.enodesExamined) &
               " enodes during matching.\n");

    Wr.PutText(Stdio.stdout,
               "\nTop 5/" & Fmt.Int(RefList.Length(unitMatchRules)) &
               " unit matching rules by number of matches (#, rule):\n");
    PrintMatchTbl(5, stats.m.uMatchCountTable);

    Wr.PutText(Stdio.stdout,
               "\nTop 5/" & Fmt.Int(RefList.Length(nonUnitMatchRules)) &
               " non-unit matching rules by number of matches (#, rule):\n");
    PrintMatchTbl(5, stats.m.nuMatchCountTable);

    Wr.PutText(Stdio.stdout,
               "\nTop 5 non-unit matching rules by number of case splits " &
               "(#, rule):\n");
    PrintMatchTbl(5, stats.m.matchSplitTable);

    Wr.PutText(Stdio.stdout,
               "\nTop 5 non-unit matching rules by score " &
               "(#, rule):\n");
    MatchingRule.Stats();

    Wr.PutText(Stdio.stdout,
               "\nTop 5 matching rules by number of MatchWork iterations " &
               "(#, rule):\n");
    PrintMatchTbl(5, stats.m.ruleMWIterTable);
    
    Wr.PutText(Stdio.stdout,
               "\nTop 5 function symbols by number of MatchWork iterations " &
               "(#, symbol):\n");
    PrintAtomIntTbl(5, stats.m.symMWIterTable);
    
    Wr.PutText(Stdio.stdout,
               "\n      " &
               "At end, had " & Fmt.Int(stats.nCons + stats.nLeaf) & 
               " total nodes (" & Fmt.Int(stats.nCons) & " cons, " &
               Fmt.Int(stats.nCarPars) & " of those cars, " &
               Fmt.Int(stats.nLeaf) & " leaf),\n               " &
               Fmt.Int(stats.nActive) & " of these active.\n");
    Wr.PutText(Stdio.stdout,
               "\n      " &
               "Maximum values: " & Fmt.Int(stats.maxTotnodes) & 
               " total nodes (" & Fmt.Int(stats.maxCons) & " cons, " &
               Fmt.Int(stats.maxCarPars) & " of those cars, " &
               Fmt.Int(stats.maxLeaf) & " leaf),\n               " &
               "Maximum active nodes: " & Fmt.Int(stats.maxActive) & ".\n");
    Wr.PutText(Stdio.stdout,
               "\n      " &
               "Of " & Fmt.Int(stats.m.mwcalls) & " calls to MatchWork, " &
               Fmt.Int(stats.m.mwnullcalls));
    IF stats.m.mwcalls # 0 THEN
      Wr.PutText(Stdio.stdout,
                 " (" & Fmt.Real(FLOAT(stats.m.mwnullcalls)*100.0/
                                 FLOAT(stats.m.mwcalls),
                                 prec := 3) & "%)")
    END (* IF *);
    Wr.PutText(Stdio.stdout,
               " made no calls to MatchArgs;\n" &
               "      of " & Fmt.Int(stats.m.mwiters) & " iterations of the " &
               "MatchWorkLoop,\n         " &
               Fmt.Int(stats.m.mwnulliters));
    IF stats.m.mwiters # 0 THEN
      Wr.PutText(Stdio.stdout,
                 " (" & Fmt.Real(FLOAT(stats.m.mwnulliters)*100.0/
                                 FLOAT(stats.m.mwiters),
                                 prec := 3) & "%)")
    END (* IF *);
    Wr.PutText(Stdio.stdout,
               " in null calls,\n         " & Fmt.Int(stats.m.mwmatchiters));
    IF stats.m.mwiters # 0 THEN
      Wr.PutText(
          Stdio.stdout,
          " (" &
          Fmt.Real(FLOAT(stats.m.mwmatchiters)*100.0/FLOAT(stats.m.mwiters),
                   prec := 3) & "%)")
    END (* IF *);
    Wr.PutText(Stdio.stdout, " called MatchArgs.\n\n");
    Wr.PutText(Stdio.stdout,
               "\n      " &
               Fmt.Int(stats.m.opaqueNUClauses) & " of " &
               Fmt.Int(stats.m.totNUClauses) & " NU clauses are opaque");
    IF stats.m.totNUClauses # 0 THEN
      Wr.PutText(Stdio.stdout,
                 " (" & Fmt.Real(FLOAT(stats.m.opaqueNUClauses) * 100.0 /
                                FLOAT(stats.m.totNUClauses), prec := 3) &
                 "%)")
    END (* IF *);
    Wr.PutText(Stdio.stdout, ".\n");
    Wr.PutText(Stdio.stdout,
               "      " &
               Fmt.Int(stats.m.ppvNULits) & " of " &
               Fmt.Int(stats.m.totNULits) & " NU lits are ppvs");
    IF stats.m.totNULits # 0 THEN
      Wr.PutText(Stdio.stdout, " (" &
                 Fmt.Real(FLOAT(stats.m.ppvNULits) * 100.0 /
                          FLOAT(stats.m.totNULits), prec := 3) &
                 "%)")
    END (* IF *);
    Wr.PutText(Stdio.stdout, ".\n\n");
    stats.misc := MiscRecStatRec{};
    TRY
      EVAL MapEgraph(CountNS)
    EXCEPT
      Prover.Timeout =>
        RAISE Prover.Error("Enode.Stats: Unexpected Prover.Timeout")
    END;
    Wr.PutText(Stdio.stdout,
               "\n Non-standard enode attributes:\n" &
               "    forbids: " & Fmt.Int(stats.misc.nsForbids) & "\n" &
               "    distinction classes: " & Fmt.Int(stats.misc.nsDCs) & "\n" &
               "    props: " & Fmt.Int(stats.misc.nsProps) & "\n" &
               "    ordNodes: " & Fmt.Int(stats.misc.nsOrdNode) & "\n" &
               "    simplex unknowns: " &
               Fmt.Int(stats.misc.nsSimpUnknown) & "\n");
    Wr.PutText(Stdio.stdout, "\nTotal pushes on undo stack by UndoType:\n");
    FOR ut := FIRST(UndoType) TO LAST(UndoType) DO
      Wr.PutText(Stdio.stdout, "    " & Fmt.Pad(UndoTypeText[ut], 20) & 
        ": " & Fmt.Int(undoStats[ut]) & "\n")
    END (* FOR *);

    Wr.PutText(Stdio.stdout, "\n\nAvoided " & Fmt.Int(stats.m.avoidedPieces) &
      " pieces; of " & Fmt.Int(stats.m.unavoidedPieces) & " others, enabled " &
      Fmt.Int(stats.m.unavoidedEnabled) & ".\n");

    Wr.PutText(Stdio.stdout, "\n\nInstantiated " & Fmt.Int(stats.nPredInsts) &
      " predicates, " & Fmt.Int(stats.nRedPredInsts) &
      " of which were redundant;\n" &
      "  and " & Fmt.Int(stats.nPredMapInsts) & " predicate maps, of which " &
      Fmt.Int(stats.nRedPredMapInsts) & " were redundant.\n");

    Wr.PutText(Stdio.stdout, "\nPropagated " & Fmt.Int(stats.nPropEq) &
      " equalities.\n");

    Wr.PutText(Stdio.stdout, "\nMax occupancy of a Pset is " &
               Fmt.Int(stats.m.maxPSetOcc) & " out of " &
               Fmt.Int(ParentSet.Width) & ".\n");

    Wr.PutText(Stdio.stdout, "\n");
    Wr.Flush(Stdio.stdout);

    IF snapshotIntrvl # -1 THEN PrintEgraphStats() END (* IF *)

  END Stats;

PROCEDURE CountNS(e: T): BOOLEAN =
  BEGIN
    IF NOT e.props <= ActiveProp THEN INC(stats.misc.nsProps) END (* IF *);
    IF e.misc # NIL THEN
      IF e.misc.forbids # NIL THEN INC(stats.misc.nsForbids) END (* IF *);
      IF e.misc.distClasses # EmptyDistClassSet THEN
        INC(stats.misc.nsDCs)
      END (* IF *);
      IF e.misc.ordNode # NIL THEN INC(stats.misc.nsOrdNode) END (* IF *);
      IF e.misc.unknown # NIL THEN INC(stats.misc.nsSimpUnknown) END (* IF *)
    END (* IF *);
    RETURN TRUE
  END CountNS;

(* Applies "p" to every enode in the egraph and returns TRUE, unless
   "p" returns "FALSE", in which case it terminates the traversal and
   returns "FALSE". *)
PROCEDURE MapEgraph(p: PROCEDURE(e: T): BOOLEAN RAISES {Prover.Timeout}):
    BOOLEAN RAISES {Prover.Timeout} =
  VAR sym: Atom.T; eRA: REFANY;
      iter := symTab.iterate();
  BEGIN
    WHILE iter.next(sym, eRA) DO
      VAR e: Leaf := eRA; BEGIN
        IF NOT Visit(e, p) THEN RETURN FALSE END (* IF *)
      END (* BEGIN *)
    END (* WHILE *);
    IF NOT Visit(enil, p) THEN RETURN FALSE END (* IF *);
    RETURN TRUE
  END MapEgraph;

PROCEDURE Visit(e: T; p: PROCEDURE(e: T): BOOLEAN RAISES {Prover.Timeout}):
    BOOLEAN RAISES {Prover.Timeout} =
  BEGIN
    IF NOT p(e) THEN RETURN FALSE END (* IF *);
    IF e.parent # NIL AND IsCar(e.root) THEN
      VAR par := e.parent; BEGIN
        REPEAT
          IF par.car = e AND NOT Visit(par, p)  THEN RETURN FALSE END (* IF *);
          par := par.same[TRUE];
        UNTIL par = e.parent;
      END (* BEGIN *)
    END (* IF *);
    RETURN TRUE
  END Visit;

PROCEDURE PrintMatchTbl(n: INTEGER; tbl: IntIntTbl.T) =
  VAR sz := tbl.size();
      ipArr := NEW(REF ARRAY OF IntPair.T, sz);
      iter := tbl.iterate();
      id, count: INTEGER;
      i := 0;
  BEGIN
    WHILE iter.next(id, count) DO
      ipArr[i].i := id; ipArr[i].j := count; INC(i)
    END (* WHILE *);
    IntPairArraySort.Sort(ipArr^, IntPair.CompareJ);
    FOR j := LAST(ipArr^) TO MAX(LAST(ipArr^)-(n-1), 0) BY -1 DO
      Wr.PutText(Stdio.stdout, Fmt.Pad(Fmt.Int(ipArr[j].j), 10) & ":   ");
      Wr.PutText(Stdio.stdout, "rule " & Fmt.Int(ipArr[j].i) & "\n")
    END (* FOR *)
  END PrintMatchTbl;

PROCEDURE PrintAtomIntTbl(n: INTEGER; tbl: AtomIntTbl.T) =
  VAR sz := tbl.size();
      map := NEW(IntRefTbl.Default).init();
      ipArr := NEW(REF ARRAY OF IntPair.T, sz);
      iter := tbl.iterate();
      id: Atom.T; count: INTEGER;
      i := 0;
  BEGIN
    WHILE iter.next(id, count) DO
      VAR hash := Atom.Hash(id); BEGIN
        EVAL map.put(hash, id);
        ipArr[i].i := hash; ipArr[i].j := count; INC(i)
      END (* BEGIN *)
    END (* WHILE *);
    IntPairArraySort.Sort(ipArr^, IntPair.CompareJ);
    FOR j := LAST(ipArr^) TO MAX(LAST(ipArr^)-(n-1), 0) BY -1 DO
      Wr.PutText(Stdio.stdout, Fmt.Pad(Fmt.Int(ipArr[j].j), 10) & ":   ");
      VAR at: REFANY; b: BOOLEAN; BEGIN
	b := map.get(ipArr[j].i, at); <*ASSERT b*>
        Wr.PutText(Stdio.stdout, Atom.ToText(at) & "\n")
      END (* BEGIN *)
    END (* FOR *)
  END PrintAtomIntTbl;

(* Requires "e" and "f" to be roots; returns "TRUE" if and only if the
   merge of "e" and "f" has been forbidden. *)
PROCEDURE MergeIsForbidden(e, f: T): BOOLEAN =
  BEGIN
    IF e = f THEN
      RETURN FALSE
    ELSE
      IF e.misc # NIL AND f.misc # NIL AND
        e.misc.distClasses * f.misc.distClasses # EmptyDistClassSet THEN
        RETURN TRUE
      END (* IF *);
      IF e.misc # NIL AND e.misc.forbids # NIL AND
        f.misc # NIL AND f.misc.forbids # NIL THEN
        VAR ep := e.misc.forbids; fp := f.misc.forbids; BEGIN
          LOOP
            IF ep.t.root = f OR fp.t.root = e THEN
              RETURN TRUE
            END (* IF *);
            ep := ep.next; fp := fp.next;
          IF ep = e.misc.forbids OR fp = f.misc.forbids THEN EXIT END (* IF *)
          END (* LOOP *)
        END (* BEGIN *)
      END (* IF *)
    END (* IF *);
    RETURN FALSE
  END MergeIsForbidden;

PROCEDURE Status(eq: Equality): AF.TruthVal =
  VAR e := eq.e.root; f := eq.f.root; BEGIN
    IF e = f THEN
      RETURN AF.TruthVal.TrueAsserted
    ELSIF MergeIsForbidden(e, f) THEN
      RETURN AF.TruthVal.FalseAsserted
    ELSE
      RETURN AF.TruthVal.Unknown
    END (* IF *)
  END Status;

PROCEDURE GEStatus(a, b: T): AF.TruthVal =
  BEGIN
    a := a.root; b := b.root;
    IF a = b THEN RETURN AF.TruthVal.TrueAsserted END;
    TYPECASE a OF
    | Leaf (la) =>
        TYPECASE la.sym OF
        | REF INTEGER (ria) =>
            TYPECASE b OF
            | Leaf (lb) =>
                TYPECASE lb.sym OF
                | REF INTEGER (rib) =>
                    IF ria^ >= rib^ THEN
                      RETURN AF.TruthVal.TrueAsserted
                    ELSE
                      RETURN AF.TruthVal.FalseAsserted
                    END;
                ELSE (* Skip *)
                END (* TYPECASE *)
            ELSE (* Skip *)
            END (* TYPECASE *)
        ELSE (* Skip *)
        END (* TYPECASE *)
    ELSE (* Skip *)
    END (* TYPECASE *);
    RETURN AF.TruthVal.Unknown
  END GEStatus;

PROCEDURE MapOverTruePreds(psym: Atom.T; m: Mapper; data: REFANY): BOOLEAN =
  VAR lf: REFANY; BEGIN
    IF symTab.get(psym, lf) THEN
      TYPECASE lf OF
      | FSym(fs) =>
          VAR p := fs.parent; BEGIN
            IF p # NIL THEN
	      REPEAT
		IF p.root = eTrue.root AND
		  NOT m(p, data) THEN
		  RETURN FALSE
		END (* IF *);
		p := p.same[TRUE]
	      UNTIL p = fs.parent
            END (* IF *);
            RETURN TRUE
          END (* BEGIN *)
      ELSE
          RETURN TRUE
      END (* TYPECASE *)
    ELSE
      RETURN TRUE
    END (* IF *)
  END MapOverTruePreds;

PROCEDURE Args(p: T; VAR (*OUT*) res: ARRAY OF T;
               VAR (*OUT*) resRes: RefList.T) =
  VAR par: Parent := p; e: T; i := 0; BEGIN
    FOR j := 0 TO LAST(res) DO res[j] := NIL END (* FOR *);
    resRes := NIL;
    <*ASSERT ISTYPE(par.car, FSym) *>
    e := par.cdr;
    WHILE e # enil DO
      par := e;
      IF i < NUMBER(res) THEN
        res[i] := par.car
      ELSE
        resRes := RefList.Cons(par.car, resRes)
      END (* IF *);
      e := par.cdr; INC(i)
    END (* WHILE *);
    resRes := RefList.ReverseD(resRes)
  END Args;

PROCEDURE FingerP(e: T): FPrint.T =
  BEGIN RETURN e.root.minHt.fp END FingerP;

PROCEDURE FPGT(fp1, fp2: FPrint.T): BOOLEAN =
  BEGIN RETURN 1 = FPrint.Compare(fp1, fp2)
  END FPGT;

(* Requires that "x" and "y" are roots.  Returns "TRUE" iff propagated
   clauses to represent the distinction of predicate-containing clauses
   "x" and "y". *)
PROCEDURE DoPredDefs(x, y: T; litSense, rm: BOOLEAN): BOOLEAN 
    RAISES { Prover.Error } =
  VAR z: T; sense: BOOLEAN; BEGIN
    <*ASSERT x = x.root AND y = y.root AND x # y *>
    IF x = eTrue.root THEN
      z := y; sense := litSense
    ELSIF y = eTrue.root THEN
      z := x; sense := litSense
    ELSIF Prop.DiffFromTrue IN x.props AND
      NOT Prop.DiffFromTrue IN y.props AND litSense THEN
      z := y; sense := FALSE
    ELSIF Prop.DiffFromTrue IN y.props AND
      NOT Prop.DiffFromTrue IN x.props AND litSense THEN
      z := x; sense := FALSE
    ELSIF NOT litSense THEN
      RETURN DoPredDistinctions(x, y, rm)
    ELSE
      RETURN FALSE
    END (* IF *);
    IF Prop.HasPredTerm IN z.props THEN
      DoPredDefsWork(z, sense, rm)
    END (* IF *);
    IF Prop.HasSelPredMapTerm IN z.props THEN
      DoSelPredMapDefsWork(z, sense, rm)
    END (* IF *);
    IF sense AND z.misc # NIL AND z.misc.forbids # NIL THEN
      VAR zp := z.misc.forbids; BEGIN
        REPEAT
          IF Prop.HasPredTerm IN zp.t.root.props THEN
            DoPredDefsWork(zp.t.root, FALSE, rm)
          END (* IF *);
          IF Prop.HasSelPredMapTerm IN zp.t.root.props THEN
            DoSelPredMapDefsWork(zp.t.root, FALSE, rm)
          END (* IF *);
          zp := zp.next
        UNTIL zp = z.misc.forbids
      END (* BEGIN *)
    END (* IF *);
    RETURN FALSE
  END DoPredDefs;

VAR predInstTrace := Env.Get("PROVER_PRED_INST_TRACE") # NIL;

(* "z" is a node whose equality with "eTrue" is being either
   asserted or denied, depending (respectively) on "sense".
   Instantiate any necessary predicate definitions.  The
   instantiations will be rightmost iff "rm" is true.
*)
PROCEDURE DoPredDefsWork(z: T; sense, rm: BOOLEAN) RAISES { Prover.Error } =
  VAR zp := z; def: PredDefs.T; BEGIN
    REPEAT
      TYPECASE zp OF
      | Parent(p) =>
          IF p.cgPtr = p THEN
	    TYPECASE p.car OF
	    | FSym(fs) =>
		IF PredDefs.GetPredDef(fs.sym, def) THEN
                  <*ASSERT def # NIL*>
		  VAR s := MatchingRule.ConcSubst{NIL, ..}; BEGIN
		    MkSubst(p.cdr, def.args, s);
                    INC(stats.nPredInsts);
                    VAR fp := FingerprintMatch(-fs.id, s);
                        b := matchFPTable.put(fp, NIL);
                    BEGIN
                      IF predInstTrace AND NOT ContextPrivate.inD1P THEN
			IO.Put("Pred Instance: " & Atom.ToText(fs.sym) &
			  "(" & Fmt.Int(s[0].id));
			VAR i := 1; BEGIN
			  WHILE i < NUMBER(s) AND s[i] # NIL DO
			    IO.Put(", " & Fmt.Int(s[i].id)); INC(i)
			  END (* WHILE *);
			  IO.Put(")");
                          IF NOT sense THEN IO.Put(" (neg)") END (* IF *);
                          IF rm THEN IO.Put(" (rm)") END (* IF *)
			END (* BEGIN *);
			IF (b AND NOT rm) THEN
                          IO.Put(" (redundant)")
                        END (* IF *);
			IO.Put("\n")
                      END (* IF *);
                      IF b AND NOT rm THEN
                        INC(stats.nRedPredInsts)
                      ELSE
                        IF NOT b THEN
                          undoFP.addhi(fp); 
                          UndoStackPush(UndoType.AddMatchFP)
                        END (* IF *);
                        <*FATAL PredSx.Error*>
                        VAR plit := Clause.CNF(
                                        PredSx.SkolemizeOuter(
                                            PredDefs.TmplInSense(def, sense),
                                            Context.rs),
                                        s);
                        BEGIN
                          plit.rightMost := rm;
                          ContextPrivate.Propagate(plit)
                        END (* BEGIN *)
                      END (* IF *)
                    END (* BEGIN *)
		  END (* BEGIN *)
		END (* IF *)
	    ELSE
	    END (* TYPECASE *)
          END (* IF *)
      ELSE
      END (* TYPECASE *);
      zp := zp.next
    UNTIL zp = z
  END DoPredDefsWork;

PROCEDURE DoSelPredMapDefsWork(z: T; sense, rm: BOOLEAN)
    RAISES { Prover.Error } =
  VAR zp := z; BEGIN
    REPEAT
      TYPECASE zp OF
      | Parent(p) =>
          IF p.cgPtr = p THEN
	    TYPECASE p.car OF <*NOWARN*>
	    | FSym(fs) =>
		IF fs.sym = PredSx.selectSym THEN
		  VAR cdr: CdrParent := p.cdr; BEGIN
                    (* Since "p" was a congruence class root whose car
                       was an "FSym", and thus a singleton class,
                       and "CdrParents" are equivalent only if they
                       are congruent, then "cdr" is the only node we
                       need to look at. *)
                    VAR indPar: Parent := cdr.cdr;
                        ind: T := indPar.car.root;
                        car := cdr.car.root;
                    BEGIN
                      IF Prop.HasPredMapTerm IN car.props THEN
                        REPEAT
                          TYPECASE car OF
                          | CarParent(carP) =>
                              IF carP.cgPtr = carP THEN
                                DoPredMapDefs(car, ind, sense, rm)
                              END (* IF *)
                          ELSE
                          END (* TYPECASE *);
                          car := car.next
                        UNTIL car = cdr.car.root
                      END (* IF *)
                    END (* BEGIN *)
		  END (* BEGIN *)
		END (* IF *)
	    END (* TYPECASE *)
          END (* IF *)
      ELSE
      END (* TYPECASE *);
      zp := zp.next
    UNTIL zp = z
  END DoSelPredMapDefsWork;

VAR predMapDebug := Env.Get("PROVER_PREDMAP_DEBUG") # NIL;

PROCEDURE DoPredMapDefs(x: CarParent; ind: T; sense, rm: BOOLEAN)
    RAISES { Prover.Error } =
  VAR def: PredDefs.T; BEGIN
    TYPECASE x.car OF <*NOWARN*>
    | FSym(fs) =>
        IF PredDefs.GetPredMapDef(fs.sym, def) THEN
          <*ASSERT def # NIL *>
	  VAR s := MatchingRule.ConcSubst{NIL, ..}; BEGIN
	    MkSubst(x.cdr, def.args, s);
	    s[def.args] := ind;
            INC(stats.nPredMapInsts);
            VAR fp := FingerprintMatch(-fs.id, s); 
                b := matchFPTable.put(fp, NIL);
            BEGIN
              IF predInstTrace AND NOT ContextPrivate.inD1P THEN
		IO.Put("Pred Instance: " & Atom.ToText(fs.sym) &
		  "(" & Fmt.Int(s[0].id));
		FOR i := 1 TO def.args-1 DO
		  IO.Put(", " & Fmt.Int(s[i].id))
		END (* FOR *);
		IO.Put(")[" & Fmt.Int(s[def.args].id) & "]");
                IF NOT sense THEN IO.Put(" (neg)") END (* IF *);
                IF rm THEN IO.Put(" (rm)") END (* IF *);
		IF (b AND NOT rm) THEN IO.Put(" (redundant)") END (* IF *);
		IO.Put("\n")
              END (* IF *);
              IF b AND NOT rm THEN
                INC(stats.nRedPredMapInsts)
              ELSE
                IF NOT b THEN
                  undoFP.addhi(fp); 
                  UndoStackPush(UndoType.AddMatchFP)
                END (* IF *);
                <*FATAL PredSx.Error*>
		VAR plit := Clause.CNF(
                                PredSx.SkolemizeOuter(
                                    PredDefs.TmplInSense(def, sense),
                                    Context.rs),
                                s);
                BEGIN
		  IF predMapDebug THEN
		    Wr.PutText(Stdio.stdout,
			       "***Instantiating sel(predmap) " &
			       Fmt.Int(x.root.id) & "[" &
			       Fmt.Int(ind.root.id) & "] ");
		    IF sense THEN
		      Wr.PutText(Stdio.stdout, "T")
		    ELSE
		      Wr.PutText(Stdio.stdout, "F")
		    END (* IF *);
		    IF rm THEN
		      Wr.PutText(Stdio.stdout, ",R")
		    END (* IF *);
		    Wr.PutText(Stdio.stdout, ":\n");
		    Sx.Print(Stdio.stdout, DbgToSx(x));
		    Wr.PutText(Stdio.stdout, "[");
		    Sx.Print(Stdio.stdout, DbgToSx(ind));
		    Wr.PutText(Stdio.stdout, "]\n");
		    Wr.Flush(Stdio.stdout)
		  END (* IF *);
		  plit.rightMost := rm;
                  (* EXP
		  ContextPrivate.PropagateProxy(plit)
                  *)
		  ContextPrivate.Propagate(plit)
		END (* BEGIN *)
              END (* IF *)
            END (* BEGIN *)
          END (* BEGIN *)
        END (* IF *)
    END (* TYPECASE *)
  END DoPredMapDefs;

VAR doPredDistincts := Env.Get("PROVER_NO_PRED_DISTS") = NIL;
    predDistLbls := Env.Get("PROVER_PRED_DIST_LBLS") # NIL;

PROCEDURE DoPredDistinctions(x, y: T; rm: BOOLEAN): BOOLEAN =
  BEGIN
    IF NOT doPredDistincts THEN RETURN FALSE END (* IF *);

    IF (Prop.HasPredTerm IN x.props OR Prop.HasSelPredMapTerm IN x.props)
       AND
       (Prop.HasPredTerm IN y.props OR Prop.HasSelPredMapTerm IN y.props)
     THEN
      VAR xl := NewEq(x, eTrue); yl := NewEq(y, eTrue);
          nxl := AF.Not(xl); nyl := AF.Not(yl);
          xOry, notxOrNoty: AF.Lit;
      BEGIN
        (* xOry.rightMost := rm; *)
        IF predDistLbls THEN
          xl.lbls := RefList.List1(NEW(AF.Label, sense := TRUE,
                                       name := LabelName.MkAtom(
                                                   Atom.FromText("PDP1"))));
          yl.lbls := RefList.List1(NEW(AF.Label, sense := TRUE,
                                       name := LabelName.MkAtom(
                                                   Atom.FromText("PDP2"))));
          nxl.lbls := RefList.List1(NEW(AF.Label, sense := FALSE,
                                        name := LabelName.MkAtom(
                                                    Atom.FromText("PDN1"))));
          nyl.lbls := RefList.List1(NEW(AF.Label, sense := FALSE,
                                        name := LabelName.MkAtom(
                                                    Atom.FromText("PDN2"))))
        END (* IF *);
        xOry := ProxyProp.Or(xl, yl);
        notxOrNoty := ProxyProp.Or(nxl, nyl);
        notxOrNoty.rightMost := rm;
        ContextPrivate.Propagate(xOry);
        ContextPrivate.Propagate(notxOrNoty)
      END (* BEGIN *);
      RETURN TRUE
    ELSE
      RETURN FALSE
    END (* IF *)
  END DoPredDistinctions;

PROCEDURE MkSubst(e: T; n: CARDINAL; VAR s: MatchingRule.Substitution) = 
  VAR i := 0; BEGIN
    WHILE e # enil DO
      VAR p: Parent := e; BEGIN
        s[i] := p.car;
        INC(i);
        e := p.cdr
      END (* BEGIN *)
    END (* WHILE *);
    <*ASSERT i = n *>
  END MkSubst;

PROCEDURE MakePredSym(e: T) =
  BEGIN
    IF NOT Prop.IsPredSym IN e.props THEN
      e.props := e.props + IsPredSymProp;
      UndoStackPush(UndoType.IsPredSym, e)
    END (* IF *)
  END MakePredSym;

PROCEDURE IsPredSym(e: T): BOOLEAN =
  BEGIN RETURN Prop.IsPredSym IN e.props
  END IsPredSym;

PROCEDURE IsPredTerm(e: T): BOOLEAN =
  BEGIN RETURN IsPredTermProp * e.root.props # EmptyPropSet
  END IsPredTerm;

TYPE
  CSArr = ARRAY [0..4] OF INTEGER;
  EGStatRec = RECORD
    snapshots,
    numEnodes,
    numParents,
    numClasses,
    congClasses,
    carNodes,
    cdrNodes,
    carClasses,
    cdrClasses,
    termClasses,
    fSyms,
    fSymParents,  (* Sum of FSym's of parentSize. *)
    childLabels,   (* Sum over car classes of number of distinct
                      labels in class, plus pre-interned members.  *)
    parentLabels,  (* Parent labels having this car class as
                      arguments. *)
    descLabels,    (* Sum over car classes of number of distinct
                      labels in class and descendents. *)

    rules,
    totPCPairs, totPPPairs,
    maxPCPairs, maxPPPairs,
    
    spcSize, sppSize
    := 0;
    topEqClassSizes := CSArr{0, ..}
  END (* RECORD *);

VAR egStatRec := EGStatRec{};

PROCEDURE PrintEgraphStats() =
  PROCEDURE DoLine(nm: TEXT; n: INTEGER) =
    BEGIN
      IO.Put("Average number of " & nm & ": " &
      Fmt.Real(FLOAT(n)/denom, style := Fmt.Style.Fix, prec := 1) & ".\n");
    END DoLine;
  VAR denom := FLOAT(egStatRec.snapshots); BEGIN
    IO.Put("The following averages are over " &
      Fmt.Int(egStatRec.snapshots) & " snapshots (interval = " &
      Fmt.Int(snapshotIntrvl) & ").\n");
    DoLine("enodes", egStatRec.numEnodes);
    DoLine("parents", egStatRec.numParents);
    DoLine("leaves", egStatRec.numEnodes - egStatRec.numParents);
    DoLine("equivalences classes", egStatRec.numClasses);
    DoLine("congruence classes", egStatRec.congClasses);
    DoLine("car nodes", egStatRec.carNodes);
    DoLine("cdr nodes", egStatRec.cdrNodes);
    DoLine("car classes", egStatRec.carClasses);
    DoLine("cdr classes", egStatRec.cdrClasses);
    DoLine("term classes", egStatRec.termClasses);
    DoLine("distinct pc pairs in active rules", egStatRec.spcSize);
    DoLine("distinct pp pairs in active rules", egStatRec.sppSize);
    IO.Put("\n");
    DoLine("nodes in class #1:", egStatRec.topEqClassSizes[0]);
    DoLine("nodes in class #2:", egStatRec.topEqClassSizes[1]);
    DoLine("nodes in class #3:", egStatRec.topEqClassSizes[2]);
    DoLine("nodes in class #4:", egStatRec.topEqClassSizes[3]);
    DoLine("nodes in class #5:", egStatRec.topEqClassSizes[4]);

    IO.Put("\nThe following averages are over all term-representing classes.\n");
    denom := FLOAT(egStatRec.termClasses);
    DoLine("distinct child labels", egStatRec.childLabels);
    DoLine("distinct parent labels", egStatRec.parentLabels);
    DoLine("distinct reachable labels", egStatRec.descLabels);


    IO.Put("\nThe following averages are over all (" &
      Fmt.Int(egStatRec.rules DIV egStatRec.snapshots) & " per snapshot) " &
      "matching rules.\n");
    denom := FLOAT(egStatRec.rules);
    DoLine("PC pairs", egStatRec.totPCPairs);
    DoLine("PP pairs", egStatRec.totPPPairs);
    IO.Put("\nThe rule with the most PC pairs has " &
      Fmt.Int(egStatRec.maxPCPairs) & " pairs.\n");
    IO.Put("\nThe rule with the most PP pairs has " &
      Fmt.Int(egStatRec.maxPPPairs) & " pairs.\n");

    IO.Put("\nThe following average is over " &
      Fmt.Int(egStatRec.fSyms) & " FSym nodes.\n");
    denom := FLOAT(egStatRec.fSyms);
    DoLine("parents", egStatRec.fSymParents);

    IO.Put("\n\n");
  END PrintEgraphStats;

PROCEDURE EgraphStatSample() =
  PROCEDURE EgraphStatSampleWork(e: T): BOOLEAN =
    BEGIN
      INC(egStatRec.numEnodes);
      IF ISTYPE(e, Parent) THEN
        INC(egStatRec.numParents);
        IF NARROW(e, Parent).cgPtr = e THEN
          INC(egStatRec.congClasses)
        END (* IF *)
      END (* IF *);
      IF e = e.root THEN
        INC(egStatRec.numClasses);
        VAR i := 0; BEGIN
          WHILE i < NUMBER(top5) DO
            IF e.size > top5[i] THEN
              VAR n := NUMBER(top5)-i-1; BEGIN
                SUBARRAY(top5, i+1, n) := SUBARRAY(top5, i, n);
                top5[i] := e.size;
                EXIT
              END (* BEGIN *) 
            END (* IF *);
            INC(i)
          END (* WHILE *)
        END (* BEGIN *)
      END (* IF *);
      IF e.parent # NIL AND IsCar(e.root) OR ISTYPE(e, CarParent) THEN
        INC(egStatRec.carNodes);
        IF e.root = e THEN 
          INC(egStatRec.carClasses);
          IF NOT ISTYPE(e, FSym) THEN
            INC(egStatRec.termClasses);
            INC(egStatRec.childLabels, ClassLabels(e) + PreInterned(e));
            INC(egStatRec.parentLabels, ParentLabels(e));
            INC(egStatRec.descLabels, DescLabels(e))
          END (* IF *)
        END (* IF *)
      END (* IF *);
      IF e.parent # NIL AND NOT IsCar(e.root) OR ISTYPE(e, CdrParent) THEN
        INC(egStatRec.cdrNodes);
        IF e.root = e THEN INC(egStatRec.cdrClasses) END (* IF *)
      END (* IF *);
      TYPECASE e OF
      | FSym(fsym) =>
          INC(egStatRec.fSyms);
          INC(egStatRec.fSymParents, fsym.parentSize)
      ELSE
      END (* TYPECASE *);
      RETURN TRUE
    END EgraphStatSampleWork;
  VAR top5 := CSArr{0, ..}; BEGIN
    INC(egStatRec.snapshots);
    TRY
      EVAL MapEgraph(EgraphStatSampleWork);
    EXCEPT
      Prover.Timeout =>
        RAISE Prover.Error("Enode.Stats: Unexpected Prover.Timeout")
    END;
    FOR k := 0 TO LAST(egStatRec.topEqClassSizes) DO
      INC(egStatRec.topEqClassSizes[k], top5[k])
    END (* FOR *);
    VAR spc := PairSet.Empty;
        spp := PairSet.Empty;
        u := unitMatchRules;
        nu := nonUnitMatchRules;
    BEGIN
      WHILE u # NIL DO
        VAR mr: MatchingRule.T := u.head; BEGIN
          FOR i := 0 TO LAST(mr.pats^) DO
            INC(egStatRec.rules);
            VAR pcs := PairSet.SparseSize(mr.pats[i].pcPairs);
                pps := PairSet.SparseSize(mr.pats[i].ppPairs);
            BEGIN
              INC(egStatRec.totPCPairs, pcs);
              INC(egStatRec.totPPPairs, pps);
              egStatRec.maxPCPairs := MAX(egStatRec.maxPCPairs, pcs);
              egStatRec.maxPPPairs := MAX(egStatRec.maxPPPairs, pps);
            END (* BEGIN *);
            PairSet.UnionSparseD(spc, mr.pats[i].pcPairs);
            PairSet.UnionSparseD(spp, mr.pats[i].ppPairs)
          END (* FOR *)
        END (* BEGIN *);
        u := u.tail
      END (* WHILE *);
      WHILE nu # NIL DO
        VAR mr: MatchingRule.T := nu.head; BEGIN
          FOR i := 0 TO LAST(mr.pats^) DO
            INC(egStatRec.rules);
            VAR pcs := PairSet.SparseSize(mr.pats[i].pcPairs);
                pps := PairSet.SparseSize(mr.pats[i].ppPairs);
            BEGIN
              INC(egStatRec.totPCPairs, pcs);
              INC(egStatRec.totPPPairs, pps);
              egStatRec.maxPCPairs := MAX(egStatRec.maxPCPairs, pcs);
              egStatRec.maxPPPairs := MAX(egStatRec.maxPPPairs, pps);
            END (* BEGIN *);
            PairSet.UnionSparseD(spc, mr.pats[i].pcPairs);
            PairSet.UnionSparseD(spp, mr.pats[i].ppPairs)
          END (* FOR *)
        END (* BEGIN *);
        nu := nu.tail
      END (* WHILE *);
      INC(egStatRec.spcSize, PairSet.Size(spc));
      INC(egStatRec.sppSize, PairSet.Size(spp))
    END (* BEGIN *)
  END EgraphStatSample;

PROCEDURE ClassLabels(e: T): INTEGER =
  VAR res := 0; f := e; BEGIN
    REPEAT
      TYPECASE f OF
      | CarParent(cp) =>
          IF NOT Prop.Marked IN cp.car.props THEN
            INC(res);
            cp.car.props := cp.car.props + MarkedProp
          END (* IF *)
      ELSE
      END (* TYPECASE *); 
      f := f.next
    UNTIL f = e;
    REPEAT
      TYPECASE f OF
      | CarParent(cp) =>
          cp.car.props := cp.car.props - MarkedProp
      ELSE
      END (* TYPECASE *); 
      f := f.next
    UNTIL f = e;
    RETURN res
  END ClassLabels;

PROCEDURE PreInterned(e: T): INTEGER =
  VAR res := 0; f := e; BEGIN
    REPEAT
      IF Prop.PreInterned IN f.props THEN INC(res) END (* IF *);
      f := f.next
    UNTIL f = e;
    RETURN res
  END PreInterned;

PROCEDURE ParentLabels(e: T): INTEGER =
  VAR is := NEW(IntSetDef.T).init();
  PROCEDURE AddLabel(fsym: FSym) =
    BEGIN EVAL is.insert(fsym.id) END AddLabel;
  BEGIN
    MapPredLabels(AddLabel, e.root, useCar := TRUE);
    RETURN is.size()
  END ParentLabels;
    

(* Requires "e" to be a root. *)
PROCEDURE DescLabels(e: T): INTEGER =
  VAR res := DescLabelsWork(e); BEGIN
    DescLabelsClear(e);
    RETURN res
  END DescLabels;

PROCEDURE DescLabelsWork(e: T): INTEGER =
  VAR res := 0; f := e; BEGIN
    REPEAT
      IF NOT Prop.Marked IN f.props THEN
        f.props := f.props + MarkedProp;
        TYPECASE f OF
        | FSym => INC(res)
        | Parent(p) =>
            INC(res, DescLabelsWork(p.car));
            INC(res, DescLabelsWork(p.cdr))
        ELSE
        END (* TYPECASE *)
      END (* IF *);
      f := f.next
    UNTIL f = e;
    RETURN res
  END DescLabelsWork;

PROCEDURE DescLabelsClear(e: T) =
  VAR f := e; BEGIN
    REPEAT
      IF Prop.Marked IN f.props THEN
        f.props := f.props - MarkedProp;
        TYPECASE f OF
        | Parent(p) =>
            DescLabelsClear(p.car);
            DescLabelsClear(p.cdr)
        ELSE
        END (* TYPECASE *)
      END (* IF *);
      f := f.next
    UNTIL f = e
  END DescLabelsClear;

PROCEDURE ShortId(e: T): INTEGER = 
  BEGIN
    RETURN e.fp.byte[7]
  END ShortId;

PROCEDURE RuleCensus(wr: Wr.T) =
  VAR rlCells := 0; BEGIN
    INC(rlCells, CountCellsList(unitMatchRules));
    INC(rlCells, CountCellsList(nonUnitMatchRules));
    Wr.PutText(wr,
      Fmt.Int(RefList.Length(unitMatchRules)) & " unit rules, " &
      Fmt.Int(RefList.Length(nonUnitMatchRules)) & " non-unit rules, " &
      "accounting for at least " & Fmt.Int(rlCells) & " cons cells.\n")
  END RuleCensus;

PROCEDURE CountCellsList(rl: RefList.T): INTEGER =
  VAR res := 0; BEGIN
    WHILE rl # NIL DO
      INC(res, Perf.RLCells(NARROW(rl.head, MatchingRule.T).template));
      rl := rl.tail
    END (* WHILE *);
    RETURN res
  END CountCellsList;

PROCEDURE ResetRuleScores() =
  BEGIN
    ResetRuleScoresList(unitMatchRules);
    ResetRuleScoresList(nonUnitMatchRules)
  END ResetRuleScores;

PROCEDURE ResetRuleScoresList(rules: RefList.T) =
  BEGIN
    WHILE rules # NIL DO
      NARROW(rules.head, MatchingRule.T).score := 0.0;
      rules := rules.tail
    END (* WHILE *)
  END ResetRuleScoresList;

VAR nuMatchStatIntrvl: CARDINAL := 0;
    parentLeakDebug: BOOLEAN;
    snapshotIntrvl: INTEGER;
    nPops := 0;
    nuconsTest: BOOLEAN;

BEGIN
  PredSx.Init();
  VAR numsi := Env.Get("PROVER_STAT_INT"); BEGIN
    IF numsi # NIL THEN
      nuMatchStatIntrvl := Scan.Int(numsi) <*NOWARN*>
    END (* IF *)
  END (* BEGIN *);
  VAR numsi := Env.Get("PROVER_SNAPSHOT_INT"); BEGIN
    IF numsi # NIL THEN
      snapshotIntrvl := Scan.Int(numsi) <*NOWARN*>
    ELSE
      snapshotIntrvl := -1
    END (* IF *)
  END (* BEGIN *);
  VAR num := Env.Get("PROVER_MATCH_DEBUG"); BEGIN
    IF num # NIL THEN
      matchDebug := Scan.Int(num) <*NOWARN*>
    END (* IF *)
  END (* BEGIN *);
  parentLeakDebug := Env.Get("PROVER_LEAK_DEBUG") # NIL;
  IF Env.Get("PROVER_NUCONS_TEST") # NIL THEN
    nuconsTest := TRUE;
    Prover.envVars := Prover.envVars & "PROVER_NUCONS_TEST\n"
  END (* IF *);
  IF NOT doPredDistincts THEN
    Prover.envVars := Prover.envVars & "PROVER_NO_PRED_DISTS\n"
  END (* IF *)
  
END Enode.

