(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jun  9 10:55:11 PDT 1995 by heydon                       *)
(*      modified on Thu Dec  8 15:46:40 1994 by gnelson                      *)
(*      modified on Mon Oct 31 18:34:30 PST 1994 by isard                    *)
(*      modified on Tue Jul 21 01:55:06 PDT 1992 by myers                    *)
<* PRAGMA LL *>

MODULE JunoSolve;

IMPORT Egraph, Equiv, RedundantSolve AS NonLinearSolve, RTVal;
IMPORT RefSeq, Word;

EXCEPTION Unsolvable;

TYPE
  Type = { Any, Pair, Num, Text, Null };
  (* There is a flat partial order on types, with "Type.Any" as bottom. *)

VAR (* CONST *)
  PlusOp, TimesOp, SinOp, CosOp, AtanOp, ExpOp, PairOp := Init(NEW(Var));
  zero := RTVal.FromReal(0.0);
  emptyString := RTVal.FromText("");

TYPE
  EC = Private;
  (* The type "EC" represents an equivalence class with an extra field to
     record if the class contains a pair expression. *)

REVEAL
  Private = Egraph.T BRANDED "JunoSolve.Private" OBJECT
    cons: EC;
  OVERRIDES
    union := Merge;
  END;
  Var = Public BRANDED "JunoSolve.Var" OBJECT
    type: Type;
    index: INTEGER;
    marked: BOOLEAN;
    uses: UseList;
    next, availLink: Var;
  END;

TYPE
  UseList = REF RECORD
    c: Constraint;
    mask := 0;
    next, availLink: UseList
  END;

(* A "cons node" is a node whose "car" is "PairOp". It represents a pair
   expression "(x, y)".

   The call "v.union(w)" where "v" is of type "EC" requires that "v.root #
   w.root".

   The following are invariants on valid "EC"'s and "Var"'s:

   P1: If an equivalence class whose root is "rt" contains a cons node, then
       "rt.cons" points to a cons node in "rt"'s class. Conversely, if "rt"'s
       class does not contain a cons node, then "rt.cons = NIL".

   P2: The "type" field of a "Var" records its type; it is only valid for a
       variable that is the root of its equivalence class.

   P3: For any variable "rt" that is the root of its equivalence class,
       "rt.known => rt.type # Type.Any". (The converse is not true, since a
       REAL or TEXT constraint can set the type of an unknown.)

   P4: The "index" field of a "Var" "v" reflects the index in the global array
       "numeric_vars" of the numeric unknown represented by "v". It is "-1" if
       "v" does not correspond to any variable in the array.

   P5: The "uses" and "next" fields of a "Var" are relevant only in
       "NumericSolve()", and they are valid only for "Var"'s that are the
       roots of their equivalence classes. *)

TYPE
  Args = ARRAY [0..2] OF Var;
  ConType = { Equal, Cons, Plus, Times, Atan, Sin, Cos, Exp, Real, Text };

REVEAL
  Constraint = BRANDED "JunoSolve.Constraint" OBJECT
    type: ConType;
    arg: Args;
    hintCnt: CARDINAL;
    mask: CARDINAL;
    nls_con: NonLinearSolve.Constraint;
    unknown: Var;
    nls_con_mask: CARDINAL;
    next: Constraint;
    availLink: Constraint
  END;
  (* These fields are only valid during the procedure "NumericSolve". *)

VAR
  mu := NEW(MUTEX);
  <* LL >= { mu } *>
  unifyList := NEW(RefSeq.T).init(sizeHint := 100);
  numeric_vars := NEW(Vars, 60);
  numeric_vals := NEW(REF ARRAY OF RTVal.Real, 60);
  numeric_con := NEW(REF ARRAY OF NonLinearSolve.Constraint, 30);

(* The "unify" list "(u1, v1, u2, v2, ...)" contains all pairs "(ui, vi)" of
   cons nodes that are equivalent, but have not yet been merged.

   All "Var"'s in "numeric_vars" are roots of their equivalence classes. *)

PROCEDURE New(known := FALSE; val: RTVal.T := NIL): Var =
  BEGIN RETURN NewPair(NIL, NIL, known, val) END New;

VAR
  varAvail, varInUse: Var := NIL;
  conAvail, conInUse: Constraint := NIL;
  useAvail, useInUse: UseList := NIL;

PROCEDURE NewPair(car, cdr: Var; known := FALSE; val: RTVal.T := NIL): Var=
  VAR res: Var; BEGIN
    IF varAvail # NIL
      THEN res := varAvail; varAvail := varAvail.availLink
      ELSE res := NEW(Var)
    END;
    res.availLink := varInUse;
    varInUse := res;
    res.known := known;
    res.val := val;
    res.car := car;
    res.cdr := cdr;
    RETURN Init(res)
  END NewPair;

PROCEDURE NewCon(type: ConType): Constraint =
  VAR res: Constraint; BEGIN
    IF conAvail # NIL
      THEN res := conAvail; conAvail := conAvail.availLink
      ELSE res := NEW(Constraint)
    END;
    res.availLink := conInUse;
    conInUse := res;
    res.type := type;
    RETURN res
  END NewCon;

PROCEDURE NewEqual(x, y: Var): Constraint =
  VAR res := NewCon(ConType.Equal); BEGIN
    res.arg[0] := x; res.arg[1] := y;
    RETURN res
  END NewEqual;

PROCEDURE NewCons(x, y, z: Var): Constraint =
  VAR res := NewCon(ConType.Cons); BEGIN
    res.arg[0] := x; res.arg[1] := y; res.arg[2] := z;
    RETURN res
  END NewCons;

PROCEDURE NewPlus(x, y, z: Var): Constraint =
  VAR res := NewCon(ConType.Plus); BEGIN
    res.arg[0] := x; res.arg[1] := y; res.arg[2] := z;
    RETURN res
  END NewPlus;

PROCEDURE NewTimes(x, y, z: Var): Constraint =
  VAR res := NewCon(ConType.Times); BEGIN
    res.arg[0] := x; res.arg[1] := y; res.arg[2] := z;
    RETURN res
  END NewTimes;

PROCEDURE NewAtan(x, y, z: Var): Constraint =
  VAR res := NewCon(ConType.Atan); BEGIN
    res.arg[0] := x; res.arg[1] := y; res.arg[2] := z;
    RETURN res
  END NewAtan;

PROCEDURE NewSin(x, y: Var): Constraint =
  VAR res := NewCon(ConType.Sin); BEGIN
    res.arg[0] := x; res.arg[1] := y;
    RETURN res
  END NewSin;

PROCEDURE NewCos(x, y: Var): Constraint =
  VAR res := NewCon(ConType.Cos); BEGIN
    res.arg[0] := x; res.arg[1] := y;
    RETURN res
  END NewCos;

PROCEDURE NewExp(x, y: Var): Constraint =
  VAR res := NewCon(ConType.Exp); BEGIN
    res.arg[0] := x; res.arg[1] := y;
    RETURN res
  END NewExp;

PROCEDURE NewReal(x: Var): Constraint =
  VAR res := NewCon(ConType.Real); BEGIN
    res.arg[0] := x;
    RETURN res
  END NewReal;

PROCEDURE NewText(x: Var): Constraint =
  VAR res := NewCon(ConType.Text); BEGIN
    res.arg[0] := x;
    RETURN res
  END NewText;

PROCEDURE NewUse(con: Constraint; next: UseList): UseList =
  VAR res: UseList; BEGIN
    IF useAvail # NIL THEN
      res := useAvail; useAvail := useAvail.availLink;
      res.c := con; res.mask := 0; res.next := next
    ELSE
      res := NEW(UseList, c := con, next := next)
    END;
    res.availLink := useInUse;
    useInUse := res;
    RETURN res
  END NewUse;

PROCEDURE Dispose() =
  BEGIN
    VAR l := varInUse; BEGIN
      IF l # NIL THEN
      	WHILE l.availLink # NIL DO l := l.availLink END;
      	l.availLink := varAvail;
      	varAvail := varInUse;
      	varInUse := NIL
      END
    END;
    VAR l := conInUse; BEGIN
      IF l # NIL THEN
      	WHILE l.availLink # NIL DO l := l.availLink END;
      	l.availLink := conAvail;
      	conAvail := conInUse;
      	conInUse := NIL
      END
    END;
    VAR l := useInUse; BEGIN
      IF l # NIL THEN
      	WHILE l.availLink # NIL DO l := l.availLink END;
      	l.availLink := useAvail;
      	useAvail := useInUse;
      	useInUse := NIL
      END
    END
  END Dispose;

CONST NoIndex = -1;

PROCEDURE Init(v: Var): Var =
(* Modify "v" into a variable "v'" such that "v'.root = v'" and such that
   P1 - P5 hold of "v'". Return "v'". *)
  BEGIN
    EVAL Egraph.T.init(v);
    v.index := NoIndex;
    v.marked := FALSE;
    v.uses := NIL;
    v.next := NIL;
    IF v.known THEN
      TYPECASE v.val OF <* NOWARN *>
      | RTVal.Null => v.type := Type.Null
      | RTVal.Number => v.type := Type.Num
      | RTVal.Text => v.type := Type.Text
      | RTVal.Pair => v.type := Type.Pair
      END
    ELSE
      v.type := Type.Any
    END;
    IF v.car = PairOp
      THEN v.cons := v
      ELSE v.cons := NIL
    END;
    RETURN v
  END Init;

PROCEDURE P(READONLY c: ARRAY OF Constraint): BOOLEAN =
  BEGIN
    LOCK mu DO
      IF NUMBER(numeric_vars^) < 3 * NUMBER(c) THEN
         numeric_vars := NEW(Vars, 3 * NUMBER(c));
      END;
      TRY
        EVAL unifyList.init();
        ConstructECs(c);
        UnifyClose();
        NumericSolve(c);
        ConstructSoln(c);
      EXCEPT
        Equiv.Forbidden, Unsolvable => RETURN FALSE
      END;
      RETURN TRUE
    END
  END P;

PROCEDURE EtpLogConstraint(<*UNUSED*>type: INTEGER) =
(* Types are: 0 = Equal; 1 = CONS; 2 = +; 3 = *; 4 = REAL; 5 = TEXT; 6 = ATAN;
   7 = SIN; 8 = COS; 9 = EXP. *)
  BEGIN END EtpLogConstraint;

PROCEDURE ConstructECs(READONLY c: ARRAY OF Constraint)
    RAISES {Equiv.Forbidden} =
  VAR con: Constraint; BEGIN
    FOR i := FIRST(c) TO LAST(c) DO
      con := c[i];
      CASE con.type OF <* NOWARN *>
      | ConType.Equal =>
          EtpLogConstraint(0);
          WITH r1 = con.arg[0].root, r2 = con.arg[1].root DO
            IF r1 # r2 THEN EVAL r1.union(r2) END
          END
      | ConType.Cons =>
          EtpLogConstraint(1);
          VAR pair := List3(PairOp, con.arg[1], con.arg[2]); BEGIN
            pair.type := Type.Pair;
            EVAL (con.arg[0].root).union(pair)
          END
      | ConType.Plus =>
          EtpLogConstraint(2);
          VAR plus := List3(PlusOp, con.arg[1], con.arg[2]); BEGIN
            SetFuncType(plus, Type.Num);
            EVAL (con.arg[0].root).union(plus)
          END
      | ConType.Times =>
          EtpLogConstraint(3);
          VAR times := List3(TimesOp, con.arg[1], con.arg[2]); BEGIN
            SetFuncType(times, Type.Num);
            EVAL (con.arg[0].root).union(times)
          END
      | ConType.Atan =>
          EtpLogConstraint(6);
          VAR atan := List3(AtanOp, con.arg[1], con.arg[2]); BEGIN
            SetFuncType(atan, Type.Num);
            EVAL (con.arg[0].root).union(atan)
          END
      | ConType.Sin =>
          EtpLogConstraint(7);
          VAR sin := List2(SinOp, con.arg[1]); BEGIN
            SetFuncType(sin, Type.Num);
            EVAL (con.arg[0].root).union(sin)
          END
      | ConType.Cos =>
          EtpLogConstraint(8);
          VAR cos := List2(CosOp, con.arg[1]); BEGIN
            SetFuncType(cos, Type.Num);
            EVAL (con.arg[0].root).union(cos)
          END
      | ConType.Exp =>
          EtpLogConstraint(9);
          VAR exp := List2(ExpOp, con.arg[1]); BEGIN
            SetFuncType(exp, Type.Num);
            EVAL (con.arg[0].root).union(exp)
          END
      | ConType.Real => EtpLogConstraint(4); SetType(con.arg[0], Type.Num)
      | ConType.Text => EtpLogConstraint(5); SetType(con.arg[0], Type.Text)
      END
    END;
  END ConstructECs;

PROCEDURE SetFuncType(v: Var; t: Type) RAISES {Equiv.Forbidden} =
(* Sets the type of "v" to "t". Verifies that the type of each argument in
   "v.cdr" is at most "t" in the flat partial order with "Type.Any" as bottom,
   and promotes the type of each argument with type "Type.Any" to "t".

   Requires "v" to be a root. Raises "Equiv.Forbidden" if some argument in
   "v.cdr" has a type that is not at most "t". *)
  BEGIN
    <* ASSERT v.root = v *>
    v.type := t;
    VAR curr: Var := v.cdr; BEGIN
      WHILE curr # NIL DO
        SetType(curr.car, t);
        curr := curr.cdr
      END
    END
  END SetFuncType;

PROCEDURE SetType(v: Var; t: Type) RAISES {Equiv.Forbidden} =
(* Sets the type of the class containing "v" to "t", or raises
   "Equiv.Forbidden" if the current type of "v"'s class is different from "t"
   and greater than bottom. *)
  VAR rt: Var := v.root; BEGIN
    IF rt.type = Type.Any THEN
      rt.type := t
    ELSIF rt.type # t THEN
      RAISE Equiv.Forbidden
    END;
  END SetType;

PROCEDURE List3(x, y, z: Egraph.T): Var =
  BEGIN
    RETURN NewPair(car := x, cdr := NewPair(car := y,
      cdr := NewPair(car := z, cdr := NIL)))
  END List3;

PROCEDURE List2(x, y: Egraph.T): Var =
  BEGIN
    RETURN NewPair(car := x, cdr := NewPair(car := y, cdr := NIL))
  END List2;

PROCEDURE Merge(x: EC; y: Equiv.T): Equiv.T RAISES {Equiv.Forbidden} =
(* By the precondition on "Equiv.T.union()", "x" and "y" must be the roots of
   their equivalence classes. They will also be of type "JunoSolve.Var".
   Let "rx" and "ry" be the results of narrowing "x" and "y", respectively, to
   the type "JunoSolve.Var". Let "root" be the root of their resulting combined
   equivalence class. Then "Merge" either raises the exception or establishes
   the following post-conditions:

   Q1: "root.type" = the meet of "rx.type" and "ry.type" in the flat partial
       order. If the meet is undefined, raise "Equiv.Forbidden".

   Q2: "root.known = rx.known OR ry.known". "root.val = rx.val" if "rx.known",
       and "root.val = ry.val" if "ry.known". If both are known, but their
       values are different, raise "Equiv.Forbidden". If "NOT root.known",
       then "root.val" is "rx.val" or "ry.val" for whichever of those two
       values is non-NIL, or either if both are non-NIL.

   Q3: "root.cons" is selected from "rx.cons" and "ry.cons" so that it is
       "NIL" only if both of them are. If both are non-NIL, then the values
       "rx.cons" and "ry.cons" are added to the global unify list.

   Q4: If "root.cons # NIL" and "root.val" is a pair, then the car and cdr of
       "root.val" have been propagated to the arguments of the cons. If
       "root.known" and "root.cons # NIL", raise "Equiv.Forbidden" if the
       known value is not a pair. *)
  <* LL >= { mu } *>
  VAR
    rx: Var := x; ry: Var := y;
    propagateKnowns := rx.known # ry.known AND
                       (rx.cons = NIL) = rx.known AND
                       (ry.cons = NIL) = ry.known;
    propagateHints  := (rx.val # NIL) # (ry.val # NIL) AND
                       (rx.cons = NIL) = (rx.val # NIL) AND
                       (ry.cons = NIL) = (ry.val # NIL);
    (* Work is necessary to establish Q4 iff "propagateKnowns OR
       propagateHints". *)
  BEGIN
    <* ASSERT rx # ry *>
    VAR root: Var := Egraph.T.union(rx, ry); nonroot: Var; BEGIN
      IF root = rx THEN nonroot := ry ELSE nonroot := rx END;
      (* Establish Q1 *)
      IF root.type = Type.Any THEN
        root.type := nonroot.type
      ELSIF nonroot.type # Type.Any AND root.type # nonroot.type THEN
        RAISE Equiv.Forbidden
      END;
      (* Establish Q2 *)
      IF nonroot.known THEN
        IF NOT root.known THEN
          root.known := TRUE; root.val := nonroot.val
        ELSIF NOT RTVal.Equal(rx.val, ry.val) THEN
          RAISE Equiv.Forbidden
        END;
      ELSIF NOT root.known THEN
        IF nonroot.val # NIL THEN root.val := nonroot.val END
      END;
      (* Establish Q3 *)
      IF nonroot.cons # NIL THEN
        IF root.cons = NIL THEN
          root.cons := nonroot.cons;
        ELSE
          unifyList.addhi(rx.cons);
          unifyList.addhi(ry.cons)
        END
      END;
      (* Establish Q4 *)
      IF propagateHints OR propagateKnowns THEN
        TYPECASE root.val OF
        | RTVal.Pair (p) =>
            Fix(root.cons.cdr.car,     root.known, p.car);
            Fix(root.cons.cdr.cdr.car, root.known, p.cdr)
        ELSE
            IF root.known THEN RAISE Equiv.Forbidden END
        END;
      END;
      RETURN root
    END;
  END Merge;

PROCEDURE Fix(v: Var; known: BOOLEAN; val: RTVal.T) RAISES {Equiv.Forbidden} =
  VAR t := New(known, val); BEGIN EVAL (v.root).union(t) END Fix;

PROCEDURE UnifyClose() RAISES {Equiv.Forbidden} =
  BEGIN
    WHILE unifyList.size() # 0 DO
      VAR 
        y: EC := unifyList.remhi();
        x: EC := unifyList.remhi();
      BEGIN
        <* ASSERT x.car = PairOp AND y.car = PairOp *>
        x := x.cdr; y := y.cdr;
        WHILE x # NIL DO
          VAR xa := x.car.root; ya := y.car.root; BEGIN
            IF xa # ya THEN EVAL xa.union(ya) END
          END;
          x := x.cdr; y := y.cdr
        END;
        <* ASSERT y = NIL *>
      END
    END
  END UnifyClose;

PROCEDURE NumericSolve(READONLY c: ARRAY OF Constraint) RAISES { Unsolvable } =
(* By a {\em variable}, we mean a "Var" that is the root of its equivalence
   class and appears in a numeric constraint in "c". We say a variable "v" is
   {\em labeled} if "v.index # NoIndex" and "numeric_vars[v.index] = v". We
   say a variable "v" {\em occurs} in a constraint "con" if it is unlabeled
   and "con" contains a "Var" whose root is "v".

   Q1: For each constraint "con", "con.hintCnt" is the number of distinct
       unlabeled variables occurring in "con", and "con.mask" is "2_b2b1b0",
       where "bi" == ``"con.arg[i]" is unlabeled''.

   Q2: There are two linked lists of constraints whose heads are "ghostReady"
       and "trueReady". Constraints on both lists have "hintCnt <= 1", and
       have valid "nls_con" and "unknown" fields set by Functional().
       Functional constraints with "hintCnt" equal to 1 are on the
       "ghostReady" queue, while non-functional constraints with "hintCnt"
       equal to 1 are on the "trueReady" queue. All constraints with "hintCnt"
       equal to 0 appear on one or the other of the two queues.

   Q3: For any variable "v", "v.uses" is a list that contains an entry "ul"
       for each constraint in which "v" occurs. The value "ul.c" is
       the constraint, and "ul.mask" is "2_b2b1b0" where "bi" == "(ul.c.arg[i]
       = v)".

   Q4: The hint for an unlabeled variable is either "NIL" or a numeric value.

   Q5: The variable "var_cnt" is the number of non-constant variables. The
       variable "numeric_cnt" is the number of numeric constraints.

   Q6: Every constant has been labeled by a number between "next_const + 1"
       and "LAST(numeric_vars^)".

   Q7: The variable "hvl" is the list of hinted unlabeled variables (possibly
       containing some variables which have become labeled), and "uvl" is the
       list of unhinted, unlabeled variables (also possible containing some
       variables which have become labeled). 

   Q8: numeric_cnt <= NUMBER(numeric_con).

   Q9: All knowns (constants) are labeled, and all unknowns are unlabeled.

   R1: The variable "lo" is the number of labeled ``true'' variables, each
       of which has been labeled with a number between "0" and "lo - 1".

   R2: The expression "var_cnt - hi" is the number of labeled ghost
       variables, each of which has been labeled by a number between "hi"
       and "var_cnt - 1".

   R3: The variable "c_lo" is the number of ghost constraints; these
       constraints are stored in the first "c_lo" entries of "numeric_con".
       The expression "numeric_cnt - c_hi" is the number of true constraints;
       these constraints are stored in the last entries of "numeric_con".

| DURING VARIABLE PROCESSING:
|
|             numeric_vars[]           __ 
|                ________	      |..|
|               |        |	      |..| = unused
|               |  True  |	      |__|
|               |  Vars  |
|               |________|
|         lo -> |........|
|               |........|    numeric_con[]
|         hi -> |________|    _____________
|               |        |   |             |
|               |  Ghost |   |    Ghost    |
|               |  Vars  |   | Constraints |
|               |________|   |_____________|
|    var_cnt -> |........|   |.............| <- c_lo
|               |........|   |.............|
| next_const -> |________|   |_____________| <- c_hi
|               |        |   |             |        
|               | Consts |   |     True    |        
|               |________|   | Constraints |        
|			     |_____________|
|                            |.............| <- numeric_cnt
|                            |.............|
|			     |_____________|
|
| AFTER VARIABLE PROCESSING:
|
|             numeric_vars[] numeric_vals[]                
|                ________       ________                   
|               |        |     |        |                  
|               |  True  |     |  True  |                  
|               |  Vars  |     |  Vals  |                  
|               |        |     |        |    numeric_con[] 
|         hi -> |________|     |________|    _____________ 
|         lo -> |        |     |        |   |             |
|               |        |     |        |   |    Ghost    |
|               |        |     |        |   | Constraints |
|               |  Ghost |     |  Ghost |   |             |
|               |  Vars  |     |  Vals  |   |             |
|               |________|     |________|   |_____________| <- c_hi
|    var_cnt -> |........|     |........|   |             | <- c_lo
|               |........|     |........|   |             |
| next_const -> |________|     |________|   |     True    |
|               |        |     |        |   | Constraints |
|               | Consts |     | Consts |   |_____________|
|               |________|     |________|   |.............| <- numeric_cnt
|       		                    |.............|
|                                           |_____________|
*)
  VAR
    lo, hi, c_lo, c_hi: CARDINAL;
    var_cnt: CARDINAL := 0;		 (* total # of variables *)
    numeric_cnt: CARDINAL := 0;		 (* total # of numeric constraints *)
    next_const := LAST(numeric_vars^);	 (* index of next known value *)
    ghostReady, trueReady: Constraint := NIL;
    hvl, uvl: Var := NIL;

  PROCEDURE NumericArgCnt(c: Constraint): CARDINAL =
  (* Return the number of variables in constraint "c" if it is a numeric
     constraint; 0 otherwise. *)
    BEGIN
      CASE c.type OF
	ConType.Plus, ConType.Times, ConType.Atan => RETURN 3
      | ConType.Sin, ConType.Cos, ConType.Exp => RETURN 2
      ELSE RETURN 0
      END
    END NumericArgCnt;

  PROCEDURE AddConToQueue(con: Constraint) =
    BEGIN
      IF Functional(con)
        THEN con.next := ghostReady; ghostReady := con
        ELSE con.next := trueReady; trueReady := con
      END
    END AddConToQueue;

  PROCEDURE UpdateUses(l: UseList) =
    BEGIN
      WHILE l # NIL DO
        DEC(l.c.mask, l.mask);
        DEC(l.c.hintCnt);
        IF l.c.hintCnt = 1 THEN AddConToQueue(l.c) END;
        l := l.next
      END
    END UpdateUses;

  (* NumericSolve *)
  BEGIN
    FOR i := FIRST(c) TO LAST(c) DO
      VAR con := c[i]; cnt := NumericArgCnt(con); BEGIN
        IF cnt > 0 THEN
          INC(numeric_cnt);
          con.hintCnt := 0;
          con.mask := 0;
          FOR j := 0 TO cnt - 1 DO
            VAR v: Var := con.arg[j].root; BEGIN
              IF NOT v.known THEN	 (* "v" is an unknown *)
                IF v.uses = NIL THEN	 (* first time seeing "v" *)
                  INC(var_cnt);
                  (* disregard any non-numeric hint *)
                  IF v.val # NIL AND NOT ISTYPE(v.val, RTVal.Number) THEN
                    v.val := NIL
                  END;
                  IF v.val = NIL
                    THEN v.next := uvl; uvl := v (* unhinted *)
                    ELSE v.next := hvl; hvl := v (*  hinted  *)
                  END
                END;
                (* add "con" to "v.uses" if necessary *)
                IF v.uses = NIL OR v.uses.c # con THEN
                  v.uses := NewUse(con, next := v.uses);
                  INC(con.hintCnt)
                END;
                (* update the variable and constraint masks *)
                VAR bit := Word.LeftShift(1, j); BEGIN
                  v.uses.mask := Word.Or(v.uses.mask, bit);
                  con.mask := Word.Or(con.mask, bit)
                END
              ELSIF v.index = NoIndex THEN
                numeric_vars[next_const] := v;
                v.index := next_const;
                DEC(next_const)
              END
            END
          END; (* FOR *)
          (* Add to "ghostReady" or "trueReady" queue if necessary *)
          IF con.hintCnt <= 1 THEN AddConToQueue(con) END
        END
      END
    END;
    (* Q1 - Q7, Q9 *)
    IF numeric_cnt = 0 THEN RETURN END;

    IF numeric_cnt > NUMBER(numeric_con^) THEN
      numeric_con := NEW(REF ARRAY OF NonLinearSolve.Constraint,
        MAX(numeric_cnt, 2 * NUMBER(numeric_con^)))
    END;
    (* Q1 - Q8, Q9 *)

    lo := 0;
    hi := var_cnt;
    c_lo := 0;
    c_hi := numeric_cnt;
    LOOP
      (* Q1 - Q8, R1 - R3 *)
      IF ghostReady # NIL THEN
       	VAR con := ghostReady; BEGIN
          <* ASSERT con.hintCnt <= 1 *>
          ghostReady := ghostReady.next;
          IF con.hintCnt = 0 THEN
            (* add "con" to the "trueReady" queue *)
            con.next := trueReady;
            trueReady := con
          ELSE
            (* label "con.unknown" as a ghost variable *)
            DEC(hi);
            numeric_vars[hi] := con.unknown;
            con.unknown.index := hi;
            UpdateUses(con.unknown.uses);
            (* finish constructing "con.nls_con" *)
            con.nls_con.arg[0] := hi;
            (* make "con.nls_con" a ghost constraint *)
            numeric_con[c_lo] := con.nls_con;
            INC(c_lo)
          END
        END
      ELSIF hvl # NIL THEN
        IF hvl.index = NoIndex THEN
          (* make "hvl" a true variable *)
          numeric_vars[lo] := hvl;
          hvl.index := lo;
          INC(lo);
          UpdateUses(hvl.uses)
        END;
        hvl := hvl.next
      ELSIF trueReady # NIL THEN
        VAR con := trueReady; BEGIN
          <* ASSERT con.hintCnt <= 1 *>
          trueReady := trueReady.next;
          IF con.hintCnt = 1 THEN
            (* make "con.unknown" a true variable *)
            numeric_vars[lo] := con.unknown;
            con.unknown.index := lo;
            INC(lo);
            UpdateUses(con.unknown.uses)
          END;
          (* finish constructing "con.nls_con" from "con.nls_con_mask" *)
          VAR i := 0; BEGIN
            WHILE con.nls_con_mask > 0 DO
              IF Word.And(con.nls_con_mask, 2_1) = 2_1 THEN
                con.nls_con.arg[i] := con.unknown.index
              END;
              INC(i);
              con.nls_con_mask := Word.RightShift(con.nls_con_mask, 1)
            END
          END;
          (* make "con.nls_con" a true constraint *)
          DEC(c_hi);
          numeric_con[c_hi] := con.nls_con
        END
      ELSIF uvl # NIL THEN
        IF uvl.index = NoIndex THEN
          (* make "uvl" a true variable *)
          numeric_vars[lo] := uvl;
          uvl.index := lo;
          INC(lo);
          UpdateUses(uvl.uses)
        END;
        uvl := uvl.next
      ELSE EXIT
      END
    END;

    (* Solve *)
    InitVals(numeric_vals, numeric_vars, lo, next_const);
    WITH constraints = SUBARRAY(numeric_con^, 0, numeric_cnt) DO
      IF NOT NonLinearSolve.P(lo, var_cnt, numeric_vals^, constraints)
        THEN RAISE Unsolvable
      END
    END;
    NonLinearSolve.Dispose()
  END NumericSolve;

PROCEDURE InitVals(VAR numeric_vals: REF ARRAY OF RTVal.Real;
                   numeric_vars: Vars;
                   lo, next_const: INTEGER) =
  (* Fill in "numeric_vals" from "numeric_vars" *)
  BEGIN
    IF NUMBER(numeric_vals^) < NUMBER(numeric_vars^) THEN
      numeric_vals := NEW(REF ARRAY OF RTVal.Real, NUMBER(numeric_vars^))
    END;
    (* fill in ``true'' variable values *)
    FOR i := 0 TO lo - 1 DO
      TYPECASE numeric_vars[i].val OF
      | RTVal.Null => numeric_vals[i] := 0.0
      | RTVal.Number (r) => numeric_vals[i] := r.val
      ELSE numeric_vals[i] := 0.0
      END
    END;
    (* fill in constant (known) values *)
    FOR i := next_const + 1 TO LAST(numeric_vars^) DO
      TYPECASE numeric_vars[i].val OF <* NOWARN *>
      | RTVal.Number (r) => numeric_vals[i] := r.val
      END
    END
  END InitVals;

CONST NoArg = FIRST(INTEGER);

PROCEDURE Functional(con: Constraint): BOOLEAN =
(* Requires "con.hintCnt <= 1", i.e., that there is at most one unknown in the
   constraint.

   If "con.mask" # 0 and it indicates that "con" is functional in the single
   unknown variable, then set "con.unknown" to that unknown variable, set
   "con.nls_con" to a constraint that solves it, and return TRUE.

   Otherwise, set "con.nls_con" to a "NonLinearSolve.Constraint" clone of
   "con", set "con.unknown" to NIL, and return FALSE.

   In either case, the arguments in "con.nls_con" corresponding to the unknown
   are unset. *)
  CONST LoBit = ARRAY [1..7] OF [0..2]{0, 1, 0, 2, 0, 1, 0}; BEGIN
    <* ASSERT con.hintCnt <= 1 *>
    IF con.mask = 0
      THEN con.unknown := NIL
      ELSE con.unknown := con.arg[LoBit[con.mask]].root
    END;
    CASE con.type OF <* NOWARN *>
      ConType.Plus =>
        CASE con.mask OF <* NOWARN *>
          2_001 =>
            con.nls_con := SetArgs(NonLinearSolve.NewPlus(), con, 1, 2);
            con.nls_con_mask := 2_001
        | 2_010 =>
            con.nls_con := SetArgs(NonLinearSolve.NewMinus(), con, 0, 2);
            con.nls_con_mask := 2_001
        | 2_100 =>
            con.nls_con := SetArgs(NonLinearSolve.NewMinus(), con, 0, 1);
            con.nls_con_mask := 2_001
        | 2_110 =>
            con.nls_con := SetArgs(NonLinearSolve.NewHalve(), con, 0);
            con.nls_con_mask := 2_001
        | 2_000, 2_101, 2_011, 2_111 =>
            con.nls_con := SetArgs(NonLinearSolve.NewPlus(), con, 1, 2, 0);
            con.nls_con_mask := con.mask;
            RETURN FALSE
        END
    | ConType.Times =>
        IF con.mask = 2_001 THEN
          con.nls_con := SetArgs(NonLinearSolve.NewTimes(), con, 1, 2);
          con.nls_con_mask := 2_001
        ELSE
          con.nls_con := SetArgs(NonLinearSolve.NewTimes(), con, 1, 2, 0);
          con.nls_con_mask := con.mask;
          RETURN FALSE
        END
    | ConType.Atan =>
        CASE con.mask OF <* NOWARN *>
        | 2_001 =>
            con.nls_con := SetArgs(NonLinearSolve.NewAtan(), con, 1, 2);
            con.nls_con_mask := 2_001
        | 2_010 =>
            con.nls_con := SetArgs(NonLinearSolve.NewMultTan(), con, 2, 0);
            con.nls_con_mask := 2_001
        | 2_000, 2_100, 2_011, 2_101, 2_110, 2_111 =>
            con.nls_con := SetArgs(NonLinearSolve.NewAtan(), con, 1, 2, 0);
            con.nls_con_mask := con.mask;
            RETURN FALSE
        END
    | ConType.Sin =>
        IF con.mask = 2_01 THEN
          con.nls_con := SetArgs(NonLinearSolve.NewSin(), con, 1);
          con.nls_con_mask := 2_01
        ELSE
          con.nls_con := SetArgs(NonLinearSolve.NewSin(), con, 1, NoArg, 0);
          con.nls_con_mask := con.mask;
          RETURN FALSE
        END
    | ConType.Cos =>
        IF con.mask = 2_01 THEN
          con.nls_con := SetArgs(NonLinearSolve.NewCos(), con, 1);
          con.nls_con_mask := 2_01
        ELSE
          con.nls_con := SetArgs(NonLinearSolve.NewCos(), con, 1, NoArg, 0);
          con.nls_con_mask := con.mask;
          RETURN FALSE
        END
    | ConType.Exp =>
        IF con.mask = 2_01 THEN
          con.nls_con := SetArgs(NonLinearSolve.NewExp(), con, 1);
          con.nls_con_mask := 2_01
        ELSE
          con.nls_con := SetArgs(NonLinearSolve.NewExp(), con, 1, NoArg, 0);
          con.nls_con_mask := con.mask;
          RETURN FALSE
        END
    END;
    RETURN TRUE
  END Functional;

PROCEDURE SetArgs(
    nlc: NonLinearSolve.Constraint;
    con: Constraint;
    a1: CARDINAL;
    a2, a0: INTEGER := NoArg)
  : NonLinearSolve.Constraint =
  BEGIN
    nlc.arg[1] := NARROW(con.arg[a1].root, Var).index;
    IF a2 # NoArg THEN nlc.arg[2] := NARROW(con.arg[a2].root, Var).index END;
    IF a0 # NoArg THEN nlc.arg[0] := NARROW(con.arg[a0].root, Var).index END;
    RETURN nlc
  END SetArgs;

PROCEDURE ConstructSoln(READONLY c: ARRAY OF Constraint) RAISES {Unsolvable} =
(* Set the "val" field of each unknown in "c" to the value of its root. *)
  BEGIN
    FOR i := FIRST(c) TO LAST(c) DO
      VAR argCnt: CARDINAL; BEGIN
        CASE c[i].type OF <* NOWARN *>
          ConType.Cons, ConType.Plus, ConType.Times, ConType.Atan =>
            argCnt := 3
        | ConType.Sin, ConType.Cos, ConType.Exp, ConType.Equal =>
            argCnt := 2
        | ConType.Real, ConType.Text =>
            argCnt := 1
        END;
        FOR j := 0 TO argCnt - 1 DO
          VAR arg := c[i].arg[j]; BEGIN
            IF NOT arg.known THEN arg.val := Value(arg.root) END
          END
        END
      END
    END
  END ConstructSoln;

PROCEDURE Value(v: Var): RTVal.T RAISES {Unsolvable} =
(* Return the value of "v", which must be the root of its equivalence class. *)
  BEGIN
    <* ASSERT v = v.root *>
    IF NOT v.known THEN
      IF v.cons # NIL THEN
        IF v.marked THEN
          RAISE Unsolvable
        ELSE
          v.marked := TRUE;
          v.val := RTVal.FromPair(
            car := Value(v.cons.cdr.car.root),
            cdr := Value(v.cons.cdr.cdr.car.root))
        END
      ELSIF v.index # NoIndex THEN
        v.val := RTVal.FromReal(numeric_vals[v.index])
      ELSE
        (* "v" is an unknown var that is not equivalent to a cons nor involved
	   in any numeric constraints. It may not have a type, or it may have
           a non-trivial type due to a REAL or TEXT constraint. Assign "v" a
           valid value for its type if it does not already have one. *)
        CASE v.type OF <* NOWARN *>
	| Type.Any =>
            IF v.val = NIL THEN v.val := RTVal.nil END
        | Type.Num =>
            IF v.val = NIL OR NOT ISTYPE(v.val, RTVal.Number) THEN
              v.val := zero
            END
        | Type.Text =>
            IF v.val = NIL OR NOT ISTYPE(v.val, RTVal.Text) THEN
              v.val := emptyString
            END
        END
      END;
      v.known := TRUE;
    END;
    RETURN v.val
  END Value;

BEGIN
END JunoSolve.
