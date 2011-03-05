(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun  4 12:15:34 PDT 2002 by saxe                     *)
(*      modified on Fri Nov  1 16:09:49 PST 1996 by detlefs                  *)

MODULE Simplex;

IMPORT Rat, AF, Context, Enode, PredSx, Signature, SigTab, ContextPrivate,
       Prover;
IMPORT Word, Atom, Fmt, Text, TextWr, Env, FPrint;
IMPORT RefList, RefSeq, RefRefTbl, IntIntTbl, IntRefTbl;
IMPORT FileWr, OSError; (* for simplex loop logging *)

(* For debugging *)
IMPORT Scan, Stdio, Wr, Sx, Thread;
VAR debug := 0;
VAR inPop := FALSE; (* For debugging *)
<*FATAL Wr.Failure, Thread.Alerted, Sx.PrintError*>

<*PRAGMA SPEC*>

REVEAL
  Unknown = BRANDED OBJECT
    uid: CARDINAL;
    var: Enode.T;
    sx: REFANY := NIL;  (* For debugging. *)
    primitive := TRUE;
  END (* OBJECT *);

TYPE
  LitUnknown = Unknown BRANDED OBJECT
    val: Rat.T;
   METHODS
    init(val: Rat.T; var: Enode.T): LitUnknown := InitLitUnknown
  END (* OBJECT *);
  VarUnknown = Unknown BRANDED OBJECT
    restricted := FALSE;
    isZero := FALSE;
    sense := TRUE;
    ownsRow: BOOLEAN;
    index: CARDINAL;
    slack := FALSE;
    identity := FALSE;
   METHODS
    init(ownsRow: BOOLEAN; var: Enode.T): VarUnknown := InitVarUnknown
  END (* RECORD *);

<*SPEC VAR UValid: MAP Unknown TO BOOLEAN *>
<*SPEC VAR USubValid: MAP Unknown TO BOOLEAN *>
<*SPEC DEPENDS UValid[u: Unknown] ON USubValid[u] *>
<*SPEC REP UValid[u: Unknown] IFF
               u # NIL AND (ISTYPE(u, LitUnknown) OR ISTYPE(u, VarUnknown))
           AND USubValid[u] *>

<*SPEC VAR VUValid: MAP VarUnknown TO BOOLEAN *>
<*SPEC VAR LUValid: MAP LitUnknown TO BOOLEAN *>
<*SPEC DEPENDS USubValid[lu: LitUnknown] ON LUValid[lu] *>
<*SPEC DEPENDS USubValid[vu: VarUnknown] ON VUValid[vu] *>
<*SPEC REP USubValid[lu: LitUnknown] IFF LUValid[lu] *>
<*SPEC REP USubValid[vu: VarUnknown] IFF VUValid[vu] *>

<*SPEC DEPENDS LUValid[lu: LitUnknown] ON lu.val, lu.var *>
<*SPEC REP LUValid[lu: LitUnknown] IFF TRUE *>

<*SPEC DEPENDS VUValid[vu: VarUnknown] ON
               vu.uid, vu.restricted, vu.isZero, vu.sense, vu.ownsRow,
               vu.index, vu.slack, vu.identity, n, m *>
<*SPEC REP VUValid[vu: VarUnknown] IFF
              vu # NIL
          AND (   vu.ownsRow AND vu.index < n
               OR NOT vu.ownsRow AND 1 <= vu.index AND vu.index < m) *>

<*SPEC PRED InUseP(u: VarUnknown) IS
       u.ownsRow AND y^[u.index] = u OR NOT u.ownsRow AND x^[u.index] = u *>

<*SPEC DEPENDS Valid ON a, m, n, dcol, x, y, UnknownArr, VUValid,
                        VarUnknown.index, VarUnknown.ownsRow *>
<*SPEC REP Valid IFF
                 { v1: a # NIL AND x # NIL AND y # NIL AND x # y } 
             AND { v2: 0 <= n AND n <= NUMBER(a^) }
             AND { v3: 0 <= m AND m <= NUMBER(x^) }
             AND { v4: 1 <= dcol AND dcol <= m }

             AND { v5: NUMBER(a^) > 0 }
             AND { v6: NUMBER(a^, 1) > 0 }
             AND { v7: NUMBER(y^) = NUMBER(a^) }
             AND { v8: NUMBER(x^) = NUMBER(a^, 1) }

             AND { v9: (ALL [i9, j9: INTEGER]
                         0 <= i9 AND i9 < n AND 0 <= j9 AND j9 < n IMPLIES
                             i9 = j9 OR y^[i9] # y^[j9]) }
             AND { v10: (ALL [i10, j10: INTEGER]
                         1 <= i10 AND i10 < m AND 1 <= j10 AND j10 < m IMPLIES
                             i10 = j10 OR x^[i10] # x^[j10]) }
             AND { v11: (ALL [i11, j11: INTEGER]
                         0 <= i11 AND i11 < n AND 1 <= j11 AND j11 < m IMPLIES
                             y^[i11] # x^[j11]) }

             AND { v12: (ALL [i12: INTEGER] 0 <= i12 AND i12 < n IMPLIES
                             VUValid[y^[i12]] AND y^[i12].ownsRow
                             AND y^[i12].index = i12) }
             AND { v13: (ALL [j13: INTEGER] 1 <= j13 AND j13 < m IMPLIES
                             VUValid[x^[j13]] AND NOT x^[j13].ownsRow
                             AND x^[j13].index = j13) }
*>

<*SPEC VAR UndoStk: SEQ[UndoRec] *>
<*SPEC DEPENDS UndoStk ON UndoStack, undoStack, undoSP *>
<*SPEC REP UndoStk = SUBARRAY(UndoStack[undoStack], 0, undoSP) *>

<*SPEC LitUnknown.init(self, val, var)
       MODIFIES UValid[self], UndoStk, Enode.SimplexMisc.unknown, uidCntr,
                VIRGIN[var], Enode.aMisc[var], Enode.aMisc'[var].unknown
       REQUIRES Valid AND var # NIL
            AND (Enode.aMisc[Enode.aRoot[var]] # NIL IMPLIES
                  ISTYPE(Enode.aMisc[Enode.aRoot[var]].unknown, VarUnknown))
       ENSURES RES = self
*>       

<*SPEC VarUnknown.init(self, ownsRow, var)
       MODIFIES UValid[self], UndoStk, uidCntr, a, RatArr2, x, y, UnknownArr,
                n, m, self.var, VIRGIN[var]
       REQUIRES Valid
            AND (ALL [i: INTEGER] 0 <= i AND i < n IMPLIES self # y^[i])
            AND (ALL [j: INTEGER] 1 <= j AND j < m IMPLIES self # x^[j])
       ENSURES { p1: UValid'[self] } AND { p2: RES = self }
           AND { p3: (ALL [i: INTEGER] 0 <= i AND i < n IMPLIES
                         y^[i] = y'^'[i]) }
           AND { p4: (ALL [j: INTEGER] 1 <= j AND j < m IMPLIES
                         x'^'[j] = x^[j]) }
           AND { p5: ownsRow IMPLIES
                         m' = m AND n' = n + 1 AND y'^'[n] = self }
           AND { p6: NOT ownsRow IMPLIES
                         n' = n AND m' = m + 1 AND x'^'[m] = self }
           AND { p7: self.var' = var }
*>       

(* An "Unknown" represents a rational value.  An "unknown" "u" may
   represent the value of an enode, in which case "u.var" is that
   enode.  An unknown "u" is an atomic simplicial variable if
   "u.primitive" is true; otherwise it is a composite term, defined as the
   the sum, difference, product, or quotient of more basic simplicial
   variables.

   A "LitUnknown" "lu" represents the fixed value "lu.val"; it is always
   associated with an enode.  The call "NEW(LitUnknown).init(q, e)"
   allocates a new "LitUnknown", initializes its value to "q", and
   sets its "var" to the root of "e"s equivalence class.

   A "VarUnknown" represents a value that may vary.  If a "VarUnknown"
   has a "TRUE" "restricted" field, then it is restricted to be zero if
   "isZero is "TRUE", or else is restricted to be at least zero.  If
   the "sense" field is "FALSE", the unknown has been negated.  If
   "slack" is "FALSE", "var" is non-"NIL" and refers to an enode.
   If "slack" is "TRUE", this unknown is a slack representing a
   constraint on the independent variables.  If "identity" is "TRUE",
   the unknown is a slack whose constraint represents an arithmetic
   identity or an identity represented in the egraph.

   The call "NEW(VarUnknown).init(b, v)" allocates a new "VarUnknown",
   initializes it with a new "id", adds it to "y" if "ownsRow" is
   "TRUE", else "y", extends "x", "y", and "a" as necessary, zeros the
   newly created row or column, sets the "index" field to the index of
   the unknown in "x" or "y", sets the "var" field to "var", the
   "primitive" field to "TRUE", the "slack" field to "FALSE", the
   "restricted" field to "FALSE", and the "identity" field to "FALSE".
*)

VAR
  uidCntr: CARDINAL;

(* Distinct unknowns have distinct "id" fields, and "idCntr" is the
   number of unknowns that have been created since the last time
   "Init" was called.
*)

TYPE RatArr2 = BRANDED REF ARRAY OF ARRAY OF Rat.T;
     UnknownArr = BRANDED REF ARRAY OF VarUnknown;

VAR a: RatArr2;
    m, n, dcol: INTEGER;
    x, y: UnknownArr;
    zero: LitUnknown;

(* "Simplex.C" is the conjunction of the constraint that each restricted
   unknown be non-negative, together with a constraint for each "i" in
   the range "[0..n)" that

| y[i] = (+ j: 1 <= j < m : x[j] * a[i,j]) + a[i,0]

   A restricted unknown is an element of "x" or "y" whose "restricted"
   field is set.  The value of "y[i]" is relevant only for "i" in the
   range "[0..n)"; the value of "x[j]" is relevant only for "j" in the
   range "[1..m)".

   Finally, it also represents the constraint that each "x[i]" for "i"
   in the range "[1..dcol)" is zero.

   We maintain the invariants that "NUMBER(y^) = NUMBER(a^)", and
   "NUMBER(x^) = NUMBER(a[0])".

   The unknown "zero" is identically equal to zero.
*)

<*SPEC InitUnknown(self)
       MODIFIES uidCntr, self.uid
       REQUIRES Valid AND self # NIL *>
PROCEDURE InitUnknown(self: Unknown): Unknown =
  BEGIN
    self.uid := uidCntr; INC(uidCntr); 
    RETURN self
  END InitUnknown;

<*SPEC InitVarUnknown(self, ownsRow, var)
       MODIFIES UValid[self], UndoStk, uidCntr, a, RatArr2, x, y, UnknownArr,
                n, m, self.var, VIRGIN[var]
       REQUIRES Valid AND self # NIL
            AND (ALL [i: INTEGER] 0 <= i AND i < n IMPLIES self # y^[i])
            AND (ALL [j: INTEGER] 1 <= j AND j < m IMPLIES self # x^[j])
       ENSURES { p1: UValid'[self] } AND { p2: RES = self }
           AND { p3: (ALL [i: INTEGER] 0 <= i AND i < n IMPLIES
                         y^[i] = y'^'[i]) }
           AND { p4: (ALL [j: INTEGER] 1 <= j AND j < m IMPLIES
                         x'^'[j] = x^[j]) }
           AND { p5: ownsRow IMPLIES
                         m' = m AND n' = n + 1 AND y'^'[n] = self }
           AND { p6: NOT ownsRow IMPLIES
                         n' = n AND m' = m + 1 AND x'^'[m] = self }
           AND { p7: self.var' = var }
*>       
PROCEDURE InitVarUnknown(self: VarUnknown;
                         ownsRow: BOOLEAN; var: Enode.T): VarUnknown =
  BEGIN
    EVAL InitUnknown(self);
    self.var := var;
    self.ownsRow := ownsRow;
    IF ownsRow THEN
      EnsureNewRow();
      self.index := n;
      y[n] := self;
      FOR j := 0 TO m-1 DO a[n,j] := Rat.Zero END (* FOR *);
      INC(n); IncNStats()
    ELSE
      EnsureNewColumn();
      self.index := m;
      x[m] := self;
      FOR i := 0 TO n-1 DO a[i,m] := Rat.Zero END (* FOR *);
      INC(m); IncMStats()
    END (* IF *);
    UndoStackPush(UndoType.CreateVarUnknown, u := self);
    RETURN self
  END InitVarUnknown;

<*SPEC InitLitUnknown(self, val, var)
       MODIFIES UValid[self], UndoStk, Enode.SimplexMisc.unknown, uidCntr,
                VIRGIN[var], Enode.aMisc[var], Enode.aMisc'[var].unknown
       REQUIRES Valid AND self # NIL AND var # NIL
            AND (Enode.aMisc[Enode.aRoot[var]] # NIL IMPLIES
                  ISTYPE(Enode.aMisc[Enode.aRoot[var]].unknown, VarUnknown))
       ENSURES RES = self
*>
PROCEDURE InitLitUnknown(self: LitUnknown;
                         val: Rat.T; var: Enode.T): LitUnknown =
  BEGIN
    EVAL InitUnknown(self);
    self.val := val;
    <*ASSERT var # NIL*>
    self.var := Enode.Root(var);
    VAR m := self.var.getMisc(FALSE);
        u: Unknown := NIL;
    BEGIN
      IF m # NIL THEN u := m.unknown END (* IF *);
      UndoStackPush(UndoType.CreateLitUnknown, u := u, u2 := self);
    END (* BEGIN *);
    self.var.getMisc().unknown := self;
    RETURN self
  END InitLitUnknown;

<*SPEC EnsureNewColumn()
       MODIFIES x, a
       REQUIRES Valid
       ENSURES { p6: NUMBER(x'^') > m }
           AND { p8: (ALL [u: UnknownArr] u # x' IMPLIES u^' = u^) }
           AND { p10: (ALL [j: INTEGER]
                          1 <= j AND j < m IMPLIES x'^'[j] = x^[j]) }
           AND { p11: x' = x  OR FRESH(x') }
*>
PROCEDURE EnsureNewColumn() =
  (* Ensure that "x[i]" and "a[i, m]" for "i < n" are valid references. *)
  BEGIN
    IF NUMBER(x^) = m THEN
      VAR new := NEW(UnknownArr, 2 * NUMBER(x^)); BEGIN
        SUBARRAY(new^, 0, NUMBER(x^)) := x^;
        x := new
      END (* BEGIN *);
      VAR new := NEW(RatArr2, NUMBER(a^), 2 * NUMBER(a[0])); BEGIN
        FOR i := 0 TO LAST(a^) DO 
          SUBARRAY(new[i], 0, NUMBER(a[0])) := a[i]
        END (* FOR *);
        a := new
      END (* BEGIN *)
    END (* IF *)
  END EnsureNewColumn;

<*SPEC EnsureNewRow()
       MODIFIES y, a
       REQUIRES Valid
       ENSURES { p6: NUMBER(y'^') > n }
           AND { p8: (ALL [u: UnknownArr] u # y' IMPLIES u^' = u^) }
           AND { p9: (ALL [i: INTEGER]
                          0 <= i AND i < n IMPLIES y'^'[i] = y^[i]) }
           AND { p11: y' = y OR FRESH(y') }
*>
PROCEDURE EnsureNewRow() =
  (* Ensure that "y[n]" and "a[n, j]" for "j < m" are valid references. *)
  BEGIN
    IF NUMBER(y^) = n THEN
      VAR new := NEW(UnknownArr, 2 * NUMBER(y^)); BEGIN
        SUBARRAY(new^, 0, NUMBER(y^)) := y^;
        y := new
      END (* BEGIN *);
      VAR new := NEW(RatArr2, 2 * NUMBER(a^), NUMBER(a[0])); BEGIN
        SUBARRAY(new^, 0, NUMBER(a^)) := a^;
        a := new
      END (* BEGIN *)
    END (* IF *)
  END EnsureNewRow;

<*SPEC GetUnknown(e, activate)
       MODIFIES UndoStk, uidCntr, Enode.aMisc[e]
       REQUIRES e # NIL AND Valid
       ENSURES Enode.aMisc'[e] # NIL
           AND Enode.aMisc'[e].unknown' # NIL
           AND RES = Enode.aMisc'[e].unknown'
*>
PROCEDURE GetUnknown(e: Enode.T; activate := FALSE): Unknown =
  BEGIN
    IF activate THEN Enode.Activate(e) END (* IF *);
    IF e.getMisc().unknown = NIL THEN
      e.getMisc().unknown := NEW(VarUnknown).init(ownsRow := FALSE, var := e)
    END (* IF *);
    RETURN e.getMisc(FALSE).unknown
  END GetUnknown;

(* Returns a new unknown with "NIL" "var" and "slack" "TRUE", owning a
   new, empty row.  Makes an undo stack entry. *)
<*SPEC NewSlack()
       MODIFIES UndoStk, uidCntr
       REQUIRES Valid
       ENSURES FRESH(RES)
*>
PROCEDURE NewSlack(): VarUnknown =
  BEGIN RETURN NEW(VarUnknown, slack := TRUE).init(ownsRow := TRUE, var := NIL)
  END NewSlack;

<*SPEC Add(r, u, q)
       MODIFIES RatArr2[a]
       REQUIRES Valid AND UValid[r] AND r.ownsRow AND UValid[u] *>
PROCEDURE Add(r: VarUnknown; u: Unknown; q: Rat.T) =
  (* Add "q * u" to the row owned by "r". *)
  VAR i := r.index;
  BEGIN
    <*ASSERT r.ownsRow *>
    <*ASSERT InUse(u) *>
    TYPECASE u OF <*NOWARN*>
    | LitUnknown(ru) =>
        a[i, 0] := Rat.Plus(a[i, 0], Rat.Times(q, ru.val))
    | VarUnknown(u) =>
        IF u.ownsRow THEN
          FOR j := 0 TO m-1 DO
            a[i,j] := Rat.Plus(a[i,j], Rat.Times(q, a[u.index,j]))
          END (* FOR *)
        ELSE
          a[i,u.index] := Rat.Plus(a[i,u.index], q)
        END (* IF *)
    END (* TYPECASE *)
  END Add;

<*SPEC FindPivot(row, i, j)
       MODIFIES i, j
       REQUIRES Valid AND row < n
       ENSURES RES IMPLIES i' < n AND dcol <= j' AND j' < m
*>
PROCEDURE FindPivot (            row: CARDINAL;
                     VAR (*OUT*) i  : CARDINAL;
                     VAR (*OUT*) j  : CARDINAL  ): BOOLEAN =
  (* Returns "TRUE" and ensures that pivoting at "i, j" will increase the
     value of "y[row]", or returns "FALSE" if "y[row]" is manifestly
     maximized.  If it returns "TRUE" and "i = row", then "y[row]" is
     manifestly unbounded. *)
  BEGIN
    j := dcol;
    WHILE j # m AND (a[row, j].num = 0
                     OR (x[j].restricted AND a[row, j].num < 0)) DO
      <*SPEC INV dcol <= j AND j <= m *>
      INC(j)
    END (* WHILE *);
    IF j = m THEN
      RETURN FALSE
    ELSE
      i := FindPivotWork(row, j, a[row, j].num);
      RETURN TRUE
    END
  END FindPivot;

<*SPEC FindPivotWork(i, j, sgn)
       REQUIRES Valid AND i < n AND dcol <= j AND j < m
       ENSURES RES < n *>
PROCEDURE FindPivotWork(i, j: CARDINAL; sgn: INTEGER): CARDINAL =
  (* Requires that "a[i,j] # 0".  Returns a pivot row
     "r" such that pivoting row "r" and column "j" changes the
     sample value of "y[i]" in the direction of "sgn" (0 indicates
     that the value may change in either direction), while
     preserving the feasibility of all rows owned by restricted
     unknowns, except possibly for that of "x[j]".  (If "a[i,j] * sgn"
     is positive, then "x[j]"'s restriction, if any, is preserved.)
     If "r = i", "y[i]" would become manifestly unbounded after a
     pivot at "r, j". *)
  VAR champ := i; scoreNum := 1; scoreDen := 0; BEGIN
    <*ASSERT a[i,j].num # 0*>
    FOR ii := 0 TO n-1 DO
      <*SPEC INV 0 <= champ AND champ < n *>
      IF ii # i AND y[ii].restricted AND
         (sgn = 0 AND a[ii,j].num # 0 OR sgn * a[ii,j].num < 0) THEN
        VAR tNum := a[ii,0].num * a[ii,j].den;
            tDen := a[ii,0].den * ABS(a[ii,j].num);
        BEGIN
          IF tNum * scoreDen < tDen * scoreNum THEN
            scoreNum := tNum;
            scoreDen := tDen;
            champ := ii
          END (* IF *)
        END (* BEGIN *)
      END (* IF *)
    END (* FOR *);
    RETURN champ
  END FindPivotWork;

VAR pivotCount := 0;

<*SPEC Pivot(row, col, undoable)
       MODIFIES a^, undoStack, undoSP, UndoStack,
                x^[col], x^[col].index, x^[col].ownsRow,
                y^[row], y^[row].index, y^[row].ownsRow, stats
       REQUIRES Valid AND row < n AND 1 <= col AND col < m
       ENSURES y^'[row] = x^[col] AND x^'[col] = y^[row]
*>
PROCEDURE Pivot(row, col: CARDINAL; undoable := TRUE) =
  (* Exchange "y[row]" with "x[col]". *)
  BEGIN
    INC(pivotCount);
    IF undoable THEN
      UndoStackPush(UndoType.Pivot, u := y[row], u2 := x[col])
    END (* IF *);
    a[row,col] := Rat.Recip(a[row,col]);
    FOR j := dcol TO m-1 DO
      IF j # col THEN
	a[row,j] := Rat.Times(a[row,j], a[row,col]);
	a[row,j].num := -a[row,j].num;
      END (* IF *)
    END (* FOR *);
    a[row,0] := Rat.Times(a[row,0], a[row,col]);
    a[row,0].num := -a[row,0].num;
    FOR i := 0 TO n-1 DO
      IF i # row AND a[i,col].num # 0 THEN
	FOR j := dcol TO m-1 DO
          <*SPEC INV Valid *>
	  IF j # col AND a[row,j].num # 0 THEN
            INC(stats.pivotInnerRoundsEx);
	    a[i,j] := Rat.Plus(a[i,j], Rat.Times(a[row,j], a[i,col]))
	  END (* IF *)
	END (* FOR *);
        INC(stats.pivotInnerRounds, m-dcol);
	a[i,0] := Rat.Plus(a[i,0], Rat.Times(a[row,0], a[i,col]))
      END (* IF *)
    END (* FOR *);
    FOR i := 0 TO n-1 DO
      IF i # row THEN
        a[i,col] := Rat.Times(a[i,col], a[row,col])
      END (* IF *)
    END (* FOR *);
    VAR t := y[row]; BEGIN y[row] := x[col]; x[col] := t END (* BEGIN *);
    y[row].ownsRow := TRUE;
    y[row].index := row;
    x[col].ownsRow := FALSE;
    x[col].index := col;
    IF debug > 3 THEN
      PrintTableau();
      DbgPrint("After pivot (" & Fmt.Int(row) & ", " & Fmt.Int(col) & ").\n")
    END (* IF *)
  END Pivot;

<*SPEC MakePositive(u, lastPivot)
       MODIFIES a^, x^, y^, undoStack, undoSP, stats
       REQUIRES Valid AND u.ownsRow AND VUValid[u] *>
PROCEDURE MakePositive(u: VarUnknown; lastPivot: BOOLEAN): BOOLEAN =
  (* Requires that "u" owns a row.  Determines the sign of "u"'s
     maximum.  If this is strictly positive returns "TRUE".  Otherwise,
     returns "FALSE", leaving "u" at its maximum value.  If
     "lastPivot" is "FALSE", leaves "u" in a row variable, but may not
     preserve the feasibility of the tableu.  If "lastPivot" is
     "TRUE", preserves the feasibility of the tableu, but may leave
     "u" in a column.
  *)
  BEGIN
    <*ASSERT u.ownsRow*>
    IF debug > 3 THEN DbgPrint("In MakePositive...\n") END (* IF *);
    IF a[u.index,0].num > 0 THEN RETURN TRUE END (* IF *);
    VAR i, j: CARDINAL; BEGIN
      LOOP
        <*SPEC INV Valid *>
        IF NOT FindPivot(u.index, i, j) THEN
          RETURN FALSE
        ELSIF i = u.index OR
              Rat.GT(a[u.index,0], Rat.Div(Rat.Times(a[i,0], a[u.index,j]),
                                           a[i,j])) THEN
          IF lastPivot THEN Pivot(i, j) END (* IF *);
          RETURN TRUE
        ELSE
          Pivot(i, j)
        END (* IF *)
      END (* LOOP *)
    END (* BEGIN *)
  END MakePositive;

<*SPEC MakeOwnRow(u)
       MODIFIES a^, x^, y^, undoStack, undoSP, stats
       REQUIRES Valid AND NOT u.ownsRow AND VUValid[u] *>
PROCEDURE MakeOwnRow(u: VarUnknown) =
  (* "u" owns a column.  If that column is not identically zero, pivots
     so that "u" owns a row, preserving feasibility (except perhaps
     of "u"'s sign constraint.)
  *)
  VAR i := 0; BEGIN
    <*ASSERT NOT u.ownsRow*>
    WHILE i < n AND a[i,u.index].num = 0 DO
      <*SPEC INV 0 <= i AND i <= n *>
      INC(i)
    END (* WHILE *);
    IF i # n THEN
      VAR ii := FindPivotWork(i, u.index, 0); BEGIN
        IF debug > 3 THEN
          DbgPrint("In MakeOwnRow...\n")
        END (* IF *);
        Pivot(ii, u.index)
      END (* BEGIN *)
    END (* IF *)
  END MakeOwnRow;

<*SPEC MakeOwnColumn(u)
       MODIFIES a^, x^, y^, undoStack, undoSP, stats
       REQUIRES Valid AND u.ownsRow AND VUValid[u]
       ENSURES RES IMPLIES NOT u.ownsRow *>
<*UNUSED*> (* !!! *)
PROCEDURE MakeOwnColumn(u: VarUnknown): BOOLEAN =
  (* "u" owns a row.  If it is possible to pivot "u" into a column
     while preserving feasibility, does so and returns "TRUE";
     otherwise, returns "FALSE" and leaves the tableau unmodified.
  *)
  VAR iu := u.index; BEGIN
    <*ASSERT u.ownsRow*>
    FOR j := dcol TO m-1 DO
      IF a[iu, j].num # 0 THEN
	IF NOT x[j].restricted OR (-a[iu, 0].num * a[iu, j].num) >= 0 THEN
	  VAR feas := TRUE; i := 0; BEGIN
	    WHILE i < n AND feas DO
	      IF y[i].restricted AND NOT y[i].isZero THEN
		VAR newSample := a[i, 0]; BEGIN
		  newSample := Rat.Minus(newSample,
					 Rat.Times(Rat.Times(a[i,j], a[iu,0]),
						   Rat.Recip(a[iu,j])));
		  IF newSample.num < 0 THEN feas := FALSE END (* IF *);
		END (* BEGIN *)
	      END (* IF *);
	      INC(i)
	    END (* WHILE *);
	    IF feas THEN Pivot(iu, j); RETURN TRUE END (* IF *)
	  END (* BEGIN *)
	END (* IF *)
      END (* IF *)
    END (* FOR *);
    RETURN FALSE
  END MakeOwnColumn;

(* Requires that "u" owns a row; if there is any non-zero non-constant
   entry in "u"'s row, sets "col" to the owner of that column, pivots
   "u" with the column owner, and returns TRUE; otherwise, returns FALSE. *)
PROCEDURE MakeOwnColumnNonFeasible(u: VarUnknown; VAR (*OUT*) col: VarUnknown): BOOLEAN =
  VAR iu := u.index; BEGIN
    <*ASSERT u.ownsRow*>
    FOR j := dcol TO m-1 DO
      IF a[iu, j].num # 0 THEN
        col := x[j]; Pivot(iu, j); RETURN TRUE
      END (* IF *)
    END (* FOR *);
    RETURN FALSE
  END MakeOwnColumnNonFeasible;

PROCEDURE NegRow0(u: VarUnknown) =
  BEGIN
    FOR j := 0 TO m-1 DO a[u.index,j].num := -a[u.index,j].num END (* FOR *);
    UndoStackPush(UndoType.NegRow0, u := u)
  END NegRow0;

PROCEDURE RowToDeadCol(u: VarUnknown) =
  (* Require that "u" own a row whose sample value can be made positive
     and negative.  Pivot "u" into a column and kill that column.
  *)
  BEGIN
    <*ASSERT u.ownsRow*>
    IF a[u.index,0].num > 0 THEN NegRow0(u) END (* IF *);
    VAR i, j: CARDINAL; b: BOOLEAN; BEGIN
      LOOP
        b := FindPivot(u.index, i, j);
        <*ASSERT b*>
        IF i = u.index OR
          Rat.GE(a[u.index,0], Rat.Div(Rat.Times(a[i,0], a[u.index,j]),
                                       a[i,j])) THEN
          Pivot(u.index, j);
          KillCol(u);
          RETURN
        ELSE
          Pivot(i, j)
        END (* IF *)
      END (* LOOP *)
    END (* BEGIN *)
  END RowToDeadCol;
          
PROCEDURE KillCol(u: VarUnknown) =
  (* Requires that "u" owns a column, which becomes dead.
     Propagates any new equalities in the tableau.
  *)
  BEGIN
    <*ASSERT NOT u.ownsRow AND u.index >= dcol*>
    IF debug > 0 THEN
      DbgPrint("Simplex.KillCol: u.index = " & Fmt.Int(u.index) & "\n");
      PrintTableau()
    END;
    KillColWork(u);
    PropagateEqualities(dcol-1)
  END KillCol;

PROCEDURE KillColWork(u: VarUnknown) =
  (* Requires that "u" owns a live column, which is killed. *)
  BEGIN
    IF u.index # dcol THEN
      FOR i := 0 TO n-1 DO
        VAR t := a[i,u.index]; BEGIN
          a[i,u.index] := a[i,dcol]; a[i,dcol] := t
        END (* BEGIN *)
      END (* FOR *);
      VAR i := u.index; BEGIN
        x[i] := x[dcol]; x[dcol] := u;
        x[i].index := i; x[dcol].index := dcol
      END (* BEGIN *)
    END (* IF *);
    IF u.restricted THEN
      <*ASSERT NOT u.isZero*>
      UndoStackPush(UndoType.RestrictFromNN, u := x[dcol])
    ELSE
      UndoStackPush(UndoType.RestrictFromFree, u := x[dcol])
    END (* IF *);
    IF u.slack THEN
      u.restricted := TRUE; u.isZero := TRUE
    ELSE
      PropEq(u, zero)
    END (* IF *);
    UndoStackPush(UndoType.DeadCol, u := x[dcol]);
    INC(dcol)
  END KillColWork;

PROCEDURE KillRow(u: VarUnknown) =
  (* Requires that "u" owns a row which is manifestly maximized at 0.
     Kills all of the columns with non-zero entries in "u"'s row.
     Propagates any new equalities in the tableau.
  *)
  VAR oldDcol := dcol; ut: UndoType; BEGIN
    <*ASSERT u.ownsRow AND u.slack AND NOT (u.restricted AND u.isZero)*>
    IF debug > 0 THEN
      DbgPrint("Simplex.KillRow: u.index = " & Fmt.Int(u.index) & "\n");
      PrintTableau()
    END;
    IF u.restricted THEN ut := UndoType.RestrictFromNN
    ELSE ut := UndoType.RestrictFromFree
    END (* IF *);
    MaximizeNonRedundant(u);
    FOR j := dcol TO m-1 DO
      IF a[u.index,j].num # 0 THEN KillColWork(x[j]) END (* IF *)
    END (* FOR *);
    UndoStackPush(ut, u := u);
    u.restricted := TRUE; u.isZero := TRUE;
    PropagateEqualities(oldDcol)
  END KillRow;

TYPE
  RowTbl = IntIntTbl.Default OBJECT
   OVERRIDES
    keyEqual := RowKeyEqual;
    keyHash := RowKeyHash
  END (* OBJECT *);

PROCEDURE RowKeyEqual(<*UNUSED*> tbl: RowTbl;
                      READONLY i1, i2: INTEGER): BOOLEAN =
  VAR res := (a[i1,0] = a[i2,0]); j := dcol; BEGIN
    WHILE res AND j # m DO
      res := (a[i1,j] = a[i2,j]);
      INC(j)
    END (* WHILE *);
    RETURN res
  END RowKeyEqual;

PROCEDURE RowKeyHash(<*UNUSED*> tbl: RowTbl;
                     READONLY i: INTEGER): Word.T =
  VAR res: Word.T := 0; BEGIN
    FOR j := dcol TO m-1 DO
      res := Word.Plus(res, Word.Rotate(a[i,j].num, j));
      res := Word.Plus(res, Word.Rotate(a[i,j].den, j))
    END (* FOR *);
    res := Word.Plus(res, Word.Rotate(a[i,0].num, 0));
    res := Word.Plus(res, Word.Rotate(a[i,0].den, 0));
    RETURN res
  END RowKeyHash;

(* Requires that "u" owns a row and is maximized at 0.
   Ensures that no column owner is redundant, and that "u" is manifestly
   maximized at 0. *)
PROCEDURE MaximizeNonRedundant(u: VarUnknown) =
  BEGIN
    LOOP
      RemoveRedundantColumnOwners();
      IF u.ownsRow AND IsManifestlyMaximized(u) THEN
        EXIT
      ELSE
        IF NOT u.ownsRow THEN MakeOwnRow(u) END (* IF *);
        <*ASSERT u.ownsRow *>
        VAR b := MakePositive(u, FALSE); BEGIN
          <*ASSERT NOT b AND a[u.index, 0].num = 0*>
        END (* BEGIN *)
      END (* IF *);
    END (* LOOP *)
  END MaximizeNonRedundant;

(* Returns "TRUE" iff the row owner "u" is manifestly maximized at 0. *)
PROCEDURE IsManifestlyMaximized(u: VarUnknown): BOOLEAN =
  BEGIN
    IF a[u.index,0].num # 0 THEN RETURN FALSE END (* IF *);
    VAR j := dcol; BEGIN
      WHILE j < m AND
        (a[u.index, j].num = 0 OR
         a[u.index, j].num < 0 AND x[j].restricted) DO
        INC(j)
      END (* WHILE *);
      RETURN j = m
    END (* BEGIN *)
  END IsManifestlyMaximized;

(* Ensures that no non-empty column owner is redundant. *)
PROCEDURE RemoveRedundantColumnOwners() =
  VAR j := dcol; BEGIN
    WHILE j < m DO
      VAR u := x[j]; BEGIN
        IF u.slack AND u.restricted AND IsRedundant(u) THEN
          <*ASSERT NOT u.ownsRow*>
          MakeOwnRow(u);
          IF u.ownsRow THEN
            DeleteRow(u.index, undoable := TRUE)
          ELSE
            (* Ignore empty column. *)
          END (* IF *)
        ELSE
          INC(j)
        END (* IF *)
      END (* BEGIN *)
    END (* IF *);
  END RemoveRedundantColumnOwners;
    
PROCEDURE PropagateEqualities(oldDcol: CARDINAL) =
  (* Columns from "oldDcol" to the current value of "dcol" have been
     killed since the last call to "PropagateEqualities".  Propagate
     any new equalities in the tableu.
  *)
  VAR tbl: RowTbl; BEGIN
    IF oldDcol = dcol THEN RETURN END (* IF *);

    tbl := NEW(RowTbl).init(n);
    FOR i := 0 TO n-1 DO
      VAR j := oldDcol; BEGIN
        WHILE j < dcol AND a[i,j].num = 0 DO INC(j) END (* WHILE *);
        IF j # dcol THEN
          VAR oneCol: INTEGER; BEGIN
            (* If "oneCol > 0", "oneCol" is the index of the unique
	       column found so far with a 1 in it, or if "oneCol = -1",
               all columns found so far have had zeros, otherwise,
               "oneCol = -2" and some column contains a non-zero,
	       non-one value, or more than one column contains a one,
               or the constant column contains a non-zero. *)
            IF a[i,0].num = 0 THEN
              oneCol := -1
            ELSE
              oneCol := -2
            END (* IF *);
            j := dcol;
            WHILE j # m AND oneCol # -2 DO
              IF a[i,j] = Rat.One THEN
                IF oneCol = -1 THEN
                  oneCol := j
                ELSE
                  oneCol := -2
                END (* IF *)
              ELSIF a[i,j].num # 0 THEN
                oneCol := -2
              END (* IF *);
              INC(j)
            END (* WHILE *);
            IF oneCol = -1 THEN
              PropEq(y[i], zero)
            ELSIF oneCol > 0 THEN
              PropEq(y[i], x[oneCol])
            END (* IF *)
          END (* BEGIN *)
        END (* IF *)
      END (* BEGIN *);
      VAR jj: INTEGER; BEGIN
        IF tbl.get(i, jj) THEN
          PropEq(y[i], y[jj])
        ELSIF y[i].var # NIL THEN
          EVAL tbl.put(i, i) 
        END (* IF *)
      END (* BEGIN *)
    END (* FOR *);
    VAR i := 0; BEGIN
      WHILE i < n DO
	VAR e := y[i].var; r := a[i, 0]; zeros := TRUE; j := dcol; BEGIN
	  WHILE zeros AND j < m DO
	    zeros := zeros AND a[i,j].num = 0; INC(j)
	  END (* WHILE *);
	  IF zeros THEN
            IF e # NIL THEN
              IF a[i,0].den # 1 THEN
                ContextPrivate.Propagate(AF.falseLit);
                RETURN
              ELSE
		PropEq(y[i], Enode.FromInt(r.num).getMisc().unknown)
              END
            END;
(* The following snippet of code was replaced by the code above
   in order to fix the constant propagation bug.  (See message to
   src.sparta of 4-Feb-1999.)

            IF a[i,0].den # 1 THEN
              ContextPrivate.Propagate(AF.falseLit);
              RETURN
            END;
	    IF e # NIL THEN
	      IF a[i,0].den = 1 AND Enode.IntHasEnode(r.num) THEN
		PropEq(y[i], Enode.FromInt(r.num).getMisc().unknown)
	      ELSE
		SetUnknownToLit(e, a[i,0])
	      END (* IF *);
	    END (* IF *);
*)
	    DeleteRow(i, undoable := TRUE)
	  ELSE
	    INC(i)
	  END (* IF *)
	END (* BEGIN *)
      END (* WHILE *)
    END (* BEGIN *)
  END PropagateEqualities;

PROCEDURE PropEq(u1, u2: Unknown) =
  (* If "u1" and "u2" are both known to the egraph, then enqueue an
     enode literal representing the their equality for later assertion
     in the egraph.
  *)
  BEGIN
    IF debug > 0 THEN
      DbgPrint("Simplex.PropEQ(" &
               Sx_ToText(UnknownToSx(u1)) &
               "[var is  " & Sx_ToText(Enode.DbgToSx(u1.var)) & "], " &
               Sx_ToText(UnknownToSx(u2)) & 
               "[var is  " & Sx_ToText(Enode.DbgToSx(u2.var)) & "])\n")
    END;
    IF u1.var # NIL AND u2.var # NIL THEN
      ContextPrivate.Propagate(Enode.NewEq(u1.var, u2.var));
      TYPECASE u2 OF
      | LitUnknown(lu) => SetUnknownToLit(u1.var, lu.val)
      ELSE
      END (* TYPECASE *)
    END (* IF *);
  END PropEq;

PROCEDURE SetUnknownToLit(e: Enode.T; q: Rat.T) =
  VAR er := Enode.Root(e); BEGIN
    TYPECASE er.getMisc().unknown OF
    | NULL, VarUnknown =>
        EVAL NEW(LitUnknown).init(q, er)
    ELSE
    END (* TYPECASE *)
  END SetUnknownToLit;
    

PROCEDURE AssertZero(u: VarUnknown): BOOLEAN =
  (* Add to "C" the assertion that "u = 0", returning "TRUE" iff the
     result is satisfiable.
  *)
  BEGIN
    IF u.restricted AND u.isZero THEN RETURN TRUE END (* IF *);
    IF debug # 0 THEN
      DbgPrint("Simplex.AssertZero: Asserting " &
               Sx_ToText(UnknownToSx(u)) & " = 0.\n")
    END (* IF *);
    Context.opsEnabled[Context.Ops.TightenBounds] := TRUE;
    IF NOT u.ownsRow THEN
      MakeOwnRow(u);
      IF NOT u.ownsRow THEN
        KillCol(u); RETURN TRUE
      END (* IF *)
    END (* IF *);
    IF NOT MakePositive(u, FALSE) THEN
      IF a[u.index,0].num < 0 THEN
        RETURN FALSE
      ELSE
        KillRow(u); RETURN TRUE
      END (* IF *)
    END (* IF *);
    NegRow0(u);
    VAR b := MakePositive(u, FALSE); BEGIN
      IF NOT b THEN
        IF a[u.index,0].num < 0 THEN (* Was maximized at a negative value. *)
          RETURN FALSE
        ELSE
          KillRow(u); RETURN TRUE
        END (* IF *)
      ELSE
        RowToDeadCol(u); RETURN TRUE
      END (* IF *)
    END (* BEGIN *)
  END AssertZero;
      
PROCEDURE AssertNonNeg(u: VarUnknown; sense: BOOLEAN): BOOLEAN =
  (* Assert "sense = (u >= 0)".  Return "TRUE" if "C" and this
     assertion is satisfiable. *)
  BEGIN
    IF u.restricted THEN RETURN sense = u.sense END (* IF *);
    Context.opsEnabled[Context.Ops.TightenBounds] := TRUE;
    IF debug # 0 THEN
      DbgPrint("Asserting ");
      VAR rel := " >= "; BEGIN
        IF NOT sense THEN rel := " < " END (* IF *);
        DbgPrint(Sx_ToText(UnknownToSx(u)) & rel & " 0.\n")
      END (* BEGIN *)
    END (* IF *);
    IF debug > 1 THEN PrintTableau() END (* IF *);
    IF NOT u.ownsRow THEN
      MakeOwnRow(u);
      <*ASSERT u.ownsRow*>
    END;
    IF NOT sense THEN
      FOR j := 0 TO m-1 DO a[u.index,j].num := -a[u.index,j].num END (* FOR *);
      a[u.index,0] := Rat.Minus(a[u.index,0], Rat.One);
      u.sense := FALSE;
      UndoStackPush(UndoType.NegRow1, u := u)
    END (* IF *);
    IF MakePositive(u, TRUE) THEN
      <*ASSERT u.slack*>
      u.restricted := TRUE;
      u.isZero := FALSE;
      UndoStackPush(UndoType.RestrictFromFree, u := u);
      RETURN TRUE
    ELSIF a[u.index,0].num = 0 THEN
      KillRow(u); RETURN TRUE
    ELSE
      RETURN FALSE
    END (* IF *)
  END AssertNonNeg;

VAR slkSym := Atom.FromText("SLK");
    
PROCEDURE EnodeIsLit(e: Enode.T; q: Rat.T) =
  BEGIN EVAL NEW(LitUnknown).init(var := e, val := q) END EnodeIsLit;

PROCEDURE IsSum(res, x, y: Enode.T): BOOLEAN =
  VAR ures: Unknown;
      slack: VarUnknown;
      ux := GetUnknown(Enode.Root(x));
      uy := GetUnknown(Enode.Root(y));
  BEGIN
    IF res.getMisc(FALSE) # NIL AND res.getMisc(FALSE).unknown # NIL THEN
      RETURN TRUE
    ELSE
      ures := GetUnknown(Enode.Root(res));
      slack := NewSlack();
      slack.identity := TRUE;
      IF computeSxs THEN
	slack.sx := RefList.List2(slkSym, Enode.DbgToSx(res));
      END (* IF *);
      IF debug # 0 THEN
	DbgPrint(Sx_ToText(UnknownToSx(slack)) & " = " &
	  Sx_ToText(UnknownToSx(ures)) & " - " &
	  Sx_ToText(UnknownToSx(ux)) & " - " &
	  Sx_ToText(UnknownToSx(uy)) & ".\n")
      END (* IF *);
      ures.primitive := FALSE;
      Add(slack, ures, Rat.One);
      Add(slack, ux, Rat.NegOne);
      Add(slack, uy, Rat.NegOne);
      RETURN AssertZero(slack)
    END (* IF *)
  END IsSum;

PROCEDURE IsDiff(res, x, y: Enode.T): BOOLEAN =
  VAR ures: Unknown;
      slack: VarUnknown;
      ux := GetUnknown(Enode.Root(x));
      uy := GetUnknown(Enode.Root(y));
  BEGIN
    IF res.getMisc(FALSE) # NIL AND res.getMisc(FALSE).unknown # NIL THEN
      RETURN TRUE
    ELSE
      ures := GetUnknown(Enode.Root(res));
      slack := NewSlack();
      slack.identity := TRUE;
      IF computeSxs THEN
	slack.sx := RefList.List2(slkSym, Enode.DbgToSx(res))
      END (* IF *);
      IF debug # 0 THEN
	DbgPrint(Sx_ToText(UnknownToSx(slack)) & " = " &
	  Sx_ToText(UnknownToSx(ures)) & " - " &
	  Sx_ToText(UnknownToSx(ux)) & " + " &
	  Sx_ToText(UnknownToSx(uy)) & ".\n")
      END (* IF *);
      ures.primitive := FALSE;
      Add(slack, ures, Rat.One);
      Add(slack, ux, Rat.NegOne);
      Add(slack, uy, Rat.One);
      RETURN AssertZero(slack)
    END (* IF *);
  END IsDiff;

PROCEDURE IsNumeral(u: Unknown; VAR (*OUT*) q: Rat.T): BOOLEAN =
  (* Returns "TRUE" if "u"'s value is a constant, returning that value
     in "q". *)
  BEGIN
    TYPECASE u OF <*NOWARN*>
    | LitUnknown(ru) =>
        q := ru.val; RETURN TRUE
    | VarUnknown(u) =>
        IF u.ownsRow THEN
          FOR j := dcol TO m-1 DO
	    IF a[u.index,j].num # 0 THEN RETURN FALSE END (* IF *)
	  END;
	  q := a[u.index,0];
	  RETURN TRUE
	ELSIF u.index < dcol THEN
	  q := Rat.Zero;
	  RETURN TRUE
	ELSE
	  RETURN FALSE
	END (* IF *)
    END (* TYPECASE *)
  END IsNumeral;

PROCEDURE IsProd(res, x, y: Enode.T): BOOLEAN =
  VAR ures: Unknown := NIL;
      slack: VarUnknown;
      ux := GetUnknown(Enode.Root(x));
      uy := GetUnknown(Enode.Root(y));
      q: Rat.T;
  BEGIN
    IF res.getMisc(FALSE) # NIL AND res.getMisc(FALSE).unknown # NIL THEN
      RETURN TRUE
    ELSE
      ures := GetUnknown(Enode.Root(res));
      IF IsNumeral(ux, q) OR IsNumeral(uy, q) THEN
        slack := NewSlack(); slack.identity := TRUE;
        IF computeSxs THEN
          slack.sx := RefList.List2(slkSym, Enode.DbgToSx(res))
        END (* IF *)
      END (* IF *)
    END (* IF *);
    IF IsNumeral(ux, q) THEN
      IF debug # 0 THEN
        DbgPrint(Sx_ToText(UnknownToSx(slack)) & " = " &
          Sx_ToText(UnknownToSx(ures)) & " - " &
          Sx_ToText(RatToSx(q)) & " * " &
          Sx_ToText(UnknownToSx(uy)) & ".\n")
      END (* IF *);
      Add(slack, ures, Rat.NegOne);
      Add(slack, uy, q);
      RETURN AssertZero(slack)
    ELSIF IsNumeral(uy, q) THEN
      IF debug # 0 THEN
        DbgPrint(Sx_ToText(UnknownToSx(slack)) & " = " &
          Sx_ToText(UnknownToSx(ures)) & " - " &
          Sx_ToText(RatToSx(q)) & " * " &
          Sx_ToText(UnknownToSx(ux)) & ".\n")
      END (* IF *);
      Add(slack, ures, Rat.NegOne);
      Add(slack, ux, q);
      RETURN AssertZero(slack)
    ELSE
      ures.primitive := TRUE;
      RETURN TRUE
    END (* IF *)
  END IsProd;

PROCEDURE IsQuotient(res, x, y: Enode.T): BOOLEAN =
  VAR ures: Unknown;
      slack: VarUnknown;
      ux := GetUnknown(Enode.Root(x));
      uy := GetUnknown(Enode.Root(y));
      qDenom: Rat.T;
  BEGIN
    IF res.getMisc(FALSE) # NIL AND res.getMisc(FALSE).unknown # NIL THEN
      RETURN TRUE
    ELSE
      ures := GetUnknown(Enode.Root(res))
    END (* IF *);
    IF IsNumeral(uy, qDenom) THEN
      slack := NewSlack(); slack.identity := TRUE;
      IF computeSxs THEN
        slack.sx := RefList.List2(slkSym, Enode.DbgToSx(res))
      END (* IF *);
      IF debug # 0 THEN
        DbgPrint(Sx_ToText(UnknownToSx(slack)) & " = " &
          Sx_ToText(UnknownToSx(ures)) & " - (" &
          Sx_ToText(UnknownToSx(ux)) & " / " &
          Sx_ToText(RatToSx(qDenom)) & ").\n")
      END (* IF *);
      Add(slack, ures, Rat.NegOne);
      Add(slack, ux, Rat.Recip(qDenom));
      RETURN AssertZero(slack)
    ELSE
      ures.primitive := TRUE;
      RETURN TRUE
    END (* IF *)
  END IsQuotient;

REVEAL
  Equality = AF.T BRANDED OBJECT
    a, b: Unknown;
    identity: BOOLEAN
   OVERRIDES
    assert := AssertEQ;
    toSx := EqToSx;
    fingerprint := EqFP;
  END (* OBJECT *);
  GEInequality = AF.T BRANDED OBJECT
    a, b: Enode.T;
   OVERRIDES
    assert := AssertGE;
    equalInContext := GEInequalityEqualInContext;
    hashInContext := GEInequalityHashInContext;
    toSx := GEIneqToSx;
    fingerprint := GEIneqFP;
  END (* OBJECT *);

  ZeroUnknownAF = AF.T BRANDED OBJECT
    u: VarUnknown;
   OVERRIDES
    assert := ZeroUnknownAssert;
    toSx := ZeroUnknownToSx;
    fingerprint := ZeroUnknownFP;
  END (* OBJECT *);

VAR
  eqTab: SigTab.T;

TYPE
  GEIneqTab = RefRefTbl.Default BRANDED OBJECT
   OVERRIDES
    keyHash := GEIneqHash;
    keyEqual := GEIneqEqual;
  END (* OBJECT *);
VAR
  geIneqTab: GEIneqTab;

(* An "Equality" "e" represents the assertion that "e.u = 0".
   An "Inequality" "e" represents the assertion that "e.u >= 0".
*)

PROCEDURE NewGT(e1, e2: Enode.T): AF.Lit =
  BEGIN
    RETURN AF.Not(NewGE(e2, e1))
  END NewGT;

PROCEDURE NewGE(e1, e2: Enode.T): AF.Lit =
  BEGIN
    IF e1 = e2 THEN RETURN AF.trueLit END (* IF *);
    VAR af := NEW(GEInequality, a := e1, b := e2).init();
        afRA: REFANY;
    BEGIN
      IF geIneqTab.get(af, afRA) THEN
        af := afRA
      ELSE
        EVAL geIneqTab.put(af, af);
        AFUndoStackPush(AFUndoType.GE, ineq := af)
      END (* IF *);
      RETURN NEW(AF.Lit, af := af, sense := TRUE)
    END (* BEGIN *)
  END NewGE;

PROCEDURE GEStatus(gei: GEInequality): AF.TruthVal =
  BEGIN
    RETURN Enode.GEStatus(gei.a, gei.b)
  END GEStatus;

PROCEDURE NewEQ(u1, u2: Unknown): AF.Lit =
  VAR af: Equality; afRA: REFANY; BEGIN
    IF u1.uid < u2.uid THEN
      VAR t1 := u1; BEGIN u1 := u2; u2 := t1 END (* BEGIN *)
    END (* IF *);
    IF eqTab.get(Signature.T{u1.uid, u2.uid}, afRA) THEN
      af := afRA
    ELSE
      af := NEW(Equality, a := u1, b := u2, identity := TRUE).init();
      EVAL eqTab.put(Signature.T{u1.uid, u2.uid}, af);
      AFUndoStackPush(AFUndoType.EQ, eq := af)
    END (* IF *);
    RETURN NEW(AF.Lit, af := af, sense := TRUE)
  END NewEQ;

PROCEDURE EtpSimpAssert(<*UNUSED*> id: INTEGER) =
  BEGIN END EtpSimpAssert;

PROCEDURE AssertEQ(eq: Equality; lit: AF.Lit): BOOLEAN =
  VAR slk := NewSlack();
      ua := eq.a; ub :=eq.b;
      pivotStart := pivotCount;
  BEGIN
    INC(stats.totAsserts); INC(stats.eqAsserts);
    IF NOT InUse(ua) THEN
      ua := Enode.Root(ua.var).getMisc(FALSE).unknown;
      <*ASSERT InUse(ua) *>
    END (* IF *);
    IF NOT InUse(ub) THEN
      ub := Enode.Root(ub.var).getMisc(FALSE).unknown;
      <*ASSERT InUse(ub) *>
    END (* IF *);
    EtpSimpAssert(eq.id);
    slk.identity := eq.identity;
    IF computeSxs THEN
      slk.sx := RefList.List2(slkSym,
                              RefList.List3(PredSx.minusSym,
                                            Enode.DbgToSx(ua.var),
                                            Enode.DbgToSx(ub.var)))
    END (* IF *);
    Add(slk, ua, Rat.One);
    Add(slk, ub, Rat.NegOne);
    IF debug # 0 THEN
      DbgPrint("Simplex.AssertEQ: " & Sx_ToText(UnknownToSx(slk)) & " = " &
        Sx_ToText(UnknownToSx(ua)) & " - " &
        Sx_ToText(UnknownToSx(ub)) & ".\n")
    END (* IF *);
    VAR res: BOOLEAN; BEGIN
      IF lit.sense THEN
        res := AssertZero(slk)
      ELSE
        res := IsConstRow(slk.index) AND a[slk.index,0].num = 0
      END (* IF *);
      IF simplexLoopTrace THEN
        LoopTrace(pivotStart);
      END;
      RETURN res;
    END (* VAR *)
  END AssertEQ;

PROCEDURE InUse(u: Unknown): BOOLEAN =
  BEGIN
    TYPECASE u OF <*NOWARN*>
    | NULL =>
        <*ASSERT FALSE*>
    | LitUnknown =>
        RETURN TRUE
    | VarUnknown(vu) =>
        RETURN (vu.ownsRow AND vu.index < n AND y[vu.index] = vu) OR
               (NOT vu.ownsRow AND vu.index < m AND x[vu.index] = vu)
    END (* TYPECASE *)
  END InUse;

PROCEDURE AssertProlog(id: INTEGER; sense: BOOLEAN; a, b: Enode.T) =
  BEGIN
    IF ContextPrivate.inD1P THEN
      INC(stats.d1PAsserts);
      VAR recRA: REFANY; rec: AFStatus; BEGIN
        IF afStatusTab.get(id, recRA) THEN
          rec := recRA;
          IF rec.state[FALSE] AND rec.state[TRUE] THEN
            INC(stats.d1PAssertsRest)
          ELSE
            <*ASSERT rec.state[FALSE] OR rec.state[TRUE] *>
            INC(stats.d1PAssertsInit);
            IF NOT rec.state[sense] THEN
              rec.state[sense] := TRUE;
              AFMarkUndoStackPush(AFMarkUndoType.Set, id, rec, sense);
              <*ASSERT rec.state[FALSE] AND rec.state[TRUE] *>
            END (* IF *)
          END (* IF *)
        ELSE
          INC(stats.d1PAssertsInit);
          AFMarkUndoStackPush(AFMarkUndoType.New, id);
          rec := NEW(AFStatus);
          rec.state[FALSE] := FALSE; rec.state[TRUE] := FALSE;
          rec.state[sense] := TRUE;
          EVAL afStatusTab.put(id, rec)
        END (* IF *)
      END (* BEGIN *);
      IF Unconstrained(a, b) THEN INC(stats.d1PAssertsUnconstrained)
      END (* IF *)
    END (* IF *)
  END AssertProlog;

PROCEDURE Unconstrained(a, b: Enode.T): BOOLEAN =
  BEGIN RETURN a.getMisc(FALSE) = NIL OR a.getMisc(FALSE).unknown = NIL OR
               b.getMisc(FALSE) = NIL OR b.getMisc(FALSE).unknown = NIL
  END Unconstrained;

PROCEDURE AssertGE(ineq: GEInequality; lit: AF.Lit): BOOLEAN =
  VAR e1 := Enode.Root(ineq.a);
      e2 := Enode.Root(ineq.b);
      pivotStart := pivotCount; BEGIN
    INC(stats.totAsserts);
    AssertProlog(ineq.id, lit.sense, e1, e2);
    VAR ue1 := GetUnknown(e1, lit.activate);
	ue2 := GetUnknown(e2, lit.activate);
	slk := NewSlack();
    BEGIN
      EtpSimpAssert(ineq.id);
      IF computeSxs THEN
	slk.sx := RefList.List2(slkSym,
				RefList.List3(PredSx.minusSym,
					      Enode.DbgToSx(e1),
					      Enode.DbgToSx(e2)))
      END (* IF *);
      Add(slk, ue1, Rat.One);
      Add(slk, ue2, Rat.NegOne);
      IF debug # 0 THEN
	DbgPrint(Sx_ToText(UnknownToSx(slk)) & " = " &
	  Sx_ToText(UnknownToSx(ue1)) & " - " &
	  Sx_ToText(UnknownToSx(ue2)) & ".\n")
      END (* IF *);
      VAR res: BOOLEAN; BEGIN
        IF SlackIsObviouslyRedundant(slk, lit.sense) THEN
          INC(stats.redundAsserts);
          DeleteRow(slk.index, undoable := TRUE); res := TRUE
        ELSE
          res := AssertNonNeg(slk, lit.sense)
        END (* IF *);
        IF simplexLoopTrace THEN
          LoopTrace(pivotStart);
        END;
        RETURN res;
      END (* VAR *)
    END (* BEGIN *)
  END AssertGE;

(* Returns "TRUE" if the assertion represented by restricting "slk" in
   the direction indicated by "sense" is manifestly redundant.  That
   is, if "sense" is "TRUE" and the row owned by "slk" has no negative
   entries, and positve entries only in the constant column or
   columns owned by restricted slacks.  If "sense" is "FALSE", then
   the constant entry is negative, and all other non-zero entries are
   negative and owned by restricted slacks.
*)
PROCEDURE SlackIsObviouslyRedundant(slk: VarUnknown; sense: BOOLEAN): BOOLEAN =
  VAR i := slk.index; BEGIN
    <*ASSERT slk.ownsRow *>
    IF Prover.noObvSimpRedund THEN RETURN FALSE END (* IF *);
    IF sense THEN
      IF a[i, 0].num < 0 THEN RETURN FALSE END (* IF *);
      FOR j := dcol TO m-1 DO
        IF a[i, j].num < 0 OR 
          a[i, j].num > 0 AND NOT x[j].restricted THEN
          RETURN FALSE
        END (* IF *)
      END (* FOR *);
      RETURN TRUE
    ELSE
      IF a[i, 0].num >= 0 THEN RETURN FALSE END (* IF *);
      FOR j := dcol TO m-1 DO
        IF a[i, j].num > 0 OR 
          a[i, j].num < 0 AND NOT x[j].restricted THEN
          RETURN FALSE
        END (* IF *)
      END (* FOR *);
      RETURN TRUE
    END (* IF *)
  END SlackIsObviouslyRedundant;

PROCEDURE ZeroUnknownAssert(self: ZeroUnknownAF; lit: AF.Lit): BOOLEAN =
  VAR pivotStart := pivotCount; res: BOOLEAN; BEGIN
    <*ASSERT lit.sense *>
    res := AssertZero(self.u);
      IF simplexLoopTrace THEN
        LoopTrace(pivotStart);
      END;
    RETURN res
  END ZeroUnknownAssert;

TYPE
  UndoType = {RestrictFromFree, RestrictFromNN,
              DeadCol, NegRow0, NegRow1,
              Pivot, CreateVarUnknown, CreateLitUnknown,
              DelRow, DelLiveCol, DelDeadCol,
              TightenBounds, NonPrimitive, Mark };
  (* "NegRow0": numerically negated "rec.u"'s row.
     "NegRow1": logically negated "rec.u"'s constraint.
     "Pivot":   pivoted row owned by "u" with column owned by "u2".
  *)
  UndoRec = RECORD
    type: UndoType;
    u: VarUnknown; u2: Unknown;
    index: CARDINAL;
    den: CARDINAL := 1;
  END (* RECORD *);
  UndoStack = BRANDED REF ARRAY OF UndoRec;

  AFUndoType = { EQ, GE, Mark };
  AFUndoRec = RECORD
    type: AFUndoType; ineq: GEInequality; eq: Equality
  END (* RECORD *);
  AFUndoStack = BRANDED REF ARRAY OF AFUndoRec;

  AFMarkUndoType = { New, Set, Mark };
  AFMarkUndoRec = RECORD
    type: AFMarkUndoType;
    id: INTEGER;
    rec: AFStatus;
    sense: BOOLEAN
  END (* RECORD *);
  AFMarkUndoStack = BRANDED REF ARRAY OF AFMarkUndoRec;

VAR
  undoStack: UndoStack;
  undoSP: CARDINAL;
  nullUndoRec := UndoRec{type := VAL(0, UndoType), u := NIL, u2 := NIL,
                         index := 0};

  afUndoStack: AFUndoStack;
  afUndoSP: CARDINAL;
  nullAFUndoRec := AFUndoRec{type := VAL(0, AFUndoType), ineq := NIL, eq := NIL};

  afMarkUndoStack: AFMarkUndoStack;
  afMarkUndoSP: CARDINAL;
  nullAFMarkUndoRec := 
      AFMarkUndoRec{type := VAL(0, AFMarkUndoType), id := 0, rec := NIL,
                    sense := FALSE};

<*SPEC UndoStackPush(type, u, u2, index, den) MODIFIES UndoStk *>
PROCEDURE UndoStackPush(type: UndoType;
                        u: VarUnknown := NIL; u2: Unknown := NIL;
                        index: CARDINAL := 0;
                        den: CARDINAL := 1) =
  BEGIN
    IF undoSP = NUMBER(undoStack^) THEN
      VAR new := NEW(UndoStack, 2 * undoSP); BEGIN
        SUBARRAY(new^, 0, undoSP) := undoStack^;
        undoStack := new
      END (* BEGIN *)
    END (* IF *);
    undoStack[undoSP].type := type;
    undoStack[undoSP].u := u;
    undoStack[undoSP].u2 := u2;
    undoStack[undoSP].index := index;
    undoStack[undoSP].den := den;
    INC(undoSP)
  END UndoStackPush;

PROCEDURE AFUndoStackPush(type: AFUndoType;
                          ineq: GEInequality := NIL;
                          eq: Equality := NIL) =
  BEGIN
    IF afUndoSP = NUMBER(afUndoStack^) THEN
      VAR new := NEW(AFUndoStack, 2 * afUndoSP); BEGIN
        SUBARRAY(new^, 0, afUndoSP) := afUndoStack^;
        afUndoStack := new
      END (* BEGIN *)
    END (* IF *);
    afUndoStack[afUndoSP].type := type;
    afUndoStack[afUndoSP].ineq := ineq;
    afUndoStack[afUndoSP].eq := eq;
    INC(afUndoSP)
  END AFUndoStackPush;

PROCEDURE AFMarkUndoStackPush(type: AFMarkUndoType; id: INTEGER;
                              rec: AFStatus := NIL; sense: BOOLEAN := FALSE) =
  BEGIN
    IF afMarkUndoSP = NUMBER(afMarkUndoStack^) THEN
      VAR new := NEW(AFMarkUndoStack, 2 * afMarkUndoSP); BEGIN
        SUBARRAY(new^, 0, afMarkUndoSP) := afMarkUndoStack^;
        afMarkUndoStack := new
      END (* BEGIN *)
    END (* IF *);
    afMarkUndoStack[afMarkUndoSP].type := type;
    afMarkUndoStack[afMarkUndoSP].id := id;
    afMarkUndoStack[afMarkUndoSP].rec := rec;
    afMarkUndoStack[afMarkUndoSP].sense := sense;
    INC(afMarkUndoSP)
  END AFMarkUndoStackPush;

<*SPEC Init()
       MODIFIES Valid, UValid, VUValid,
                n, m, dcol, uidCntr, undoStack, undoSP,
                afUndoStack, afUndoSP,
                zero, eqTab, geIneqTab, delUnknownStack, nPush, depth, stats,
                afStatusTab
       ENSURES Valid'
*>
PROCEDURE Init() =
  BEGIN
    a := NEW(RatArr2, 100, 100);
    x := NEW(UnknownArr, 100); y := NEW(UnknownArr, 100);
    n := 0;
    m := 1;
    dcol := 1;
    uidCntr := 0;
    undoStack := NEW(UndoStack, 100);
    undoSP := 0;
    afUndoStack := NEW(AFUndoStack, 100);
    afUndoSP := 0;
    afMarkUndoStack := NEW(AFMarkUndoStack, 100);
    afMarkUndoSP := 0;
    VAR zNode := Enode.FromInt(0); BEGIN
      zero := zNode.getMisc(FALSE).unknown
    END (* BEGIN *);
    eqTab := NEW(SigTab.Default).init(1000);
    geIneqTab := NEW(GEIneqTab).init(1000);
    delUnknownStack := NEW(RefSeq.T).init();

    nPush := 0;
    depth := 0;
    stats := StatRec{};
    afStatusTab := NEW(IntRefTbl.Default).init()
  END Init;

VAR nPush: CARDINAL;
    depth: CARDINAL;

PROCEDURE Push() =
  BEGIN
    INC(depth); INC(nPush);
    UndoStackPush(UndoType.Mark);
    AFUndoStackPush(AFUndoType.Mark);
    IF NOT ContextPrivate.inD1P THEN
      AFMarkUndoStackPush(AFMarkUndoType.Mark, 0)
    END (* IF *)
  END Push;

TYPE
  URatArray = REF ARRAY OF RECORD u: VarUnknown; r: Rat.T END (* RECORD *);
  DelURec = REF RECORD
    u: VarUnknown;
    a: URatArray;
  END (* RECORD *);
VAR
  delUnknownStack: RefSeq.T;

PROCEDURE Pop() =
  (* Restore "C" to its last saved state.  That is, set "C :=
     SC:hipop()". *)
  BEGIN
    inPop := TRUE;
    TRY
    LOOP
        DEC(undoSP);
        TRY
          WITH top = undoStack[undoSP] DO
            CASE top.type OF
            | UndoType.RestrictFromFree =>
                top.u.restricted := FALSE
            | UndoType.RestrictFromNN =>
                top.u.isZero := FALSE
            | UndoType.DeadCol =>
                DEC(dcol);
                <*ASSERT top.u.index = dcol*>
            | UndoType.NegRow0 =>
                <*ASSERT top.u.ownsRow*>
                FOR j := 0 TO m-1 DO
                  a[top.u.index,j].num := -a[top.u.index,j].num
                END (* FOR *)
            | UndoType.NegRow1 =>
                <*ASSERT top.u.ownsRow*>
                FOR j := 0 TO m-1 DO
                  a[top.u.index,j].num := -a[top.u.index,j].num
                END (* FOR *);
                a[top.u.index,0] := Rat.Minus(a[top.u.index,0], Rat.One);
                top.u.sense := TRUE;
            | UndoType.Pivot =>
                (* "top.u" is the unknown owning the row that was pivoted
                   (and therefore now owns a column), and "top.index" is the
                   row that it originally owned. *)
                VAR u2: VarUnknown := top.u2; BEGIN
                  <*ASSERT NOT top.u.ownsRow AND u2.ownsRow *>
                  Pivot(u2.index, top.u.index, undoable := FALSE)
                END (* BEGIN *)
            | UndoType.CreateVarUnknown =>
                VAR u: VarUnknown := top.u; BEGIN
                  <*ASSERT NOT u.restricted*>
                  IF u.ownsRow THEN
                    DeleteRow(u.index)
                  ELSE
                    <*ASSERT u.index >= dcol*>
                    FOR i := 0 TO n-1 DO
                      <*ASSERT i = top.index OR a[i,u.index].num = 0*>
                    END (* FOR *);
                    DeleteCol(u.index)
                  END (* IF *);
                  IF u.var # NIL THEN
                    VAR m := u.var.getMisc(FALSE); BEGIN
                      IF m # NIL THEN m.unknown := NIL END (* IF *)
                    END (* BEGIN *)
                  END (* IF *)
                END (* BEGIN *)
            | UndoType.CreateLitUnknown =>
                <*ASSERT top.u2.var # NIL *>
                top.u2.var.getMisc(FALSE).unknown := top.u
            | UndoType.DelRow =>
                VAR entry: DelURec := delUnknownStack.remhi(); BEGIN
                  <*ASSERT entry.u.ownsRow *>
                  INC(n); IncNStats();
                  VAR i := entry.u.index; BEGIN
                    y[n-1] := y[i]; y[n-1].index := n-1;
                    FOR j := 0 TO m-1 DO a[n-1,j] := a[i,j] END (* FOR *);
                    a[i,0] := entry.a[0].r;
                    FOR j := 1 TO m-1 DO
                      a[i,entry.a[j].u.index] := entry.a[j].r
                    END (* FOR *);
                    y[i] := entry.u
                  END (* BEGIN *)
                END (* BEGIN *)
            | UndoType.DelLiveCol, UndoType.DelDeadCol =>
                VAR entry: DelURec := delUnknownStack.remhi(); BEGIN
                  <*ASSERT NOT entry.u.ownsRow *>
                  INC(m); IncMStats();
                  VAR j := entry.u.index; BEGIN
                    CASE top.type OF <*NOWARN*>
                    | UndoType.DelLiveCol =>
                        x[m-1] := x[j]; x[m-1].index := m-1;
                        FOR i := 0 TO n-1 DO a[i,m-1] := a[i,j] END (* FOR *);
                        FOR i := 0 TO n-1 DO
                          a[entry.a[i].u.index,j] := entry.a[i].r
                        END (* FOR *)
                    | UndoType.DelDeadCol =>
                        IF dcol < m-1 THEN
                          FOR i := 0 TO n-1 DO
                            a[i,m-1] := a[i,dcol]
                          END (* FOR *);
                          x[m-1] := x[dcol]; x[m-1].index := m-1
                        END (* IF *);
                        IF j # dcol THEN
                          x[dcol] := x[j]; x[dcol].index := dcol;
                          FOR i := 0 TO n-1 DO a[i,dcol] := a[i,j] END (* FOR *)
                        END (* IF *);
                        FOR i := 0 TO n-1 DO
                          a[entry.a[i].u.index,j] := entry.a[i].r
                        END (* FOR *);
                        INC(dcol)
                    END (* CASE *);
                    x[j] := entry.u
                  END (* BEGIN *)
                END (* BEGIN *)
            | UndoType.TightenBounds =>
                a[top.u.index, 0] := Rat.Plus(a[top.u.index, 0],
                                              Rat.T{top.index, top.den})
            | UndoType.NonPrimitive =>
                top.u2.primitive := TRUE
            | UndoType.Mark =>
                EXIT
            END (* CASE *)
          END (* WITH *)
        FINALLY
          undoStack[undoSP] := nullUndoRec
        END (* TRY *)
      END (* LOOP *);
      LOOP
        DEC(afUndoSP);
        TRY
          WITH top = afUndoStack[afUndoSP] DO
            CASE top.type OF
            | AFUndoType.GE =>
                VAR ra: REFANY; b := geIneqTab.delete(top.ineq, ra); BEGIN
                  <*ASSERT b*>
                END (* BEGIN *)
            | AFUndoType.EQ =>
                VAR ra: REFANY;
                    b := eqTab.delete(
                           Signature.T{top.eq.a.uid, top.eq.b.uid}, ra);
                BEGIN
                  <*ASSERT b*>
                END (* BEGIN *)
            | AFUndoType.Mark =>
                EXIT
            END (* CASE *)
          END (* WITH *);
        FINALLY
          afUndoStack[afUndoSP] := nullAFUndoRec
        END (* TRY *)
      END (* LOOP *);
      IF NOT ContextPrivate.inD1P THEN
        LOOP
          DEC(afMarkUndoSP);
          TRY
            WITH top = afMarkUndoStack[afMarkUndoSP] DO
              CASE top.type OF
              | AFMarkUndoType.New =>
                  VAR ra: REFANY; b := afStatusTab.delete(top.id, ra); BEGIN
                    <*ASSERT b*>
                  END (* BEGIN *)
              | AFMarkUndoType.Set =>
                  top.rec.state[top.sense] := FALSE
              | AFMarkUndoType.Mark =>
                  EXIT
              END (* CASE *)
            END (* WITH *);
          FINALLY
            afMarkUndoStack[afMarkUndoSP] := nullAFMarkUndoRec
          END (* TRY *)
        END (* LOOP *)
      END (* IF *);
      DEC(depth);
      IF debug > 1 THEN
        DbgPrint("\nIn Pop:\n");
        PrintTableau()
      END (* IF *);
    FINALLY
      inPop := FALSE;
    END;
  END Pop;

PROCEDURE IsConstRow(i: CARDINAL): BOOLEAN =
  BEGIN
    FOR j := dcol TO m-1 DO
      IF a[i, j].num # 0 THEN RETURN FALSE END (* IF *)
    END (* FOR *);
    RETURN TRUE
  END IsConstRow;

(* Attempts to find pairs of unknowns whose constraints taken
   together, form a slice bounded by two hyperplanes.  If successful,
   assumes that constraints are over integer functions, and attempts
   to tighten the bounds by querying whether the assertion that one of
   the unknowns takes on its extreme value is satisfiable.  If it is
   not, moves that constraint towards the middle by one and tries
   again.  That process may lead to a contradiction if the two
   hyperplanes cross, or it may lead to a propagated equality if they meet
   exactly and are dependent on a single column variable. *)
PROCEDURE TightenBounds(): BOOLEAN RAISES {Prover.Timeout} =
  VAR uArr: UnknownArr;
      tstSlk: VarUnknown;
      nPre: CARDINAL;
  BEGIN
    Context.opsEnabled[Context.Ops.TightenBounds] := FALSE;
    DeleteAllRedundant();
    uArr := NEW(UnknownArr, n + m - 1);
    nPre := n;
    SUBARRAY(uArr^, 0, m-1) := SUBARRAY(x^, 1, m-1);
    SUBARRAY(uArr^, m-1, n) := SUBARRAY(y^, 0, n);
    tstSlk := NewSlack();
    FOR k := 0 TO nPre + m - 2 DO  (* n for y, + (m-1) for x, - 1 for 0-based *)
      VAR u := uArr[k]; pivotColOwner: VarUnknown := NIL; BEGIN
        TRY
          Context.Push();
          IF NOT u.slack AND u.primitive AND
            (NOT u.ownsRow OR MakeOwnColumnNonFeasible(u, pivotColOwner)) THEN
            VAR pos, neg := -1;  (* -1 => not found, -2 => more than 1 found. *)
            BEGIN
              FOR i := 0 TO nPre-1 DO
                IF y[i].restricted AND NOT y[i].isZero THEN
                  IF a[i, u.index].num > 0 AND pos # -2 THEN
                    IF pos = -1 THEN pos := i
                    ELSE
                      Add(tstSlk, y[pos], Rat.One);
                      Add(tstSlk, y[i], Rat.Times(Rat.NegOne,
                                                  Rat.Recip(a[i, u.index])));
                      IF IsConstRow(tstSlk.index) THEN
                        IF a[tstSlk.index, 0].num > 0 THEN pos := i END (* IF *)
                      ELSE
                        pos := -2
                      END (* IF *)
                    END (* IF *);
                    (* Reset tstSlk row. *)
                    Add(tstSlk, tstSlk, Rat.NegOne)
                  ELSIF a[i, u.index].num < 0 AND neg # -2 THEN
                    IF neg = -1 THEN neg := i
                    ELSE
                      Add(tstSlk, y[neg], Rat.One);
                      Add(tstSlk, y[i], Rat.Times(Rat.NegOne,
                                                  Rat.Recip(a[i, u.index])));
                      IF IsConstRow(tstSlk.index) THEN
                        IF a[tstSlk.index, 0].num < 0 THEN neg := i END (* IF *)
                      ELSE
                        neg := -2
                      END (* IF *);
                      (* Reset tstSlk row. *)
                      Add(tstSlk, tstSlk, Rat.NegOne)
                    END (* IF *)
                  END (* IF *)
                END (* IF *)
              END (* FOR *);
              IF pos >= 0 AND neg >= 0 THEN
                Add(tstSlk, y[pos], Rat.Recip(a[pos, u.index]));
                Add(tstSlk, y[neg], Rat.Recip(Rat.Abs(a[neg, u.index])));
                IF IsConstRow(tstSlk.index) THEN
                  VAR diff := a[tstSlk.index, 0];
                      orig := a[pos, 0];
                      yPos := y[pos];
                  BEGIN
                    <*ASSERT diff.num >= 0 *>
                    a[pos, 0] := Rat.Times(
                                     Rat.Floor(
                                         Rat.Times(a[pos, 0],
                                                   Rat.Recip(a[pos, u.index]))),
                                     a[pos, u.index]);
                    LOOP
                      IF diff.num < 0 THEN EXIT END (* IF *);
                      TRY
                        Context.Push();
                        IF pivotColOwner # NIL THEN
                          Pivot(pivotColOwner.index, u.index)
                        END (* IF *);
                        Context.Assert(NEW(AF.Lit,
                                           af := NEW(ZeroUnknownAF,
                                                     u := yPos).init()));
                        Context.UnitConsequences();
                        IF Context.sat THEN EXIT END (* IF *);
                      FINALLY
                        Context.Pop()
                      END (* TRY *);
                      a[yPos.index, 0] := Rat.Minus(a[yPos.index, 0], Rat.One);
                      diff := Rat.Minus(diff, Rat.One)
                    END (* LOOP *);
                    IF a[yPos.index, 0].num # orig.num OR
                      a[yPos.index, 0].den # orig.den THEN
                      VAR delta := Rat.Minus(orig, a[yPos.index, 0]); BEGIN
                        <*ASSERT delta.num > 0*>
                        UndoStackPush(UndoType.TightenBounds,
                                      u := yPos,
                                      index := delta.num, den := delta.den)
                      END (* BEGIN *)
                    END (* IF *);
                    IF diff.num < 0 THEN
                      (* Reset tstSlk row. *)
                      Add(tstSlk, tstSlk, Rat.NegOne);
                      RETURN FALSE
                    ELSIF diff.num = 0 AND u.var # NIL THEN
                      <*ASSERT a[yPos.index, 0].den = 1*>
                      VAR r := Rat.Div(a[yPos.index, 0],
                                       a[yPos.index, u.index]); BEGIN
                        <*ASSERT r.den = 1*>
                        ContextPrivate.Propagate(Enode.NewEq(
                                                     u.var,
                                                     Enode.FromInt(-r.num)));
                      END (* BEGIN *)
                    END (* IF *)
                  END (* BEGIN *)
                END (* IF *)
              END (* IF *);
              (* Reset tstSlk row. *)
              Add(tstSlk, tstSlk, Rat.NegOne)
            END (* BEGIN *)
          END (* IF *)
        FINALLY
          Context.Pop()
        END (* TRY *)
      END (* BEGIN *)
    END (* FOR *);
    RETURN TRUE
  END TightenBounds;

PROCEDURE Top(): RefList.T =
  VAR res: RefList.T := NIL;
      uArr := NEW(UnknownArr, n + m - 1);
  BEGIN
    IF debug > 1 THEN
      DbgPrint("\nIn Top:\n");
      PrintTableau()
    END (* IF *);
    DeleteAllRedundant();
    (* Now make all column owners primitive non-slacks. *)
    SUBARRAY(uArr^, 0, m-1) := SUBARRAY(x^, 1, m-1);
    SUBARRAY(uArr^, m-1, n) := SUBARRAY(y^, 0, n);
    VAR badCol := TRUE; nUnknowns := n + m - 1; BEGIN
      WHILE badCol DO
        badCol := FALSE;
        VAR j := 0; BEGIN
          WHILE j < nUnknowns AND NOT badCol DO
            VAR u := uArr[j]; BEGIN
              IF (u.slack OR NOT u.primitive) AND
                NOT u.ownsRow AND u.index >= dcol THEN
                badCol := TRUE;
                IF debug > 2 THEN
                  DbgPrint("\nFound bad column owner in col " &
                    Fmt.Int(u.index) & ":\n");
                  PrintTableau()
                END (* IF *);

                VAR ii := 0; uu: VarUnknown; primRow := FALSE; BEGIN
                  WHILE ii < nUnknowns AND NOT primRow DO
                    uu := uArr[ii];
                    IF NOT uu.slack AND uu.primitive AND uu.ownsRow THEN
                      VAR jj := dcol; BEGIN
                        WHILE jj < m AND
                          (a[uu.index, jj].num = 0 OR
                          (NOT x[jj].slack AND x[jj].primitive)) DO
                          INC(jj)
                        END (* WHILE *);
                        IF jj < m THEN
                          Pivot(uu.index, jj); primRow := TRUE;
                        ELSE
                          (* uu was not primitive; it depended only on
                             primitives. *)
                          uu.primitive := FALSE;
                          UndoStackPush(UndoType.NonPrimitive, u2 := uu);
                          INC(ii)
                        END (* IF *)
                      END (* BEGIN *)
                    ELSE
                      INC(ii);
                    END (* IF *)
                  END (* WHILE *);
                  (* Assertions 1 and 2 in the proof. *)
                  <*ASSERT ii < nUnknowns AND primRow *>
                END (* BEGIN *)
              END (* IF *)
            END (* BEGIN *);
            INC(j);
          END (* WHILE *)
        END (* BEGIN *)
      END (* WHILE *)
    END (* BEGIN *);
    IF debug > 1 THEN
      DbgPrint("\nAfter deleting non-primitive non-slacks...:\n");
      PrintTableau()
    END (* IF *);
    (* Now, add a conjunct for each slack that does not represent an
       arithmetic identity. *)
    FOR k := 0 TO n + m - 2 DO  (* n for y, + (m-1) for x, - 1 for 0-based *)
      VAR u := uArr[k]; BEGIN
        IF u.slack AND NOT u.identity THEN
          IF debug > 2 THEN
            PrintTableau();
            DbgPrint(
                "Printing conjunct for " & Sx_ToText(UnknownToSx(u)) & ".\n")
          END (* IF *);
          <*ASSERT u.slack AND u.ownsRow AND u.restricted*>
          VAR pos: RefList.T := NIL;
              neg: RefList.T := NIL;
              i := u.index;
              constSum := Rat.Zero;
          BEGIN
            FOR j := dcol TO m-1 DO
              <*ASSERT NOT x[j].slack*>
              VAR xjSx := Enode.ToSx(x[j].var); BEGIN
                TYPECASE xjSx OF
                | REF INTEGER(ri) =>
                    constSum := Rat.Plus(
                                    constSum,
                                    Rat.Times(Rat.T{num := ri^, den := 1},
                                              a[i, j]))
                ELSE
                    IF a[i,j].num > 0 THEN
                      IF a[i,j] = Rat.One THEN
                        pos := RefList.Cons(xjSx, pos)
                      ELSE
                        pos := RefList.Cons(RefList.List3(PredSx.timesSym,
                                                          RatToSx(a[i,j]),
                                                          xjSx),
                                            pos)
                      END (* IF *)
                    ELSIF a[i,j].num < 0 THEN
                      IF a[i,j] = Rat.NegOne THEN
                        neg := RefList.Cons(xjSx, neg)
                      ELSE
                        neg := RefList.Cons(
                                   RefList.List3(PredSx.timesSym, 
                                                 RatToSx(Rat.T{-a[i,j].num,
                                                               a[i,j].den}),
                                                 xjSx),
                                   neg)
                      END (* IF *)
                    END (* IF *)
                END (* TYPECASE *)
              END (* BEGIN *)
            END (* FOR *);
            VAR relSym: REFANY; BEGIN
              IF u.isZero THEN
                relSym := PredSx.eqSym
              ELSE
                relSym :=  PredSx.leSym
              END (* IF *);
              constSum := Rat.Plus(constSum, a[i,0]);
              IF constSum.num > 0 THEN
                pos := RefList.AppendD(pos, RefList.List1(RatToSx(constSum)))
              ELSIF constSum.num < 0 THEN
                IF constSum.num = -1 AND relSym = PredSx.leSym THEN
                  relSym := PredSx.ltSym
                ELSE
                  neg := RefList.AppendD(neg, RefList.List1(
                                                  RatToSx(Rat.T{-constSum.num,
                                                                constSum.den})))
                END (* IF *)
              END (* IF *);
              VAR posRA, negRA: REFANY; BEGIN
                IF pos # NIL THEN
                  IF RefList.Length(pos) = 1 THEN
                    posRA := pos.head
                  ELSE
                    posRA := RefList.Cons(PredSx.plusSym, pos)
                  END (* IF *)
                ELSE
                  posRA := zeroSym
                END (* IF *);
                IF neg # NIL THEN
                  IF RefList.Length(neg) = 1 THEN
                    negRA := neg.head
                  ELSE
                    negRA := RefList.Cons(PredSx.plusSym, neg)
                  END (* IF *)
                ELSE
                  negRA := zeroSym
                END (* IF *);
                res := RefList.Cons(RefList.List3(relSym, negRA, posRA), res)
              END (* BEGIN *)
            END (* BEGIN *)
          END (* BEGIN *)
        END (* IF *)
      END (* BEGIN *)
    END (* FOR *);
    RETURN res
  END Top;

PROCEDURE DeleteAllRedundant() =
  VAR uArr := NEW(UnknownArr, n + m - 1); BEGIN
    SUBARRAY(uArr^, 0, m-1) := SUBARRAY(x^, 1, m-1);
    SUBARRAY(uArr^, m-1, n) := SUBARRAY(y^, 0, n);
    (* First get rid of redundant constraints. *)
    FOR k := 0 TO n + m - 2 DO  (* n for y, + (m-1) for x, - 1 for 0-based *)
      VAR u := uArr[k]; BEGIN
        IF u.slack AND NOT u.identity THEN
          DeleteRedundant(u)
        END (* IF *)
      END (* BEGIN *)
    END (* FOR *)
  END DeleteAllRedundant;


(* Requires that "u" is a slack, and that the tableau is feasible.
   Deletes "u" iff "u" represents a redundant constraint, preserving the
   feasibility of the tableau.
*)
PROCEDURE DeleteRedundant(u: VarUnknown) =
  BEGIN
    IF IsRedundant(u) THEN DeleteUnknown(u) END (* IF *)
  END DeleteRedundant;

(* Requires that "u" is a slack, and that the tableau is feasible.
   Returns "TRUE" iff "u" represents a redundant constraint; preserves the
   feasibility of the tableau.
*)
PROCEDURE IsRedundant(u: VarUnknown): BOOLEAN =
  VAR res: BOOLEAN; dbgNormal := FALSE; BEGIN
    <*ASSERT u.slack*>
    IF NOT u.restricted OR u.isZero THEN 
      RETURN TRUE
    END (* IF *);
    Push();
    TRY
      IF NOT u.ownsRow THEN MakeOwnRow(u) END (* IF *);
      IF NOT u.ownsRow THEN
        res := FALSE
      ELSE
        NegRow0(u);
        res := NOT MakePositive(u, FALSE);
      END;
      dbgNormal := TRUE;
    FINALLY    
      IF NOT dbgNormal AND debug > 0 THEN
        Wr.PutText(Stdio.stdout, "Simplex.IsRedundant: abnormal exit");
        Wr.Flush(Stdio.stdout);
      END;
      Pop();
    END;    
    RETURN res
  END IsRedundant;


PROCEDURE DeleteUnknown(u: VarUnknown) =
  BEGIN
    IF NOT u.ownsRow AND u.index >= dcol THEN MakeOwnRow(u) END (* IF *);
    IF u.ownsRow THEN DeleteRow(u.index, undoable := TRUE)
    ELSE DeleteCol(u.index, undoable := TRUE)
    END (* IF *)
  END DeleteUnknown;
    
(* Move row "n-1" into row "i", and decrement "n". *)
PROCEDURE DeleteRow(i: CARDINAL; undoable := FALSE) =
  BEGIN
    IF NOT undoable AND
       NOT inPop THEN
      Wr.PutText(Stdio.stderr,
        "Simplex.DeleteRow: *** Called with undoable=FALSE, not from Pop.\n");
      Wr.Flush(Stdio.stdout)
    END;
    IF undoable THEN
      VAR urata := NEW(URatArray, m);
          entry := NEW(DelURec, u := y[i], a := urata);
      BEGIN
        urata[0].u := NIL; urata[0].r := a[i,0];
        FOR j := 1 TO m-1 DO
          urata[j].u := x[j]; urata[j].r := a[i,j]
        END (* FOR *);
        delUnknownStack.addhi(entry);
        UndoStackPush(UndoType.DelRow)
      END (* BEGIN *)
    END (* IF *);
    FOR j := 0 TO m-1 DO a[i,j] := a[n-1,j] END (* FOR *);
    y[i] := y[n-1]; y[i].index := i;
    DEC(n)
  END DeleteRow;
    
(* Move column "m-1" into column "j", and decrement "m".  If "j" is less than
   "dcol", also decrement "dcol".  Write an undo record iff "undoable"
   is "TRUE". *)
PROCEDURE DeleteCol(j: CARDINAL; undoable := FALSE) =
  BEGIN
    IF undoable THEN
      VAR urata := NEW(URatArray, n);
          entry := NEW(DelURec, u := x[j], a := urata);
      BEGIN
        FOR i := 0 TO n-1 DO
          urata[i].u := y[i]; urata[i].r := a[i,j]
        END (* FOR *);
        delUnknownStack.addhi(entry)
      END (* BEGIN *)
    END (* IF *);
    IF j >= dcol THEN
      FOR i := 0 TO n-1 DO a[i,j] := a[i,m-1] END (* FOR *);
      x[j] := x[m-1]; x[j].index := j;
      IF undoable THEN UndoStackPush(UndoType.DelLiveCol) END (* IF *)
    ELSE
      DEC(dcol);
      IF j # dcol THEN
        FOR i := 0 TO n-1 DO a[i,j] := a[i,dcol] END (* FOR *);
        x[j] := x[dcol]; x[j].index := j
      END (* IF *);
      IF dcol < m-1 THEN
        FOR i := 0 TO n-1 DO a[i,dcol] := a[i,m-1] END (* FOR *);
        x[dcol] := x[m-1]; x[dcol].index := dcol
      END (* IF *);
      IF undoable THEN UndoStackPush(UndoType.DelDeadCol) END (* IF *)
    END (* IF *);
    DEC(m)
  END DeleteCol;

PROCEDURE RatToSx(q: Rat.T): REFANY =
  (* Return an S-Expression representing "q". *)
  BEGIN
    IF q.num = 0 THEN
      RETURN zeroSym
    ELSIF q.den = 1 THEN
      RETURN IntToSx(q.num)
    ELSE
      RETURN RefList.List3(Atom.FromText("/"), IntToSx(q.num), IntToSx(q.den))
    END (* IF *)
  END RatToSx;

VAR ints: ARRAY [-100..100] OF REF INTEGER;

PROCEDURE IntToSx(i: INTEGER): REF INTEGER =
  BEGIN
    IF i >= -100 AND i <= 100 THEN
      IF ints[i] = NIL THEN
        ints[i] := NEW(REF INTEGER);
        ints[i]^ := i
      END (* IF *);
      RETURN ints[i]
    ELSE
      VAR res := NEW(REF INTEGER); BEGIN res^ := i; RETURN res END (* BEGIN *)
    END (* IF *)
  END IntToSx;

VAR zeroSym: REF INTEGER;
    seqSym := Atom.FromText("=");

PROCEDURE EqToSx(eq: Equality; normForm: BOOLEAN): REFANY =
  BEGIN
    IF normForm THEN
      RETURN RefList.List3(seqSym, Enode.ToSx(eq.a.var),
                           Enode.ToSx(eq.b.var))
    ELSE
      RETURN RefList.List3(seqSym, Enode.DbgToSx(eq.a.var),
                           Enode.DbgToSx(eq.b.var)) 
    END (* IF *)
  END EqToSx;

PROCEDURE EqFP(eq: Equality): FPrint.T =
  BEGIN RETURN FPrint.Combine(Enode.FingerP(eq.a.var),
                                Enode.FingerP(eq.b.var))
  END EqFP;
                      
PROCEDURE GEIneqToSx(ineq: GEInequality; normForm: BOOLEAN): REFANY =
  BEGIN
    IF normForm THEN
      RETURN RefList.List3(PredSx.gtSym, Enode.ToSx(ineq.a),
                           Enode.ToSx(ineq.b))
    ELSE
      RETURN RefList.List3(PredSx.geSym, Enode.DbgToSx(ineq.a),
                           Enode.DbgToSx(ineq.b))
    END (* IF *)
  END GEIneqToSx;

PROCEDURE GEIneqFP(ineq: GEInequality): FPrint.T =
  BEGIN RETURN FPrint.Combine(Enode.FingerP(ineq.a), Enode.FingerP(ineq.b))
  END GEIneqFP;

VAR isZeroSym := Atom.FromText("ISZERO");

PROCEDURE ZeroUnknownToSx(zu: ZeroUnknownAF; normForm: BOOLEAN): REFANY =
  BEGIN
    RETURN RefList.List2(isZeroSym, UnknownToSx(zu.u, normForm))
  END ZeroUnknownToSx;
    

PROCEDURE UnknownToSx(u: Unknown; normForm := FALSE): REFANY =
  BEGIN
    IF u.sx = NIL THEN
      IF u.var # NIL THEN
        IF normForm THEN
          RETURN Enode.ToSx(u.var)
        ELSE
          RETURN Enode.DbgToSx(u.var)
        END (* IF *)
      ELSE
        VAR vu: VarUnknown := u; BEGIN
          RETURN Atom.FromText("s" & Fmt.Int(vu.uid))
        END (* BEGIN *)
      END (* IF *)
    END (* IF *);
    RETURN u.sx
  END UnknownToSx;

VAR varFP := FPrint.FromText("VAR");
    noVarFP := FPrint.FromText("NOVAR");

PROCEDURE ZeroUnknownFP(zu: ZeroUnknownAF): FPrint.T =
  BEGIN
    IF zu.u.var # NIL THEN
      RETURN FPrint.Combine(varFP, Enode.FingerP(zu.u.var))
    ELSE
      RETURN FPrint.Combine(noVarFP, FPrint.FromInt(zu.u.uid))
    END (* IF *)
  END ZeroUnknownFP;

<*SPEC DbgPrint(txt) REQUIRES txt # NIL *>
PROCEDURE DbgPrint(txt: TEXT) =
  BEGIN
    Wr.PutText(Stdio.stdout, txt);
    Wr.Flush(Stdio.stdout)
  END DbgPrint;

<*SPEC PrintTableau(skipDead, targRow) *>
PROCEDURE PrintTableau(skipDead := FALSE; targRow := -1) =
  VAR colName := NEW(REF ARRAY OF TEXT, m);
      rowName := NEW(REF ARRAY OF TEXT, n);
      nIndex := 0;
  BEGIN
    Wr.PutText(Stdio.stdout, "-------------------------------------------\n");
    FOR j := 1 TO m-1 DO
      IF NOT skipDead OR j = 0 OR j >= dcol THEN
        VAR n := Sx_ToText(UnknownToSx(x[j]));
            suffix := "";
        BEGIN
          IF x[j].restricted THEN
            IF x[j].isZero THEN
              IF x[j].sense THEN
                suffix := "(z)"
              ELSE
                suffix := "(Z)"
              END (* IF *)
            ELSE
              IF x[j].sense THEN
                suffix := "(n)"
              ELSE
                suffix := "(N)"
              END (* IF *)
            END (* IF *)
          END (* IF *);
          IF Text.Length(n & suffix) > 7 THEN
            colName[j] := "[" & Fmt.Int(nIndex) & "]";
            Wr.PutText(Stdio.stdout, colName[j] & " == " & n & "\n");
            colName[j] := colName[j] & suffix
          ELSE
            colName[j] := n & suffix
          END (* IF *);
          INC(nIndex);
        END (* BEGIN *)
      END (* IF *)
    END (* FOR *);
    FOR i := 0 TO n-1 DO
      IF targRow = -1 OR targRow = i THEN
        VAR n := Sx_ToText(UnknownToSx(y[i]));
            suffix := "";
        BEGIN
          IF y[i].restricted THEN
            IF y[i].isZero THEN
              IF y[i].sense THEN
                suffix := "(z)"
              ELSE
                suffix := "(Z)"
              END (* IF *)
            ELSE
              IF y[i].sense THEN
                suffix := "(n)"
              ELSE
                suffix := "(N)"
              END (* IF *)
            END (* IF *)
          END (* IF *);
          IF Text.Length(n & suffix) > 15 THEN
            rowName[i] := "[" & Fmt.Int(nIndex) & "]";
            Wr.PutText(Stdio.stdout, rowName[i] & " == " & n & "\n");
            rowName[i] := rowName[i] & suffix
          ELSE
            rowName[i] := n & suffix
          END (* IF *);
          INC(nIndex);
        END (* BEGIN *)
      END (* IF *)
    END (* FOR *);
    Wr.PutText(Stdio.stdout, "\t\t1's");
    FOR j := 1 TO m-1 DO
      IF NOT skipDead OR j = 0 OR j >= dcol THEN
        IF j = dcol THEN
          Wr.PutText(Stdio.stdout, "\t*" & colName[j])
        ELSE
          Wr.PutText(Stdio.stdout, "\t" & colName[j])
        END (* IF *);
      END (* IF *)
    END (* FOR *);
    Wr.PutText(Stdio.stdout, "\n");
    FOR i := 0 TO n-1 DO
      IF targRow = -1 OR targRow = i THEN
        Wr.PutText(Stdio.stdout, rowName[i]);
        IF Text.Length(rowName[i]) <= 7 THEN
          Wr.PutText(Stdio.stdout, "\t")
        END (* IF *);
        FOR j := 0 TO m-1 DO
          IF NOT skipDead OR j = 0 OR j >= dcol THEN
            Wr.PutText(Stdio.stdout, "\t" & Rat.ToText(a[i,j]))
          END (* IF *)
        END (* FOR *);
        Wr.PutText(Stdio.stdout, "\n")
      END (* FOR *);
      Wr.Flush(Stdio.stdout)
    END (* FOR *)
  END PrintTableau;

TYPE
  StatRec = RECORD
    pivotInnerRounds, pivotInnerRoundsEx,
    maxN, maxLive, maxMN, maxNAtMN, maxLiveColAtMN := 0;
    totAsserts, eqAsserts, redundAsserts := 0;
    d1PAsserts, d1PAssertsInit, d1PAssertsRest := 0;
    d1PAssertsUnconstrained := 0;
  END (* RECORD *);
  AFStatus = OBJECT state: ARRAY BOOLEAN OF BOOLEAN END (* OBJECT *);
VAR
  stats: StatRec;
  afStatusTab: IntRefTbl.T;

PROCEDURE IncNStats() =
  VAR live := m-dcol; BEGIN
    IF n > stats.maxN THEN stats.maxN := n END (* IF *);
    IF n*live > stats.maxMN THEN
      stats.maxMN := n*live; stats.maxNAtMN := n; stats.maxLiveColAtMN := live
    END (* IF *)
  END IncNStats;

PROCEDURE IncMStats() =
  VAR live := m-dcol; BEGIN
    IF live > stats.maxLive THEN stats.maxLive := live END (* IF *);
    IF n*live > stats.maxMN THEN
      stats.maxMN := n*live; stats.maxNAtMN := n; stats.maxLiveColAtMN := live
    END (* IF *)
  END IncMStats;


PROCEDURE Stats() =
  BEGIN
    DbgPrint(
        "Of " & Fmt.Int(stats.totAsserts) & " simplex assertions, " &
        Fmt.Int(stats.eqAsserts) & " assert equalities, and " &
        Fmt.Int(stats.redundAsserts) &
        " are obviously redundant inequalities.\n");
    DbgPrint(
        "Final tableau size is " & Fmt.Int(n) & " rows and " &
        Fmt.Int(m) & " columns, " & Fmt.Int(m-dcol) & " of those live.\n");
    DbgPrint(
        "Maximum tableau size product is " & Fmt.Int(stats.maxNAtMN) &
        " rows and " & Fmt.Int(stats.maxLiveColAtMN) & " live columns.\n");
    IF stats.pivotInnerRounds > 0 THEN
      VAR rat := FLOAT(stats.pivotInnerRoundsEx)/
                 FLOAT(stats.pivotInnerRounds)*100.0; BEGIN
        DbgPrint(Fmt.Real(rat, Fmt.Style.Auto, prec := 1) &
                 "% of pivot inner loops resulted in Rat ops.\n\n")
      END (* BEGIN *)
    END (* IF *);
    IF stats.d1PAsserts > 0 THEN
      VAR d1PAssertsF := FLOAT(stats.d1PAsserts); BEGIN
        DbgPrint(
            "Did " & Fmt.Int(stats.d1PAsserts) & " simplex assertions while " &
            "depth-1-plunging;\n" &
            "   " & Fmt.Int(stats.d1PAssertsInit) & "(" &
            Fmt.Real(FLOAT(stats.d1PAssertsInit)*100.0/d1PAssertsF, prec := 3) &
            "%) initial, " & Fmt.Int(stats.d1PAssertsRest) & "(" &
            Fmt.Real(FLOAT(stats.d1PAssertsRest)*100.0/d1PAssertsF, prec := 3) &
            "%) subsequent.\n" &
            "   " & Fmt.Int(stats.d1PAssertsUnconstrained) & "(" &
            Fmt.Real(FLOAT(stats.d1PAssertsUnconstrained)*100.0/d1PAssertsF,
                     prec := 3) &
            "%) assertions are unconstrained.\n")
      END (* BEGIN *)
    END (* IF *)
  END Stats;

PROCEDURE Sx_ToText(sx: REFANY): TEXT =
  VAR twr := TextWr.New(); BEGIN
    Sx.Print(twr, sx);
    RETURN TextWr.ToText(twr)
  END Sx_ToText;

PROCEDURE UnknownEqual(u1, u2: Unknown): BOOLEAN =
  BEGIN
    TYPECASE u1 OF <*NOWARN*>
    | VarUnknown =>
        RETURN u1 = u2
    | LitUnknown(lu1) =>
        TYPECASE u2 OF
        | LitUnknown(lu2) => RETURN lu1.val = lu2.val
        ELSE RETURN FALSE
        END (* TYPECASE *)
    END (* TYPECASE *)
  END UnknownEqual;

PROCEDURE GEInequalityEqualInContext(ineq1: GEInequality; af: AF.T): BOOLEAN =
  BEGIN
    TYPECASE af OF
    | GEInequality(ineq2) =>
        RETURN Enode.Root(ineq1.a) = Enode.Root(ineq2.a)
           AND Enode.Root(ineq1.b) = Enode.Root(ineq2.b)
    ELSE
        RETURN FALSE
    END (* TYPECASE *)
  END GEInequalityEqualInContext;

PROCEDURE GEInequalityHashInContext(ineq: GEInequality): Word.T =
  BEGIN 
    RETURN Word.Xor(Word.Rotate(ineq.a.getId(), Word.Size DIV 2),
                    ineq.b.getId())
  END GEInequalityHashInContext;

PROCEDURE GEIneqHash(<*UNUSED*> tbl: GEIneqTab;
                        READONLY ineqRA: REFANY): Word.T =
  VAR ineq: GEInequality := ineqRA; BEGIN
    RETURN Word.Xor(FPrint.Hash(ineq.a.getFP()), 
                    FPrint.Hash(ineq.b.getFP()))
  END GEIneqHash;

PROCEDURE GEIneqEqual(<*UNUSED*> tbl: GEIneqTab;
                      READONLY ineq1RA, ineq2RA: REFANY): BOOLEAN =
  VAR ineq1: GEInequality := ineq1RA; ineq2: GEInequality := ineq2RA; BEGIN
    RETURN ineq1.a = ineq2.a AND ineq1.b = ineq2.b
  END GEIneqEqual;

VAR computeSxs := FALSE;

PROCEDURE PrintSize(wr: Wr.T) =
  BEGIN
    Wr.PutText(wr, Fmt.Int(n) & " x " & Fmt.Int(m) & " = " & Fmt.Int(m * n))
  END PrintSize;

PROCEDURE LoopTrace(pivotStart: INTEGER) =
  BEGIN
    Wr.PutText(traceWriter, "; Simplex.LoopTrace: " &
       Fmt.Int(m) & " rows, " &
       Fmt.Int(n) & " cols, " &
       Fmt.Int(pivotCount-pivotStart) & " pivots\n");
    Wr.Flush(traceWriter);
  END LoopTrace;

<*FATAL OSError.E *>
VAR traceWriter: FileWr.T := NIL;

VAR simplexLoopTrace := FALSE;
(* For each instance of the simplex algorithm, writes size of matrix
   and number of pivots performed to "SimplexLoop.log".  Set by
   the environment variable PROVER_SIMPLEX_LOOP_TRACE. *)

BEGIN

  zeroSym := NEW(REF INTEGER); zeroSym^ := 0;
  FOR i := -100 TO 100 DO ints[i] := NIL END (* FOR *);
  IF Env.Get("PROVER_DBG") # NIL THEN computeSxs := TRUE END (* IF *);

  simplexLoopTrace := Env.Get("PROVER_SIMPLEX_LOOP_TRACE") # NIL;
  IF simplexLoopTrace THEN 
    Prover.envVars := Prover.envVars & "PROVER_SIMPLEX_LOOP_TRACE" & "\n";
    traceWriter := FileWr.OpenAppend("SimplexLoop.log")
  END (* IF *);

  VAR str := Env.Get("PROVER_SIMPLEX_DEBUG"); BEGIN
    IF str # NIL THEN
      debug := Scan.Int(str); <*NOWARN*>
    END (* IF *)
  END (* BEGIN *);

END Simplex.
