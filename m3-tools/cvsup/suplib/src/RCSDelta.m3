(* Copyright 1996-2003 John D. Polstra.
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
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgment:
 *      This product includes software developed by John D. Polstra.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
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
 * $Id$ *)

MODULE RCSDelta EXPORTS RCSDelta, RCSDeltaClass;

IMPORT
  Fmt, RCSDeltaList, RCSEdit, RCSError, RCSFile, RCSPhrases,
  RCSRevNum, RCSString, SortedRCSEditTbl, Text, TokScan;

(*****************************************************************************)
(* Accessor methods. *)
(*****************************************************************************)

REVEAL
  Accessor = AccessorPublic BRANDED OBJECT
    delta: T;
  METHODS
    init(delta: T): Accessor := AccessorInit;
  END;

PROCEDURE AccessorInit(self: Accessor; delta: T): Accessor =
  BEGIN
    self.delta := delta;
    RETURN self;
  END AccessorInit;

(*****************************************************************************)
(* HeadAccessor methods. *)
(*****************************************************************************)

TYPE
  HeadAccessor = Accessor OBJECT
    str: RCSString.T;
    nLines: CARDINAL;
    curLine: CARDINAL;
    iter: RCSString.Iterator;
  METHODS
    init(delta: T): HeadAccessor := HeadInit;
  OVERRIDES
    getLine := HeadGetLine;
    numLines := HeadNumLines;
  END;

PROCEDURE HeadInit(ha: HeadAccessor; delta: T): HeadAccessor =
  BEGIN
    EVAL Accessor.init(ha, delta);
    ha.str := delta.text;
    ha.nLines := delta.text.numLines();
    ha.curLine := LAST(CARDINAL);
    ha.iter := NIL;
    RETURN ha;
  END HeadInit;

PROCEDURE HeadGetLine(ha: HeadAccessor; i: CARDINAL): RCSString.T =
  VAR
    line: RCSString.T;
    ok: BOOLEAN;
  BEGIN
    <* ASSERT i >= 1 *>
    <* ASSERT i <= ha.nLines *>
    IF ha.curLine > i THEN  (* Seeking backwards, must start from front. *)
      ha.iter := ha.str.iterate();
      ha.curLine := 1;
    END;
    WHILE ha.curLine <= i DO  (* Skip over unwanted lines, keep one we want. *)
      ok := ha.iter.next(line);
      <* ASSERT ok *>
      INC(ha.curLine);
    END;
    RETURN line;
  END HeadGetLine;

PROCEDURE HeadNumLines(ha: HeadAccessor): CARDINAL =
  BEGIN
    RETURN ha.nLines;
  END HeadNumLines;

(*****************************************************************************)
(* DiffAccessor methods. *)
(*****************************************************************************)

TYPE
  DiffAccessor = Accessor OBJECT
    tbl: SortedRCSEditTbl.T;
    nLines: CARDINAL;
    iter: SortedRCSEditTbl.Iterator;
    curStart: CARDINAL;
    curKey: INTEGER;
    curEdit: RCSEdit.T;
    delKey: INTEGER;      (* Pending delete key *)
    delEdit: RCSEdit.T;   (* Pending delete record, or NIL *)
  METHODS
    init(delta: T): DiffAccessor RAISES {RCSError.E} := DiffInit;
  OVERRIDES
    getLine := DiffGetLine;
    numLines := DiffNumLines;
  END;

PROCEDURE DiffInit(da: DiffAccessor; delta: T): DiffAccessor
  RAISES {RCSError.E} =
  VAR
    iter := delta.text.iterate();
    ts: TokScan.T;
    line: RCSString.T;
    type: CHAR;
    where: CARDINAL;
    numLines: CARDINAL;
    baseLines := delta.diffBase.accessor.numLines();
    lastALine := -1;
    lastDLine := -1;
    key: CARDINAL;  (* Last output line affected by edit *)
    text: REF ARRAY OF RCSString.T;
    offset := 0;  (* Output line # + offset = input line # *)
    ok: BOOLEAN;
  BEGIN
    EVAL Accessor.init(da, delta);
    da.tbl := NEW(SortedRCSEditTbl.Default).init();
    da.delEdit := NIL;
    WHILE iter.next(line) DO
      ts := TokScan.New(line.toText());
      TRY
	WITH field = ts.getToken("edit command") DO
	  type := Text.GetChar(field, 0);
	  where := TokScan.AtoI(Text.Sub(field, 1), "edit line number");
	END;
	numLines := ts.getInt("edit line count");
      EXCEPT TokScan.Error(msg) =>
	RAISE RCSError.E(msg);
      END;

      CASE type OF
      | 'a' =>
	IF where > baseLines THEN
	  RAISE RCSError.E(
	    "\"a\" edit command specifies line past end of base delta");
	END;
	IF where <= lastALine OR where < lastDLine THEN
	  RAISE RCSError.E(
	    "\"a\" edit command applies to line preceding earlier edit");
	END;
	lastALine := where;
	key := where + numLines - offset;
	text := NEW(REF ARRAY OF RCSString.T, numLines);
	FOR i := 0 TO numLines-1 DO
	  IF NOT iter.next(text[i]) THEN
	    RAISE RCSError.E("Too few edit lines to add");
	  END;
	END;
	IF da.delEdit # NIL AND da.delKey = key - numLines THEN
	  (* We can merge this add into the previous delete. *)
	  da.delKey := key;
	  da.delEdit.text := text;
	  DiffPutPendingDelete(da);
	ELSE
	  DiffPutPendingDelete(da);
	  DiffAddEdit(da, key, NEW(RCSEdit.T, offset := offset, text := text));
	END;
	DEC(offset, numLines);
      | 'd' =>
	IF where + numLines - 1 > baseLines THEN
	  RAISE RCSError.E(
	    "\"d\" edit command specifies line past end of base delta");
	END;
	IF where <= lastALine OR where <= lastDLine THEN
	  RAISE RCSError.E(
	    "\"d\" edit command applies to line preceding earlier edit");
	END;
	lastDLine := where + numLines - 1;
	DiffPutPendingDelete(da);
	da.delKey := where - 1 - offset;
	da.delEdit := NEW(RCSEdit.T, offset := offset, text := NIL);
	INC(offset, numLines);
      ELSE
	RAISE RCSError.E(
	  "Invalid edit command \"" & Text.FromChar(type) & "\"");
      END;
    END;
    DiffPutPendingDelete(da);
    da.nLines := baseLines - offset;
    DiffAddEdit(da, da.nLines+1, NEW(RCSEdit.T, offset := offset,
      text := NIL));
    da.iter := da.tbl.iterate();
    da.curStart := 1;
    ok := da.iter.next(da.curKey, da.curEdit);
    <* ASSERT ok *>
    RETURN da;
  END DiffInit;

PROCEDURE DiffGetLine(da: DiffAccessor; i: CARDINAL): RCSString.T =
  VAR
    daCur := da;
    iCur := i;
  BEGIN
    (* This used to be a nice clean recursive function.  But it caused
       overflow of the limited thread stacks in some pathological cases.
       Luckily, the recursion was only tail recursion, so it was fairly
       easy to transform it into a loop. *)
    LOOP
      <* ASSERT iCur >= 1 *>
      <* ASSERT iCur <= daCur.nLines *>
      IF iCur < daCur.curStart OR iCur > daCur.curKey THEN  (* Need new edit *)
	IF iCur # daCur.curKey + 1 THEN  (* Random access, we have to seek *)
	  daCur.iter.seek(iCur);
	END;
	WITH ok = daCur.iter.next(daCur.curKey, daCur.curEdit) DO
	  <* ASSERT ok *>
	END;
	<* ASSERT iCur <= daCur.curKey *>
	daCur.curStart := iCur;
      END;

      IF daCur.curEdit.text # NIL THEN
	WITH firstAdded = daCur.curKey - NUMBER(daCur.curEdit.text^) + 1 DO
	  IF firstAdded <= iCur THEN
	    RETURN daCur.curEdit.text[iCur-firstAdded];
	  END;
	END;
      END;

      INC(iCur, daCur.curEdit.offset);
      WITH newAcc = daCur.delta.diffBase.accessor DO
	IF newAcc.delta.diffBase = NIL THEN  (* Found the head. *)
	  RETURN newAcc.getLine(iCur);
	END;
	daCur := newAcc;
      END;
    END;
  END DiffGetLine;

PROCEDURE DiffNumLines(da: DiffAccessor): CARDINAL =
  BEGIN
    RETURN da.nLines;
  END DiffNumLines;

PROCEDURE DiffAddEdit(da: DiffAccessor; key: CARDINAL; edit: RCSEdit.T)
  RAISES {RCSError.E} =
  BEGIN
    IF da.tbl.put(key, edit) THEN
      RAISE RCSError.E("Duplicate edit ending with output line "
	& Fmt.Int(key));
    END;
  END DiffAddEdit;

PROCEDURE DiffPutPendingDelete(da: DiffAccessor)
  RAISES {RCSError.E} =
  BEGIN
    IF da.delEdit # NIL THEN
      DiffAddEdit(da, da.delKey, da.delEdit);
      da.delEdit := NIL;
    END;
  END DiffPutPendingDelete;

(*****************************************************************************)
(* Support for getting reverse diffs. *)
(*****************************************************************************)

TYPE
  DiffIterator = RCSString.Iterator OBJECT
    delta: T;
    state: DIState;
    iter: SortedRCSEditTbl.Iterator;
    curKey, nextKey: INTEGER;
    curEdit, nextEdit: RCSEdit.T;
    nextAddedLine, lastAddedLine: CARDINAL;
    numAdded, numDeleted: CARDINAL;
  END;

  DIState = { GetNextEdit, PutDelCmd, PutAddCmd, PutAddedLine };

PROCEDURE GetReverseDiff(delta: T): RCSString.Iterator
  RAISES {RCSError.E} =
  VAR
    di: DiffIterator;
    ok: BOOLEAN;
  BEGIN
    <* ASSERT delta.diffBase # NIL *>
    MakeAccessors(delta);
    di := NEW(DiffIterator, next := ReverseDiffNext);
    di.delta := delta;
    di.iter := NARROW(delta.accessor, DiffAccessor).tbl.iterate();
    di.state := DIState.GetNextEdit;
    ok := di.iter.next(di.nextKey, di.nextEdit);
    <* ASSERT ok *>
    RETURN di;
  END GetReverseDiff;

PROCEDURE ReverseDiffNext(di: DiffIterator; VAR line: RCSString.T): BOOLEAN =
  VAR
  BEGIN
    LOOP
      CASE di.state OF
      | DIState.GetNextEdit =>
	di.curKey := di.nextKey;
	di.curEdit := di.nextEdit;
	IF NOT di.iter.next(di.nextKey, di.nextEdit) THEN
	  RETURN FALSE;
	END;
	IF di.curEdit.text # NIL THEN
	  di.numDeleted := NUMBER(di.curEdit.text^);
	ELSE
	  di.numDeleted := 0;
	END;
	di.numAdded := di.numDeleted + di.nextEdit.offset - di.curEdit.offset;
	IF di.numDeleted > 0 THEN
	  di.state := DIState.PutDelCmd;
	ELSIF di.numAdded > 0 THEN
	  di.state := DIState.PutAddCmd;
	END;

      | DIState.PutDelCmd =>
	<* ASSERT di.numDeleted > 0 *>
	WITH firstOutputLineDeleted = di.curKey - di.numDeleted + 1 DO
	  line := RCSString.FromText(
	    "d" & Fmt.Int(firstOutputLineDeleted) &
	    " " & Fmt.Int(di.numDeleted) & "\n");
	END;
	IF di.numAdded > 0 THEN
	  di.state := DIState.PutAddCmd;
	ELSE
	  di.state := DIState.GetNextEdit;
	END;
	RETURN TRUE;

      | DIState.PutAddCmd =>
	<* ASSERT di.numAdded > 0 *>
	WITH lastOutputLineDeleted = di.curKey DO
	  line := RCSString.FromText(
	    "a" & Fmt.Int(lastOutputLineDeleted) &
	    " " & Fmt.Int(di.numAdded) & "\n");
	END;
	di.nextAddedLine := di.curKey - di.numDeleted + 1 + di.curEdit.offset;
	di.lastAddedLine := di.nextAddedLine + di.numAdded - 1;
	di.state := DIState.PutAddedLine;
	RETURN TRUE;

      | DIState.PutAddedLine =>
	<* ASSERT di.nextAddedLine <= di.lastAddedLine *>
	line := di.delta.diffBase.accessor.getLine(di.nextAddedLine);
	INC(di.nextAddedLine);
	IF di.nextAddedLine > di.lastAddedLine THEN
	  di.state := DIState.GetNextEdit;
	END;
	RETURN TRUE;
      END;
    END;
  END ReverseDiffNext;

(* The following procedure is unused at present.  But it is correct.  It
   is rather complicated, and potentially useful, so I am leaving it in
   the code for the time being. *)

<*UNUSED*>
PROCEDURE ForwardDiffNext(di: DiffIterator; VAR line: RCSString.T): BOOLEAN =
  VAR
  BEGIN
    LOOP
      CASE di.state OF
      | DIState.GetNextEdit =>
	di.curKey := di.nextKey;
	di.curEdit := di.nextEdit;
	IF NOT di.iter.next(di.nextKey, di.nextEdit) THEN
	  RETURN FALSE;
	END;
	IF di.curEdit.text # NIL THEN
	  di.numAdded := NUMBER(di.curEdit.text^);
	ELSE
	  di.numAdded := 0;
	END;
	di.numDeleted := di.numAdded + di.nextEdit.offset - di.curEdit.offset;
	IF di.numDeleted > 0 THEN
	  di.state := DIState.PutDelCmd;
	ELSIF di.numAdded > 0 THEN
	  di.state := DIState.PutAddCmd;
	END;

      | DIState.PutDelCmd =>
	<* ASSERT di.numDeleted > 0 *>
	WITH lastInputLineNotDeleted = di.curKey - di.numAdded +
	  di.curEdit.offset,
	  firstInputLineDeleted = lastInputLineNotDeleted + 1 DO
	  line := RCSString.FromText(
	    "d" & Fmt.Int(firstInputLineDeleted) &
	    " " & Fmt.Int(di.numDeleted) & "\n");
	END;
	IF di.numAdded > 0 THEN
	  di.state := DIState.PutAddCmd;
	ELSE
	  di.state := DIState.GetNextEdit;
	END;
	RETURN TRUE;

      | DIState.PutAddCmd =>
	<* ASSERT di.numAdded > 0 *>
	WITH lastInputLineNotDeleted = di.curKey - di.numAdded +
	  di.curEdit.offset,
	  lastInputLineDeleted = lastInputLineNotDeleted + di.numDeleted DO
	  line := RCSString.FromText(
	    "a" & Fmt.Int(lastInputLineDeleted) &
	    " " & Fmt.Int(di.numAdded) & "\n");
	END;
	di.lastAddedLine := di.curKey;
	di.nextAddedLine := di.curKey - di.numAdded + 1;
	di.state := DIState.PutAddedLine;
	RETURN TRUE;

      | DIState.PutAddedLine =>
	<* ASSERT di.nextAddedLine <= di.lastAddedLine *>
	line := di.delta.accessor.getLine(di.nextAddedLine);
	INC(di.nextAddedLine);
	IF di.nextAddedLine > di.lastAddedLine THEN
	  di.state := DIState.GetNextEdit;
	END;
	RETURN TRUE;
      END;
    END;
  END ForwardDiffNext;

(*****************************************************************************)

TYPE
  TextIterator = RCSString.Iterator OBJECT
    delta: T;
    curLine: CARDINAL;
    numLines: CARDINAL;
  OVERRIDES
    next := TextNext;
  END;

PROCEDURE GetText(delta: T;
                  diffBase: T := NIL): RCSString.Iterator
  RAISES {RCSError.E} =
  VAR
    iter: RCSString.Iterator;
  BEGIN
    IF delta.diffBase = diffBase THEN
      (* What we want is in the delta's text. *)
      RCSFile.ParseDelta(delta.rcsFile, delta);
      iter := delta.text.iterate();
    ELSIF diffBase = NIL THEN
      (* Get the full text. *)
      MakeAccessors(delta);
      iter := NEW(TextIterator, delta := delta, curLine := 1,
	numLines := delta.accessor.numLines());
    ELSIF diffBase.diffBase = delta THEN
      (* We can do it with a reverse diff. *)
      iter := GetReverseDiff(diffBase);
    ELSE
      RAISE RCSError.E("Cannot compute diffs from " & diffBase.revision
	& " to " & delta.revision);
    END;
    RETURN iter;
  END GetText;

PROCEDURE TextNext(ti: TextIterator; VAR line: RCSString.T): BOOLEAN =
  BEGIN
    IF ti.curLine <= ti.numLines THEN
      line := ti.delta.accessor.getLine(ti.curLine);
      INC(ti.curLine);
      RETURN TRUE;
    END;
    RETURN FALSE;
  END TextNext;

PROCEDURE MakeAccessors(delta: T) RAISES {RCSError.E} =
  VAR
    stack: RCSDeltaList.T := NIL;
  BEGIN
    (* We use an explicit stack rather than recursion, to avoid
       overflowing the possibly-limited thread stack space. *)
    WHILE delta.accessor = NIL AND delta.diffBase # NIL DO
      stack := RCSDeltaList.Cons(delta, stack);
      delta := delta.diffBase;
    END;
    IF delta.accessor = NIL THEN  (* This delta has full text. *)
      RCSFile.ParseDelta(delta.rcsFile, delta);
      delta.accessor := NEW(HeadAccessor).init(delta);
    END;
    WHILE stack # NIL DO
      delta := stack.head;
      RCSFile.ParseDelta(delta.rcsFile, delta);
      delta.accessor := NEW(DiffAccessor).init(delta);
      stack := stack.tail;
    END;
  END MakeAccessors;

(*****************************************************************************)

PROCEDURE AddBranch(delta: T;
                    branch: T;
		    diffBase: T)
  RAISES {RCSError.E} =
  VAR
    bl := delta.branches;
    last: RCSDeltaList.T := NIL;
  BEGIN
    IF diffBase = NIL THEN diffBase := delta END;
    WHILE bl # NIL AND Compare(bl.head, branch) < 0 DO
      last := bl;
      bl := bl.tail;
    END;
    IF bl # NIL AND Equal(bl.head, branch) THEN
      RAISE RCSError.E("Attempt to add existing branch " & branch.revision);
    END;
    WITH newElem = RCSDeltaList.Cons(branch, bl) DO
      IF last = NIL THEN  (* Adding at front of list *)
	delta.branches := newElem;
      ELSE
	last.tail := newElem;
      END;
    END;
    branch.prev := delta;
    branch.diffBase := diffBase;
  END AddBranch;

PROCEDURE ChangeBranch(delta, oldBranch, newBranch: T)
  RAISES {RCSError.E} =
  VAR
    bl := delta.branches;
  BEGIN
    WHILE bl # NIL AND bl.head # oldBranch DO
      bl := bl.tail;
    END;
    IF bl = NIL THEN
      RAISE RCSError.E("Attempt to change nonexistent branch "
	& oldBranch.revision);
    END;
    oldBranch.prev := NIL;
    bl.head := newBranch;
    newBranch.prev := delta;
  END ChangeBranch;

PROCEDURE Compare(d1, d2: T): [-1..1] =
  BEGIN
    RETURN RCSRevNum.Compare(d1.revision, d2.revision);
  END Compare;

PROCEDURE Dead(delta: T; inAttic: BOOLEAN): BOOLEAN =
  VAR
    rf: RCSFile.T;
  BEGIN
    (* If its state is "dead", then it is dead. *)
    IF delta.state # NIL AND Text.Equal(delta.state, "dead") THEN
      RETURN TRUE;
    END;
    (* If it is in the Attic and it is the head of the default branch,
       then it is dead.  This is intended to approximate CVS's broken
       backward compatibility with early-version repositories. *)
    IF inAttic THEN
      rf := delta.rcsFile;
      IF rf.branch = NIL OR RCSRevNum.NumParts(rf.branch) = 1 THEN
	(* Default branch is main branch. *)
	RETURN RCSRevNum.IsTrunk(delta.revision) AND delta.prev = NIL;
      ELSE
	RETURN RCSRevNum.Equal(RCSRevNum.Prefix(delta.revision), rf.branch)
	  AND delta.next = NIL;
      END;
    END;
    (* It is alive. *)
    RETURN FALSE;
  END Dead;

PROCEDURE DeleteBranch(delta: T; branch: T)
  RAISES {RCSError.E} =
  VAR
    bl := delta.branches;
    last: RCSDeltaList.T := NIL;
  BEGIN
    WHILE bl # NIL AND bl.head # branch DO
      last := bl;
      bl := bl.tail;
    END;
    IF bl = NIL THEN
      RAISE RCSError.E("Attempt to delete nonexistent branch "
	& branch.revision);
    END;
    IF last = NIL THEN
      delta.branches := bl.tail;
    ELSE
      last.tail := bl.tail;
    END;
  END DeleteBranch;

PROCEDURE Equal(d1, d2: T): BOOLEAN =
  BEGIN
    RETURN RCSRevNum.Equal(d1.revision, d2.revision);
  END Equal;

PROCEDURE GetBranch(delta: T; revNum: RCSRevNum.T): T
  RAISES {RCSError.E} =
  VAR
    bl := delta.branches;
  BEGIN
    WITH n = RCSRevNum.NumParts(revNum) DO
      IF n < 3 OR n MOD 2 = 0 THEN
	RAISE RCSError.E("Attempt to get invalid branch number " & revNum);
      END;
    END;
    WHILE bl # NIL
    AND NOT RCSRevNum.Equal(RCSRevNum.Prefix(bl.head.revision), revNum) DO
      bl := bl.tail;
    END;
    IF bl = NIL THEN
      RAISE RCSError.E("Attempt to get nonexistent branch " & revNum);
    END;
    RETURN bl.head;
  END GetBranch;

PROCEDURE GetLog(delta: T): RCSString.T
  RAISES {RCSError.E} =
  BEGIN
    RCSFile.ParseDelta(delta.rcsFile, delta);
    RETURN delta.log;
  END GetLog;

PROCEDURE GetPrev(delta: T): T =
  BEGIN
    RETURN delta.prev;
  END GetPrev;

PROCEDURE Predecessor(delta: T): T =
  BEGIN
    IF RCSRevNum.IsTrunk(delta.revision) THEN
      RETURN delta.next;
    ELSE
      RETURN delta.prev;
    END;
  END Predecessor;

PROCEDURE ToText(delta : T) : TEXT =
  VAR res : TEXT;
  BEGIN
    IF delta.revision = NIL THEN
      res := "(no revision) ";
    ELSE
      res := delta.revision & " ";
    END;
    IF delta.date = NIL THEN
      res := res & "(no date) ";
    ELSE
      res := res & delta.date & " ";
    END;
    IF delta.author = NIL THEN
      res := res & "no author ";
    ELSE
      res := res & delta.author & " ";
    END;
    IF delta.state = NIL THEN
      res := res & "no state ";
    ELSE
      res := res & delta.state;
    END;
    RETURN res;
  END ToText;

(*****************************************************************************)
TYPE
  BranchIterator = Iterator OBJECT
    cur: RCSDeltaList.T;
  OVERRIDES
    next := NextBranch;
  END;

PROCEDURE IterateBranches(delta: T): Iterator =
  BEGIN
    RETURN NEW(BranchIterator, cur := delta.branches);
  END IterateBranches;

PROCEDURE IterateBranchesReversed(delta: T): Iterator =
  BEGIN
    RETURN NEW(BranchIterator, cur := RCSDeltaList.Reverse(delta.branches));
  END IterateBranchesReversed;

PROCEDURE NextBranch(bi: BranchIterator; VAR delta: T): BOOLEAN =
  BEGIN
    IF bi.cur = NIL THEN RETURN FALSE END;
    delta := bi.cur.head;
    bi.cur := bi.cur.tail;
    RETURN TRUE;
  END NextBranch;

(*****************************************************************************)

PROCEDURE IterateTextPhrases(delta: T): RCSPhrases.Iterator =
  BEGIN
    RETURN RCSPhrases.Iterate(delta.textPhrases);
  END IterateTextPhrases;

PROCEDURE IterateTreePhrases(delta: T): RCSPhrases.Iterator =
  BEGIN
    RETURN RCSPhrases.Iterate(delta.treePhrases);
  END IterateTreePhrases;

BEGIN
END RCSDelta.
