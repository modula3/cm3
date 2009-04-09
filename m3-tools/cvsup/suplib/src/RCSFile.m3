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

UNSAFE MODULE RCSFile;

IMPORT
  CText, FileAttr, Fmt, MD5, OSError, Pathname,
  RCSAccess, RCSAccessList, RCSDate, RCSDelta,
  RCSDeltaClass, RCSDeltaList, RCSDeltaListSort, RCSDeltaTbl,
  RCSError, RCSKeyword, RCSPhrase, RCSPhrases, RCSRevNum, RCSString,
  RCSTag, RCSTagList, RCSTagListSort, SortedRCSDeltaTbl, Text,
  TextIntTbl, TextSeq, Thread, TokScan, UnixMisc, Ustat, Word, Wr;

REVEAL
  T = Public BRANDED OBJECT
    attr: FileAttr.T := NIL;
    buf: ADDRESS := NIL;
    len: CARDINAL := 0;

    deltaTbl: SortedRCSDeltaTbl.T;
    accessList: RCSAccessList.T := NIL;
    tagList: RCSTagList.T := NIL;

    newPhrases: RCSPhrases.T := NIL;

    start: UNTRACED REF CHAR := NIL;
    ptr: UNTRACED REF CHAR := NIL;
    limit: UNTRACED REF CHAR := NIL;

    line: CARDINAL := 1;
    curTok: Token;

    head: RCSDelta.T := NIL;
    tail: RCSDelta.T := NIL;
  OVERRIDES
    init := Init;
  END;

VAR
  keyTab := NEW(TextIntTbl.Default).init(2*NUMBER(Keyword));

PROCEDURE CalculateMD5(rf: T; md5: MD5.T) =
  BEGIN
    md5.updateRaw(rf.buf, rf.len);
  END CalculateMD5;

PROCEDURE Close(rf: T) RAISES {OSError.E} =
  BEGIN
    IF rf.buf # NIL THEN
      UnixMisc.Unmap(rf.buf, rf.len);
      rf.buf := NIL;
    END;
    rf.len := 0;
    rf.start := NIL;
    rf.ptr := NIL;
    rf.limit := NIL;
  END Close;

PROCEDURE GetDelta(rf: T; revNum: RCSRevNum.T): RCSDelta.T
  RAISES {RCSError.E} =
  VAR
    delta: RCSDelta.T;
  BEGIN
    IF NOT rf.deltaTbl.get(revNum, delta) THEN
      Oops(rf, "Non-existent revision number " & revNum);
    END;
    RETURN delta;
  END GetDelta;

PROCEDURE GetHeadDelta(rf: T): RCSDelta.T
  RAISES {RCSError.E} =
  BEGIN
    IF rf.head = NIL THEN
      Oops(rf, "File contains no deltas");
    END;
    RETURN rf.head;
  END GetHeadDelta;

PROCEDURE GetTagDelta(rf: T;
                      tag: TEXT := NIL;
		      date: RCSDate.T := NIL): RCSDelta.T
  RAISES {RCSError.E} =
  CONST
    ImportDateFudge = 3.5d0;
    (* If the date stamps of revisions 1.1 and 1.1.1.1 differ by
       less than "ImportDateFudge" seconds, we assume the file was
       originally brought into the repository by a "cvs import". *)
  VAR
    tlp: RCSTagList.T;
    revNum: RCSRevNum.T;
    delta, delta1111: RCSDelta.T;
  BEGIN
    IF tag # NIL THEN  (* The caller specified a tag. *)
      tlp := rf.tagList;
      WHILE tlp # NIL DO
	IF Text.Equal(tlp.head.name, tag) THEN EXIT END;
	tlp := tlp.tail;
      END;
      IF tlp = NIL THEN
	Oops(rf, "No such tag " & tag);
      END;
      revNum := tlp.head.revNum;
    ELSE
      revNum := NIL;
    END;

    delta := GetRevDateDelta(rf, revNum, date);
    IF revNum = NIL AND RCSRevNum.Equal(delta.revision, "1.1") THEN
      (* A date-only search has found revision 1.1.  If this file was
         apparently created by a "cvs import", take the appropriate
	 revision from the vendor branch instead. *)
      TRY
	delta1111 := GetDelta(rf, "1.1.1.1");
	IF ABS(RCSDate.ToTime(delta.date) - RCSDate.ToTime(delta1111.date))
	  < ImportDateFudge
	THEN
	  RETURN GetRevDateDelta(rf, "1.1.1", date);
	END;
      EXCEPT RCSError.E => (* Just continue. *) END;
    END;
    RETURN delta;
  END GetTagDelta;

PROCEDURE GetRevDateDelta(rf: T;
                          revNum: RCSRevNum.T := NIL;
		          date: RCSDate.T := NIL): RCSDelta.T
  RAISES {RCSError.E} =
  VAR
    delta: RCSDelta.T;
    isCVSBranch := FALSE;
    isSpecific := FALSE;
    ok: BOOLEAN;
  BEGIN
    IF revNum # NIL THEN  (* The caller specified a revision number. *)
      IF RCSRevNum.NumParts(revNum) MOD 2 = 0 THEN
	(* Specific revision, or special CVS branch. *)
	WITH last = RCSRevNum.Last(revNum), prefix = RCSRevNum.Prefix(revNum) DO
	  IF NOT Text.Equal(RCSRevNum.Last(prefix), "0") THEN
	    (* Specific revision. *)
	    isSpecific := TRUE;
	    delta := GetDelta(rf, revNum);
	  ELSE
	    isCVSBranch := TRUE;
	    (* Convert the CVS branch into an RCS branch, and try
	       to get the tip of that branch.  If the branch doesn't
	       exist, get the branch-point delta instead. *)
	    revNum := RCSRevNum.Cat(RCSRevNum.Prefix(prefix), last);
	    TRY
	      delta := GetBranchTip(rf, revNum);
	    EXCEPT RCSError.E =>
	      delta := GetDelta(rf, RCSRevNum.Prefix(revNum));
	    END;
	  END;
	END;
      ELSE  (* RCS branch. *)
	delta := GetBranchTip(rf, revNum);
      END;
    ELSIF rf.branch # NIL THEN  (* Use the tip of the default branch. *)
      revNum := rf.branch;
      delta := GetBranchTip(rf, revNum);
    ELSIF rf.head # NIL THEN  (* Use the head. *)
      revNum := RCSRevNum.Prefix(rf.head.revision);
      delta := rf.head;
    ELSE
      Oops(rf, "File contains no deltas");
    END;

    (* At this point, "revNum" is guaranteed to be set non-NIL.  For a
       specific revision, it is the revision number of the specific
       delta.  For a branch, or for the default branch, it is the
       revision number of the branch. *)

    IF date # NIL THEN  (* The caller specified a date. *)
      IF isSpecific THEN
	ok := RCSDate.Compare(delta.date, date) <= 0;
      ELSE
	WHILE delta # NIL AND RCSDate.Compare(delta.date, date) > 0 DO
	  delta := RCSDelta.Predecessor(delta);
	END;
	IF delta = NIL THEN
	  ok := FALSE;
	ELSE
	  (* If the delta is on the correct branch, then it is OK. *)
	  IF RCSRevNum.NumParts(revNum) = 1 THEN  (* Must be on the trunk. *)
	    ok := RCSRevNum.IsTrunk(delta.revision);
	  ELSE  (* Must be on the same branch. *)
	    ok := RCSRevNum.Equal(revNum, RCSRevNum.Prefix(delta.revision));
	  END;
	  (* If it's a CVS branch, then it is also OK if the delta is at
	     the branch point. *)
	  IF NOT ok AND isCVSBranch THEN
	    ok := RCSRevNum.Equal(RCSRevNum.Prefix(revNum), delta.revision);
	  END;
	END;
      END;
      IF NOT ok THEN
	Oops(rf, "Non-existent revision/date combination");
      END;
    END;

    RETURN delta;
  END GetRevDateDelta;

PROCEDURE GetBranchTip(rf: T; branch: RCSRevNum.T): RCSDelta.T
  RAISES {RCSError.E} =
  VAR
    delta: RCSDelta.T;
    dlp: RCSDeltaList.T;
  BEGIN
    IF RCSRevNum.NumParts(branch) = 1 THEN  (* Main branch. *)
      delta := rf.head;
      WHILE delta # NIL DO
	IF RCSRevNum.Equal(RCSRevNum.Prefix(delta.revision), branch) THEN
	  EXIT;
	END;
	delta := delta.next;
      END;
      IF delta = NIL THEN
	Oops(rf, "No such branch " & branch);
      END;
    ELSE  (* A branch off of a delta. *)
      delta := GetDelta(rf, RCSRevNum.Prefix(branch));  (* The branch point. *)
      dlp := delta.branches;
      WHILE dlp # NIL DO
	IF RCSRevNum.Equal(RCSRevNum.Prefix(dlp.head.revision), branch) THEN
	  EXIT;
	END;
	dlp := dlp.tail;
      END;
      IF dlp = NIL THEN
	Oops(rf, "No such branch " & branch);
      END;

      (* Now follow the branch to its tip. *)
      delta := dlp.head;
      WHILE delta.next # NIL DO
	delta := delta.next;
      END;
    END;

    RETURN delta;
  END GetBranchTip;

PROCEDURE GetAttr(rf: T): FileAttr.T =
  BEGIN
    RETURN rf.attr;
  END GetAttr;

PROCEDURE GetToken(rf: T) =
  CONST
    WS = SET OF CHAR{' ', '\010', '\t', '\n', '\013', '\f', '\r'};
    Special = SET OF CHAR{'$', ',', '.', ':', ';', '@'};
    Sym = SET OF CHAR{'!' .. '~'} - Special;
    ID = Sym + SET OF CHAR{'.'};
    Digit = SET OF CHAR{'0' .. '9'};
    Num = Digit + SET OF CHAR{'.'};
  VAR
    start: UNTRACED REF CHAR;
    line: CARDINAL;
    ch: CHAR;
  BEGIN
    WHILE rf.ptr < rf.limit AND rf.ptr^ IN WS DO
      IF rf.ptr^ = '\n' THEN INC(rf.line) END;
      INC(rf.ptr);
    END;
    start := rf.ptr;
    line := rf.line;

    IF rf.ptr = rf.limit THEN
      rf.curTok.type := TokType.EOF;
      rf.curTok.keyword := Keyword.None;
      rf.curTok.line := line;
      rf.curTok.ptr := start;
      rf.curTok.len := 0;
      RETURN;
    END;

    IF rf.ptr^ IN ID THEN
      VAR
	type := TokType.Num;
	keyOrd := ORD(Keyword.None);
	ch: CHAR;
      BEGIN
	WHILE rf.ptr < rf.limit DO
	  ch := rf.ptr^;
	  IF NOT ch IN ID THEN EXIT END;
	  IF NOT ch IN Num THEN
	    type := TokType.Id;
	  END;
	  INC(rf.ptr);
	END;
	rf.curTok.ptr := start;
	rf.curTok.len := rf.ptr - start;
	IF type = TokType.Id THEN
	  EVAL keyTab.get(TokText(rf.curTok), keyOrd);
	END;
	rf.curTok.type := type;
	rf.curTok.keyword := VAL(keyOrd, Keyword);
	rf.curTok.line := line;
	RETURN;
      END;
    END;

    CASE rf.ptr^ OF
    | ';' =>
	INC(rf.ptr);
	rf.curTok.type := TokType.Semicolon;
	rf.curTok.keyword := Keyword.None;
	rf.curTok.line := line;
	rf.curTok.ptr := start;
	rf.curTok.len := 1;
	RETURN;
    | ':' =>
	INC(rf.ptr);
	rf.curTok.type := TokType.Colon;
	rf.curTok.keyword := Keyword.None;
	rf.curTok.line := line;
	rf.curTok.ptr := start;
	rf.curTok.len := 1;
	RETURN;
    | '@' =>
	INC(rf.ptr);
	start := rf.ptr;
	LOOP
	  IF rf.ptr = rf.limit THEN  (* Unterminated string. *)
	    rf.curTok.type := TokType.Bad;
	    rf.curTok.keyword := Keyword.None;
	    rf.curTok.line := line;
	    rf.curTok.ptr := start;
	    rf.curTok.len := rf.ptr - start;
	    RETURN;
	  END;
	  ch := rf.ptr^;
	  INC(rf.ptr);
	  IF ch = '@' THEN
	    IF rf.ptr = rf.limit OR rf.ptr^ # '@' THEN EXIT END;
	    INC(rf.ptr);
	  ELSIF ch = '\n' THEN
	    INC(rf.line);
	  END;
	END;
	rf.curTok.type := TokType.String;
	rf.curTok.keyword := Keyword.None;
	rf.curTok.line := line;
	rf.curTok.ptr := start;
	rf.curTok.len := rf.ptr - 1 - start;
	RETURN;
    ELSE
      INC(rf.ptr);
      rf.curTok.type := TokType.Bad;
      rf.curTok.keyword := Keyword.None;
      rf.curTok.line := line;
      rf.curTok.ptr := start;
      rf.curTok.len := 1;
      RETURN;
    END;
  END GetToken;

PROCEDURE Import(p: Pathname.T;
                 revNum: RCSRevNum.T;
		 author: TEXT;
		 state: TEXT;
		 logLines := -1): T
  RAISES {OSError.E} =
  (* Any RCSError.E that gets raised in this procedure really is due to
     an internal error.  We go ahead and let the core dump happen so that
     we can find the bug. *)
  <* FATAL RCSError.E *>
  VAR
    rf: T;
    statbuf: Ustat.struct_stat;
    np: CARDINAL;
    stack: TextSeq.T;
    date: RCSDate.T;
    delta, pred: RCSDelta.T;
    logEdits: TEXT;
  BEGIN
      rf := NEW(T).init();
      rf.buf := UnixMisc.MapFile(p, statbuf);
      rf.attr := FileAttr.FromStat(statbuf);
      rf.len := statbuf.st_size;
      rf.start := rf.buf;
      rf.limit := rf.start + rf.len;
      rf.ptr := rf.limit;  (* Already at "end of file". *)

      date := RCSDate.FromTime(FileAttr.GetModTime(rf.attr));

      np := RCSRevNum.NumParts(revNum);
      IF np = 0 THEN
	revNum := "1";
	INC(np);
      END;
      IF np MOD 2 = 1 THEN
	revNum := RCSRevNum.Cat(revNum, "1");
	INC(np);
      END;

      stack := NEW(TextSeq.T).init();
      WHILE np > 2 DO
	stack.addhi(revNum);
	revNum := RCSRevNum.Prefix(RCSRevNum.Prefix(revNum));
	DEC(np, 2);
      END;

      pred := NIL;
      delta := AddDelta(rf,
	revNum := revNum,
	diffBase := pred,
	date := date,
	author := author,
	state := state,
	log := RCSString.FromText("Initial revision\n"),
	text := NEW(SimpleString, ptr := rf.start, len := rf.len));
      delta.isPlaceHolder := TRUE;

      WHILE stack.size() > 0 DO
	revNum := stack.remhi();
	pred := delta;
	delta := AddDelta(rf,
	  revNum := revNum,
	  diffBase := pred,
	  date := date,
	  author := author,
	  state := state,
	  log := RCSString.FromText("Initial import.\n"),
	  text := RCSString.FromText(""));
	delta.isPlaceHolder := TRUE;
      END;

      delta.isPlaceHolder := FALSE;  (* The last delta is the real one. *)

      IF logLines >= 0 THEN
	logEdits := MakeLogEdits(rf, logLines);
	IF NOT Text.Empty(logEdits) THEN
	  DeleteDelta(rf, delta);
	  delta := AddDelta(rf,
	    revNum := revNum,
	    diffBase := delta,
	    date := date,
	    author := author,
	    state := state,
	    log := RCSString.FromText("Initial import.\n"),
	    text := RCSString.FromText(logEdits));
	END;
      END;

      RETURN rf;
  END Import;

PROCEDURE Init(rf: T;
               desc: RCSString.T := NIL): T =
  BEGIN
    IF desc = NIL THEN desc := RCSString.FromText("") END;

    rf.attr := NEW(FileAttr.T).init(FileAttr.FileType.File);
    rf.deltaTbl := NEW(SortedRCSDeltaTbl.Default).init();
    rf.desc := desc;
    rf.curTok := NEW(Token);

    RETURN rf;
  END Init;

PROCEDURE MakeLogEdits(rf: T; logLines: CARDINAL): TEXT =
  TYPE
    State = {
      Idle, NeedL, Needo, Needg, NeedColon,
      FindDollar, FindNewline, Voila, Ignore
    };
  VAR
    ptr: UNTRACED REF CHAR := rf.start;
    limit: UNTRACED REF CHAR := rf.start + rf.len;
    lineNum := 1;
    state := State.Idle;
    edits := "";
    ch: CHAR;
    ignoreCount: CARDINAL;
  BEGIN
    WHILE ptr < limit DO
      ch := ptr^;
      IF state # State.Idle THEN
	CASE state OF
	| State.Idle =>
	    <* ASSERT FALSE *>
	| State.NeedL =>
	    IF ch = 'L' THEN
	      state := State.Needo;
	    ELSE
	      state := State.Idle;
	    END;
	| State.Needo =>
	    IF ch = 'o' THEN
	      state := State.Needg;
	    ELSE
	      state := State.Idle;
	    END;
	| State.Needg =>
	    IF ch = 'g' THEN
	      state := State.NeedColon;
	    ELSE
	      state := State.Idle;
	    END;
	| State.NeedColon =>
	    IF ch = ':' THEN
	      state := State.FindDollar;
	    ELSE
	      state := State.Idle;
	    END;
	| State.FindDollar =>
	    IF ch = '$' THEN
	      state := State.FindNewline;
	    ELSIF ch = '\n' THEN
	      state := State.Idle;
	    END;
	| State.FindNewline =>
	    IF ch = '\n' THEN
	      state := State.Voila;
	    END;
	| State.Voila =>
	    edits := edits &
	      "d" & Fmt.Int(lineNum) & " " & Fmt.Int(logLines+2) & "\n";
	    ignoreCount := logLines;
	    state := State.Ignore;
	| State.Ignore =>
	    IF ch = '\n' THEN
	      DEC(ignoreCount);
	      IF ignoreCount = 0 THEN
		state := State.Idle;
	      END;
	    END;
	END;
      ELSIF ch = '$' THEN
	state := State.NeedL;
      END;

      IF ch = '\n' THEN INC(lineNum) END;
      INC(ptr);
    END;
    RETURN edits;
  END MakeLogEdits;

PROCEDURE OpenReadonly(p: Pathname.T): T
  RAISES {OSError.E, RCSError.E} =
  VAR
    rf: T;
    statbuf: Ustat.struct_stat;
  BEGIN
    rf := NEW(T).init();
    rf.buf := UnixMisc.MapFile(p, statbuf);
    rf.attr := FileAttr.FromStat(statbuf);
    rf.len := statbuf.st_size;
    rf.start := rf.buf;
    rf.ptr := rf.start;
    rf.limit := rf.start + rf.len;

    TRY
      ParseAdmin(rf);
      ParseTree(rf);
      ParseDesc(rf);
      RETURN rf;
    EXCEPT RCSError.E(msg) =>
      TRY Close(rf) EXCEPT OSError.E => (* Ignore *) END;
      RAISE RCSError.E(msg);
    END;
  END OpenReadonly;

PROCEDURE ParseAdmin(rf: T) RAISES {RCSError.E} =
  VAR
    lastAccess: RCSAccessList.T := NIL;
    lastTag: RCSTagList.T := NIL;
  BEGIN
    (* head {num}; *)
    EatKeyword(rf, Keyword.Head);
    (* Figure out whether this RCS file is in the format produced by CVS
       for an initial import.  CVS doesn't bother to generate the same
       whitespace as RCS would have, sigh. *)
    IF rf.ptr < rf.limit AND rf.ptr^ = ' ' THEN
      rf.options := rf.options + Options{Option.CVSInitialImport};
    ELSE
      rf.options := rf.options - Options{Option.CVSInitialImport};
    END;
    IF HaveType(rf, TokType.Num) THEN
      rf.head := EnterDelta(rf, TokText(CurTok(rf)));
      EatTok(rf);
    END;
    EatType(rf, TokType.Semicolon);

    (* {branch {num};} *)
    IF HaveKeyword(rf, Keyword.Branch) THEN
      EatTok(rf);
      IF HaveType(rf, TokType.Num) THEN
	rf.branch := TokText(CurTok(rf));
	EatTok(rf);
      END;
      EatType(rf, TokType.Semicolon);
    END;

    (* access {id}*; *)
    EatKeyword(rf, Keyword.Access);
    <* ASSERT rf.accessList = NIL *>
    WHILE HaveType(rf, TokType.Id) DO
      WITH access = NEW(RCSAccess.T) DO
	access.name := TokText(CurTok(rf));
	EatTok(rf);
	WITH l = RCSAccessList.List1(access) DO
	  IF lastAccess = NIL THEN
	    rf.accessList := l;
	  ELSE
	    lastAccess.tail := l;
	  END;
	  lastAccess := l;
	END;
      END;
    END;
    EatType(rf, TokType.Semicolon);

    (* symbols {sym:num}*; *)
    EatKeyword(rf, Keyword.Symbols);
    <* ASSERT rf.tagList = NIL *>
    WHILE HaveType(rf, TokType.Id) DO
      WITH tag = NEW(RCSTag.T) DO
	tag.name := TokText(CurTok(rf));
	EatTok(rf);
	EatType(rf, TokType.Colon);
	NeedType(rf, TokType.Num);
	tag.revNum := TokText(CurTok(rf));
	EatTok(rf);
	WITH l = RCSTagList.List1(tag) DO
	  IF lastTag = NIL THEN
	    rf.tagList := l;
	  ELSE
	    lastTag.tail := l;
	  END;
	  lastTag := l;
	END;
      END;
    END;
    EatType(rf, TokType.Semicolon);

    (* locks {id:num}*; {strict;} *)
    EatKeyword(rf, Keyword.Locks);
    WHILE HaveType(rf, TokType.Id) DO
      EatTok(rf);  (* FIXME *)
      EatType(rf, TokType.Colon);
      EatType(rf, TokType.Num);
    END;
    EatType(rf, TokType.Semicolon);
    IF HaveKeyword(rf, Keyword.Strict) THEN
      rf.strictLocking := TRUE;
      EatTok(rf);
      EatType(rf, TokType.Semicolon);
    ELSE
      rf.strictLocking := FALSE;
    END;

    (* {comment {string};} *)
    IF HaveKeyword(rf, Keyword.Comment) THEN
      EatTok(rf);
      IF HaveType(rf, TokType.String) THEN
	rf.comment := TokText(CurTok(rf));
	EatTok(rf);
      END;
      EatType(rf, TokType.Semicolon);
    END;

    (* {expand {string};} *)
    IF HaveKeyword(rf, Keyword.Expand) THEN
      EatTok(rf);
      IF HaveType(rf, TokType.String) THEN
	rf.expand := RCSKeyword.DecodeExpand(TokText(CurTok(rf)));
	EatTok(rf);
      END;
      EatType(rf, TokType.Semicolon);
    END;

    (* {newphrase}* *)
    rf.newPhrases := ParseNewPhrases(rf);
  END ParseAdmin;

PROCEDURE ParseTree(rf: T) RAISES {RCSError.E} =
  VAR
    delta: RCSDelta.T;
    ok: BOOLEAN;
  BEGIN
    WHILE HaveType(rf, TokType.Num) DO
      delta := EnterDelta(rf, TokText(CurTok(rf)));
      EatTok(rf);

      (* date num; *)
      EatKeyword(rf, Keyword.Date);
      NeedType(rf, TokType.Num);
      delta.date := TokText(CurTok(rf));
      EatTok(rf);
      EatType(rf, TokType.Semicolon);

      (* author id; *)
      EatKeyword(rf, Keyword.Author);
      NeedType(rf, TokType.Id);
      delta.author := TokText(CurTok(rf));
      EatTok(rf);
      EatType(rf, TokType.Semicolon);

      (* state {id}; *)
      EatKeyword(rf, Keyword.State);
      IF HaveType(rf, TokType.Id) THEN
	delta.state := TokText(CurTok(rf));
	EatTok(rf);
      END;
      EatType(rf, TokType.Semicolon);

      (* branches {num}*; *)
      EatKeyword(rf, Keyword.Branches);
      WHILE HaveType(rf, TokType.Num) DO
	WITH br = TokText(CurTok(rf)) DO
	  IF NOT RCSRevNum.Equal(RCSRevNum.Prefix(RCSRevNum.Prefix(br)),
	    delta.revision)
	  THEN
	    RAISE RCSError.E("Invalid branch " & br & " for delta "
	      & delta.revision);
	  END;
	  RCSDeltaClass.AddBranch(delta, EnterDelta(rf, br), delta);
	END;
	EatTok(rf);
      END;
      EatType(rf, TokType.Semicolon);

      (* next {num}; *)
      EatKeyword(rf, Keyword.Next);
      IF HaveType(rf, TokType.Num) THEN
	WITH next = EnterDelta(rf, TokText(CurTok(rf))) DO
	  IF RCSRevNum.IsTrunk(delta.revision) THEN
	    ok := RCSRevNum.IsTrunk(next.revision)
	      AND RCSRevNum.Compare(next.revision, delta.revision) < 0;
	  ELSE
	    ok := RCSRevNum.Compare(next.revision, delta.revision) > 0
	      AND RCSRevNum.Equal(RCSRevNum.Prefix(next.revision),
		RCSRevNum.Prefix(delta.revision));
	  END;
	  IF NOT ok THEN
	    RAISE RCSError.E("Invalid next delta " & next.revision
	      & " for delta " & delta.revision);
	  END;
	  delta.next := next;
	  next.prev := delta;
	  next.diffBase := delta;
	END;
	EatTok(rf);
      ELSIF RCSRevNum.IsTrunk(delta.revision) THEN
	rf.tail := delta;
      END;
      EatType(rf, TokType.Semicolon);

      (* {newphrase}* *)
      delta.treePhrases := ParseNewPhrases(rf);
    END;
  END ParseTree;

PROCEDURE ParseDelta(rf: T; delta: RCSDelta.T)
  RAISES {RCSError.E} =
  BEGIN
    WHILE NOT delta.isParsed DO
      ParseOneDeltaText(rf);
    END;
  END ParseDelta;

PROCEDURE ParseOneDeltaText(rf: T) RAISES {RCSError.E} =
  VAR
    revision: RCSRevNum.T;
    delta: RCSDelta.T;
  BEGIN
    (* num *)
    NeedType(rf, TokType.Num);
    revision := TokText(CurTok(rf));
    EatTok(rf);
    IF NOT rf.deltaTbl.get(revision, delta) THEN
      Oops(rf, "Missing revision " & revision);
    END;

    (* log string *)
    EatKeyword(rf, Keyword.Log);
    NeedType(rf, TokType.String);
    WITH tok = CurTok(rf) DO
      delta.log := NEW(QuotedString, ptr := tok.ptr, len := tok.len);
    END;
    EatTok(rf);

    (* {newphrase}* *)
    delta.textPhrases := ParseNewPhrases(rf);

    (* text string *)
    EatKeyword(rf, Keyword.Text);
    NeedType(rf, TokType.String);
    WITH tok = CurTok(rf) DO
      delta.text := NEW(QuotedString, ptr := tok.ptr, len := tok.len);
    END;
    EatTok(rf);
    delta.isParsed := TRUE;
  END ParseOneDeltaText;

PROCEDURE ParseDesc(rf: T) RAISES {RCSError.E} =
  VAR
    descEndingLine: CARDINAL;
  BEGIN
    (* desc string *)
    EatKeyword(rf, Keyword.Desc);
    NeedType(rf, TokType.String);
    WITH tok = CurTok(rf) DO
      rf.desc := NEW(QuotedString, ptr := tok.ptr, len := tok.len);
    END;
    EatTok(rf);

    (* Peek ahead at the next token, and figure out how many blank lines
       are between the end of the desc string and the start of the next
       token.  This varies depending on whether the initial checkin was
       done via RCS, or directly by CVS import.  We bother to check this
       so that we can try to generate an exact replica of the original
       file when we write it out. *)
    descEndingLine := rf.line;
    EVAL CurTok(rf);
    IF rf.line - descEndingLine - 1 > 2 THEN
      rf.options := rf.options + Options{Option.ExtraLineAfterDesc};
    ELSE
      rf.options := rf.options - Options{Option.ExtraLineAfterDesc};
    END;
  END ParseDesc;

PROCEDURE ParseNewPhrases(rf: T): RCSPhrases.T
  RAISES {RCSError.E} =
  VAR
    phrases: RCSPhrases.T := NIL;
    phrase: RCSPhrase.T;
  BEGIN
    IF HaveKeyword(rf, Keyword.None) THEN
      phrases := RCSPhrases.New();
      REPEAT
	phrase := RCSPhrase.New(TokText(rf.curTok));
	EatTok(rf);
	WHILE HaveType(rf, TokType.Id)
	OR HaveType(rf, TokType.Num)
	OR HaveType(rf, TokType.String)
	OR HaveType(rf, TokType.Colon)
	DO
	  RCSPhrase.Append(phrase, TokText(rf.curTok),
	    HaveType(rf, TokType.String));
	  EatTok(rf);
	END;
	EatType(rf, TokType.Semicolon);
	RCSPhrases.Append(phrases, phrase);
      UNTIL NOT HaveKeyword(rf, Keyword.None);
    END;
    RETURN phrases;
  END ParseNewPhrases;

PROCEDURE EnterDelta(rf: T; revision: RCSRevNum.T): RCSDelta.T =
  VAR
    delta: RCSDelta.T;
  BEGIN
    IF NOT rf.deltaTbl.get(revision, delta) THEN
      delta := NEW(RCSDelta.T, rcsFile := rf, revision := revision);
      EVAL rf.deltaTbl.put(revision, delta);
    END;
    RETURN delta;
  END EnterDelta;

(*****************************************************************************)
(* Modifying already-parsed RCS files. *)
(*****************************************************************************)

PROCEDURE AddDelta(rf: T;
                   revNum: RCSRevNum.T;
		   diffBase: RCSDelta.T;
                   date: TEXT;
		   author: TEXT;
		   state: TEXT;
		   log: RCSString.T;
		   text: RCSString.T;
		   treePhrases: RCSPhrases.T := NIL;
		   textPhrases: RCSPhrases.T := NIL): RCSDelta.T
  RAISES {RCSError.E} =
  VAR
    delta: RCSDelta.T;
    oldDelta: RCSDelta.T;
    next: RCSDelta.T;
    prev: RCSDelta.T;
    bp: RCSDelta.T;
    branch: RCSDelta.T;
    branchRevNum: RCSRevNum.T;
    bpRevNum: RCSRevNum.T;
  BEGIN
    WITH n = RCSRevNum.NumParts(revNum) DO
      IF n < 2 OR n MOD 2 # 0 THEN
	Oops(rf, "Attempt to add invalid revision number " & revNum);
      END;
    END;
    delta := NEW(RCSDelta.T,
      rcsFile := rf,
      revision := revNum,
      date := date,
      author := author,
      state := state,
      log := log,
      text := text,
      treePhrases := treePhrases,
      textPhrases := textPhrases,
      diffBase := diffBase,
      isParsed := TRUE);

    IF rf.deltaTbl.get(revNum, oldDelta) THEN  (* Delta already exists. *)
      IF NOT oldDelta.isPlaceHolder THEN
	Oops(rf, "Attempt to add existing delta " & revNum);
      END;
      prev := oldDelta.prev;
      next := oldDelta.next;
      delta.branches := oldDelta.branches;
      oldDelta.branches := NIL;
      DeleteDelta(rf, oldDelta);
    ELSE
      oldDelta := NIL;
      prev := NIL;
      next := NIL;
    END;

    IF RCSRevNum.IsTrunk(revNum) THEN
      IF oldDelta = NIL THEN  (* Find the insertion point. *)
	prev := NIL;
	next := rf.head;
	WHILE next # NIL AND RCSRevNum.Compare(next.revision, revNum) >= 0 DO
	  prev := next;
	  next := next.next;
	END;
      END;
      delta.prev := prev;
      delta.next := next;
      IF delta.prev # NIL THEN
	delta.prev.next := delta;
      ELSE
	rf.head := delta;
      END;
      IF delta.next # NIL THEN
	delta.next.prev := delta;
      ELSE
	rf.tail := delta;
      END;
    ELSE
      branchRevNum := RCSRevNum.Prefix(revNum);
      bpRevNum := RCSRevNum.Prefix(branchRevNum);
      IF NOT rf.deltaTbl.get(bpRevNum, bp) THEN
	Oops(rf, "No branch point for adding delta " & revNum);
      END;

      TRY
	branch := RCSDelta.GetBranch(bp, branchRevNum);
      EXCEPT RCSError.E =>
	branch := NIL;
      END;

      IF branch = NIL THEN
	RCSDeltaClass.AddBranch(bp, delta, diffBase);
      ELSE
	IF oldDelta = NIL THEN  (* Find the insertion point. *)
	  prev := bp;
	  next := branch;
	  WHILE next # NIL AND RCSRevNum.Compare(next.revision, revNum) <= 0 DO
	    prev := next;
	    next := next.next;
	  END;
	END;
	delta.prev := prev;
	delta.next := next;
	IF delta.prev # bp THEN
	  delta.prev.next := delta;
	ELSE
	  RCSDeltaClass.ChangeBranch(bp, delta.next, delta);
	END;
	IF delta.next # NIL THEN delta.next.prev := delta END;
      END;
    END;
    EVAL rf.deltaTbl.put(revNum, delta);
    RETURN delta;
  END AddDelta;

PROCEDURE AddTag(rf: T; name: TEXT; revNum: RCSRevNum.T): RCSTag.T
  RAISES {RCSError.E} =
  VAR
    tag := NEW(RCSTag.T, name := name, revNum := revNum);
    rem := RCSRevNum.NumParts(revNum) MOD 2;
    p := rf.tagList;
  BEGIN
    WHILE p # NIL DO
      IF RCSTag.Equal(p.head, tag) AND
	RCSRevNum.NumParts(p.head.revNum) MOD 2 = rem THEN
	Oops(rf, "Attempt to add existing tag " & name);
      END;
      p := p.tail;
    END;
    rf.tagList := RCSTagList.Cons(tag, rf.tagList);
    RETURN tag;
  END AddTag;

PROCEDURE DeleteDelta(rf: T; delta: RCSDelta.T)
  RAISES {RCSError.E} =
  BEGIN
    IF delta.branches # NIL THEN
      Oops(rf, "Attempt to delete delta (" & delta.revision
	& ") with branches");
    END;

    (* Parse the delta, if it has not already been parsed.  We may
       need it to be parsed later, e.g., if it is used as a diff
       base, or if we need to parse through it to get to a delta
       farther down in the file.  Once we have removed it from the
       delta table, it won't be possible to parse it any more, so
       to be safe, we have to do it now. *)
    ParseDelta(rf, delta);

    IF RCSRevNum.IsTrunk(delta.revision) THEN
      IF delta.prev # NIL THEN
	delta.prev.next := delta.next;
      ELSE
	rf.head := delta.next;
      END;
      IF delta.next # NIL THEN
	delta.next.prev := delta.prev;
      ELSE
	rf.tail := delta.prev;
      END;
    ELSE
      IF delta.prev.next # delta THEN  (* First delta on its branch. *)
	IF delta.next = NIL THEN  (* Only delta on its branch. *)
	  RCSDeltaClass.DeleteBranch(delta.prev, delta);
	ELSE
	  RCSDeltaClass.ChangeBranch(delta.prev, delta, delta.next);
	  delta.next.prev := delta.prev;
	END;
      ELSE
	delta.prev.next := delta.next;
	IF delta.next # NIL THEN delta.next.prev := delta.prev END;
      END;
    END;
    delta.next := NIL;
    delta.prev := NIL;
    EVAL rf.deltaTbl.delete(delta.revision, delta);
  END DeleteDelta;

PROCEDURE DeleteTag(rf: T; name: TEXT; revNum: RCSRevNum.T)
  RAISES {RCSError.E} =
  VAR
    p := rf.tagList;
    last: RCSTagList.T := NIL;
  BEGIN
    WHILE p # NIL DO
      IF Text.Equal(p.head.name, name) AND
	RCSRevNum.Equal(p.head.revNum, revNum) THEN
	IF last = NIL THEN
	  rf.tagList := p.tail;
	ELSE
	  last.tail := p.tail;
	END;
	RETURN;
      END;
      last := p;
      p := p.tail;
    END;
    Oops(rf, "No such tag " & name & ":" & revNum);
  END DeleteTag;

(*****************************************************************************)
(* Writing to a file *)
(*****************************************************************************)

PROCEDURE ToWr(rf: T; wr: Wr.T)
  RAISES {RCSError.E, Thread.Alerted, Wr.Failure} =
  BEGIN
    PutAdmin(rf, wr);
    PutDeltas(rf, wr);
    PutDesc(rf, wr);
    PutDeltaTexts(rf, wr);
  END ToWr;

PROCEDURE PutAdmin(rf: T; wr: Wr.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    accessIter: AccessIterator;
    access: RCSAccess.T;
    tagIter: TagIterator;
    tag: RCSTag.T;
    headWS: TEXT;
    branchWS: TEXT;
    accessWS: TEXT;
    accessSep: TEXT;
    symbolsWS: TEXT;
    symbolSep: TEXT;
    locksWS: TEXT;
    commentWS: TEXT;
    expandWS: TEXT;
  BEGIN
    IF Option.CVSInitialImport IN rf.options THEN
      headWS := "     ";
      branchWS := "   ";
      accessWS := "   ";
      accessSep := "   ";
      symbolsWS := " ";
      symbolSep := " ";
      locksWS := "    ";
      commentWS := "  ";
      expandWS := "   ";
    ELSE
      headWS := "\t";
      branchWS := "\t";
      accessWS := "";
      accessSep := "\n\t";
      symbolsWS := "";
      symbolSep := "\n\t";
      locksWS := "";
      commentWS := "\t";
      expandWS := "\t";
    END;

    Wr.PutText(wr, "head" & headWS);
    IF rf.head # NIL THEN
      Wr.PutText(wr, rf.head.revision);
    END;
    Wr.PutText(wr, ";\n");
    IF rf.branch # NIL THEN
      Wr.PutText(wr, "branch" & branchWS & rf.branch & ";\n");
    END;

    Wr.PutText(wr, "access" & accessWS);
    accessIter := IterateAccess(rf);
    WHILE accessIter.next(access) DO
      Wr.PutText(wr, accessSep & access.name);
    END;
    Wr.PutText(wr, ";\n");

    Wr.PutText(wr, "symbols" & symbolsWS);
    tagIter := IterateTags(rf);
    WHILE tagIter.next(tag) DO
      Wr.PutText(wr, symbolSep & tag.name & ":" & tag.revNum);
    END;
    Wr.PutText(wr, ";\n");

    Wr.PutText(wr, "locks" & locksWS & ";");  (* FIXME *)
    IF rf.strictLocking THEN
      Wr.PutText(wr, " strict;");
    END;
    Wr.PutChar(wr, '\n');
    IF rf.comment # NIL THEN
      Wr.PutText(wr, "comment" & commentWS & "@");
      PutEscaped(wr, rf.comment);
      Wr.PutText(wr, "@;\n");
    END;
    IF rf.expand # RCSKeyword.ExpandMode.Default THEN
      Wr.PutText(wr, "expand" & expandWS & "@");
      PutEscaped(wr, RCSKeyword.EncodeExpand(rf.expand));
      Wr.PutText(wr, "@;\n");
    END;
    PutPhrases(wr, rf.newPhrases);
    Wr.PutChar(wr, '\n');
  END PutAdmin;

PROCEDURE PutDeltas(rf: T; wr: Wr.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    stack: RCSDeltaList.T := NIL;
    delta: RCSDelta.T;
    iter: RCSDelta.Iterator;
    branch: RCSDelta.T;
    dateWS: TEXT;
    authorWS: TEXT;
    stateWS: TEXT;
    branchesWS: TEXT;
    branchesSep: TEXT;
    nextWS: TEXT;
  BEGIN
    IF Option.CVSInitialImport IN rf.options THEN
      dateWS := "     ";
      authorWS := "  ";
      stateWS := "  ";
      branchesWS := " ";
      branchesSep := "";
      nextWS := "     ";
    ELSE
      dateWS := "\t";
      authorWS := "\t";
      stateWS := "\t";
      branchesWS := "";
      branchesSep := "\n\t";
      nextWS := "\t";
    END;

    (* Emit the deltas in preorder.  We use the stack algorithm rather
       than recursion, because the recursion can become quite deep.  Since
       we are running in a thread, we don't have much stack space to waste. *)
    IF rf.head # NIL THEN
      stack := RCSDeltaList.Cons(rf.head, stack);
    END;
    WHILE stack # NIL DO
      (* Pop top delta from stack. *)
      delta := stack.head;
      stack := stack.tail;

      (* Emit the delta. *)
      Wr.PutText(wr, "\n" & delta.revision & "\n");
      Wr.PutText(wr, "date" & dateWS & delta.date & ";" &
	authorWS & "author " & delta.author & ";" &
	stateWS & "state ");
      IF delta.state # NIL THEN Wr.PutText(wr, delta.state) END;
      Wr.PutText(wr, ";\n");

      Wr.PutText(wr, "branches" & branchesWS);
      iter := RCSDelta.IterateBranches(delta);
      WHILE iter.next(branch) DO
	Wr.PutText(wr, branchesSep & branch.revision);
      END;
      Wr.PutText(wr, ";\n");

      Wr.PutText(wr, "next" & nextWS);
      IF delta.next # NIL THEN Wr.PutText(wr, delta.next.revision) END;
      Wr.PutText(wr, ";\n");

      PutPhrases(wr, delta.treePhrases);

      (* Push children in reverse order. *)
      iter := RCSDelta.IterateBranchesReversed(delta);
      WHILE iter.next(branch) DO
	stack := RCSDeltaList.Cons(branch, stack);
      END;
      IF delta.next # NIL THEN
	stack := RCSDeltaList.Cons(delta.next, stack);
      END;
    END;
  END PutDeltas;

PROCEDURE PutDeltaTexts(rf: T; wr: Wr.T)
  RAISES {RCSError.E, Thread.Alerted, Wr.Failure} =
  VAR
    delta: RCSDelta.T;
    stack: RCSDeltaList.T := NIL;
    children: RCSDeltaList.T := NIL;
    iter: RCSDelta.Iterator;
    branch: RCSDelta.T;
  BEGIN
    (* Here again, we use pre-order to output the delta texts.  But when
       a node has multiple children (i.e., there are branches hanging
       off of it), we have to be careful about their relative order.  We
       want the newest children to come first, so we have to push them
       on the stack oldest-first.

       Again here we use a stack algorithm rather than recursion, to
       guard against possible thread stack overflow. *)
    IF rf.head # NIL THEN
      stack := RCSDeltaList.Cons(rf.head, stack);
    END;
    WHILE stack # NIL DO
      (* Pop top delta from stack. *)
      delta := stack.head;
      stack := stack.tail;

      (* Emit the delta text. *)
      Wr.PutText(wr, "\n\n" & delta.revision & "\nlog\n");
      PutString(wr, RCSDelta.GetLog(delta).iterate());
      PutPhrases(wr, delta.textPhrases);
      Wr.PutText(wr, "text\n");
      PutString(wr, RCSDelta.GetText(delta, delta.prev));

      (* Push the children oldest-first.  There is a wrinkle here.
	 We have encountered strange RCS files in which there is
	 a revision 1.1 whose date says it is newer than revision
	 3.0.  This "cannot" have happened, since revision 3.0 is
	 derived from 1.1.  These RCS files were probably created
	 by some sort of hackery, but we would nevertheless like
	 to handle them properly.  To do that, we maintain that
	 any node on the main branch is by definition older than
	 its "prev" node, which is in turn older than any other
	 children (branches). *)

      IF delta.next # NIL THEN
	IF RCSRevNum.IsTrunk(delta.revision) THEN  (* Oldest by definition. *)
	  stack := RCSDeltaList.Cons(delta.next, stack);
	ELSE  (* Handle it like the other children. *)
	  children := RCSDeltaList.Cons(delta.next, children);
	END;
      END;
      iter := RCSDelta.IterateBranches(delta);
      WHILE iter.next(branch) DO
	children := RCSDeltaList.Cons(branch, children);
      END;
      children := RCSDeltaListSort.SortD(children, CompByDate);
      WHILE children # NIL DO
	stack := RCSDeltaList.Cons(children.head, stack);
	children := children.tail;
      END;
    END;
  END PutDeltaTexts;

PROCEDURE PutDesc(rf: T; wr: Wr.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    Wr.PutText(wr, "\n\ndesc\n");
    PutString(wr, rf.desc.iterate());
    IF Option.ExtraLineAfterDesc IN rf.options THEN
      Wr.PutChar(wr, '\n');
    END;
  END PutDesc;

PROCEDURE PutEscaped(wr: Wr.T; t: TEXT)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    atPos := Text.FindChar(t, '@');
    start: CARDINAL;
  BEGIN
    IF atPos = -1 THEN  (* The usual case, no '@' characters. *)
      Wr.PutText(wr, t);
    ELSE  (* There are some '@' characters that we have to double. *)
      start := 0;
      REPEAT
	Wr.PutText(wr, Text.Sub(t, start, atPos + 1 - start));  (* Thru '@' *)
	start := atPos;  (* Will get the '@' again. *)
	atPos := Text.FindChar(t, '@', atPos + 1);
      UNTIL atPos = -1;
      Wr.PutText(wr, Text.Sub(t, start));
    END;
  END PutEscaped;

PROCEDURE PutPhrase(wr: Wr.T; phrase: RCSPhrase.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    iter := RCSPhrase.IterateWords(phrase);
    word: TEXT;
    isString: BOOLEAN;
  BEGIN
    Wr.PutText(wr, RCSPhrase.GetKey(phrase));
    IF iter.next(word, isString) THEN
      LOOP
	Wr.PutChar(wr, '\t');
	IF isString THEN
	  Wr.PutChar(wr, '@'); PutEscaped(wr, word); Wr.PutChar(wr, '@');
	ELSE
	  Wr.PutText(wr, word);
	END;
	IF NOT iter.next(word, isString) THEN EXIT END;
	IF NOT isString AND Text.Equal(word, ":") THEN
	  (* Collapse the common form "word:word" onto a single line. *)
	  Wr.PutChar(wr, ':');
	  IF NOT iter.next(word, isString) THEN EXIT END;
	  IF isString THEN
	    Wr.PutChar(wr, '@'); PutEscaped(wr, word); Wr.PutChar(wr, '@');
	  ELSE
	    Wr.PutText(wr, word);
	  END;
	  IF NOT iter.next(word, isString) THEN EXIT END;
	END;
	Wr.PutText(wr, "\n");
      END;
    END;
    Wr.PutText(wr, ";\n");
  END PutPhrase;

PROCEDURE PutPhrases(wr: Wr.T; phrases: RCSPhrases.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    iter: RCSPhrases.Iterator;
    phrase: RCSPhrase.T;
  BEGIN
    IF phrases # NIL THEN
      iter := RCSPhrases.Iterate(phrases);
      WHILE iter.next(phrase) DO
	PutPhrase(wr, phrase);
      END;
    END;
  END PutPhrases;

PROCEDURE PutString(wr: Wr.T; iter: RCSString.Iterator)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    line: RCSString.T;
  BEGIN
    Wr.PutChar(wr, '@');
    WHILE iter.next(line) DO
      PutEscaped(wr, line.toText());
    END;
    Wr.PutText(wr, "@\n");
  END PutString;

PROCEDURE CompByDate(d1, d2: RCSDelta.T): [-1..1] =
  VAR
    c: [-1..1];
  BEGIN
    c := RCSDate.Compare(d1.date, d2.date);

    (* It has occurred that revisions 1.1 and 1.1.1.1 had exactly the
       same date, and 1.1.1.1 happened to come out first from a sort.
       We rely on branches coming out before their respective branch
       points.  To prevent problems, we break date ties by comparing
       revision numbers. *)
    IF c = 0 THEN
      c := RCSRevNum.Compare(d1.revision, d2.revision);
    END;
    RETURN c;
  END CompByDate;

(*****************************************************************************)
(* "NewPhrases" support *)
(*****************************************************************************)

PROCEDURE IteratePhrases(rf: T): RCSPhrases.Iterator =
  BEGIN
    RETURN RCSPhrases.Iterate(rf.newPhrases);
  END IteratePhrases;

PROCEDURE AddPhrase(rf: T; phrase: RCSPhrase.T) =
  BEGIN
    IF rf.newPhrases = NIL THEN
      rf.newPhrases := RCSPhrases.New();
    END;
    RCSPhrases.Append(rf.newPhrases, phrase);
  END AddPhrase;

PROCEDURE DeletePhrases(rf: T) =
  BEGIN
    rf.newPhrases := NIL;
  END DeletePhrases;

(*****************************************************************************)
(* Iteration support *)
(*****************************************************************************)

TYPE
  AccessIteratorImpl = AccessIterator OBJECT
    cur: RCSAccessList.T;
  OVERRIDES
    next := NextAccess;
  END;

  TagIteratorImpl = TagIterator OBJECT
    cur: RCSTagList.T;
  OVERRIDES
    next := NextTag;
  END;

PROCEDURE IterateByNumber(rf: T; up: BOOLEAN := TRUE): RCSDeltaTbl.Iterator =
  BEGIN
    RETURN rf.deltaTbl.iterateOrdered(up);
  END IterateByNumber;

PROCEDURE IterateAccess(rf: T): AccessIterator =
  BEGIN
    RETURN NEW(AccessIteratorImpl, cur := rf.accessList);
  END IterateAccess;

PROCEDURE IterateTags(rf: T): TagIterator =
  BEGIN
    RETURN NEW(TagIteratorImpl, cur := rf.tagList);
  END IterateTags;

PROCEDURE IterateTagsByName(rf: T): TagIterator =
  BEGIN
    RETURN NEW(TagIteratorImpl, cur := RCSTagListSort.Sort(rf.tagList));
  END IterateTagsByName;

PROCEDURE NextAccess(iter: AccessIteratorImpl; VAR access: RCSAccess.T): BOOLEAN =
  BEGIN
    IF iter.cur = NIL THEN RETURN FALSE END;
    access := iter.cur.head;
    iter.cur := iter.cur.tail;
    RETURN TRUE;
  END NextAccess;

PROCEDURE NextTag(iter: TagIteratorImpl; VAR tag: RCSTag.T): BOOLEAN =
  BEGIN
    IF iter.cur = NIL THEN RETURN FALSE END;
    tag := iter.cur.head;
    iter.cur := iter.cur.tail;
    RETURN TRUE;
  END NextTag;

(*****************************************************************************)
(* Options support *)
(*****************************************************************************)

PROCEDURE EncodeOptions(options: Options): TEXT =
  VAR
    flags: Word.T := 0;
  BEGIN
    FOR o := FIRST(Option) TO LAST(Option) DO
      IF o IN options THEN
	flags := Word.Or(flags, Word.LeftShift(1, ORD(o)));
      END;
    END;
    RETURN Fmt.Unsigned(flags, 10);
  END EncodeOptions;

PROCEDURE DecodeOptions(text: TEXT): Options
  RAISES {RCSError.E} =
  VAR
    options := Options{};
  BEGIN
    TRY
      WITH flags = TokScan.AtoI(text) DO
	FOR o := FIRST(Option) TO LAST(Option) DO
	  IF Word.And(flags, Word.LeftShift(1, ORD(o))) # 0 THEN
	    options := options + Options{o};
	  END;
	END;
      END;
      RETURN options;
    EXCEPT TokScan.Error =>
      RAISE RCSError.E("Invalid RCSFile option encoding");
    END;
  END DecodeOptions;

(*****************************************************************************)
(* Low level parsing routines *)
(*****************************************************************************)

PROCEDURE EatTok(rf: T) =
  BEGIN
    EVAL CurTok(rf);
    rf.curTok.type := TokType.None;
  END EatTok;

PROCEDURE EatType(rf: T; type: TokType) RAISES {RCSError.E} =
  BEGIN
    NeedType(rf, type);
    EatTok(rf);
  END EatType;

PROCEDURE EatKeyword(rf: T; key: Keyword) RAISES {RCSError.E} =
  BEGIN
    NeedKeyword(rf, key);
    EatTok(rf);
  END EatKeyword;

PROCEDURE NeedType(rf: T; type: TokType) RAISES {RCSError.E} =
  BEGIN
    IF NOT HaveType(rf, type) THEN
      Oops(rf, "\"" & TokTypeName(type) & "\" expected");
    END;
  END NeedType;

PROCEDURE NeedKeyword(rf: T; key: Keyword) RAISES {RCSError.E} =
  BEGIN
    IF NOT HaveKeyword(rf, key) THEN
      Oops(rf, "\"" & KeywordName(key) & "\" expected");
    END;
  END NeedKeyword;

PROCEDURE HaveType(rf: T; type: TokType): BOOLEAN =
  BEGIN
    RETURN CurTok(rf).type = type;
  END HaveType;

PROCEDURE HaveKeyword(rf: T; key: Keyword): BOOLEAN =
  BEGIN
    RETURN HaveType(rf, TokType.Id) AND
      CurTok(rf).keyword = key;
  END HaveKeyword;

PROCEDURE CurTok(rf: T): Token =
  BEGIN
    IF rf.curTok.type = TokType.None THEN
      GetToken(rf);
    END;
    RETURN rf.curTok;
  END CurTok;

PROCEDURE Oops(rf: T; msg: TEXT) RAISES {RCSError.E} =
  BEGIN
    RAISE RCSError.E(Fmt.Int(rf.line) & ": " & msg);
  END Oops;

(*****************************************************************************)
(* The "Token" type. *)
(*****************************************************************************)

TYPE
  Token = REF RECORD
    type: TokType := TokType.None;
    keyword: Keyword := Keyword.None;
    line: CARDINAL;
    ptr: UNTRACED REF CHAR;
    len: CARDINAL;
  END;

  TokType = {
    Colon,
    Id,		(* also includes Sym *)
    Num,
    Semicolon,
    String,

    Bad,

    EOF,

    None
  };

  Keyword = {
    Access,
    Author,
    Branch,
    Branches,
    Comment,
    Date,
    Desc,
    Expand,
    Head,
    Locks,
    Log,
    Next,
    State,
    Strict,
    Symbols,
    Text,

    None
  };

PROCEDURE KeywordName(key: Keyword): TEXT =
  BEGIN
    CASE key OF
    | Keyword.Access	=> RETURN "access";
    | Keyword.Author	=> RETURN "author";
    | Keyword.Branch	=> RETURN "branch";
    | Keyword.Branches	=> RETURN "branches";
    | Keyword.Comment	=> RETURN "comment";
    | Keyword.Date	=> RETURN "date";
    | Keyword.Desc	=> RETURN "desc";
    | Keyword.Expand	=> RETURN "expand";
    | Keyword.Head	=> RETURN "head";
    | Keyword.Locks	=> RETURN "locks";
    | Keyword.Log	=> RETURN "log";
    | Keyword.Next	=> RETURN "next";
    | Keyword.State	=> RETURN "state";
    | Keyword.Strict	=> RETURN "strict";
    | Keyword.Symbols	=> RETURN "symbols";
    | Keyword.Text	=> RETURN "text";
    | Keyword.None	=> RETURN "none";
    END;
  END KeywordName;

PROCEDURE TokText(tok: Token): TEXT =
  BEGIN
    IF tok.type = TokType.String THEN
      RETURN CText.CopyQuotedMtoT(tok.ptr, tok.len);
    ELSE
      RETURN CText.CopyMtoT(tok.ptr, tok.len);
    END;
  END TokText;

PROCEDURE TokTypeName(type: TokType): TEXT =
  BEGIN
    CASE type OF
    | TokType.Colon		=> RETURN "Colon";
    | TokType.Id		=> RETURN "Id";
    | TokType.Num		=> RETURN "Num";
    | TokType.Semicolon		=> RETURN "Semicolon";
    | TokType.String		=> RETURN "String";
    | TokType.Bad		=> RETURN "Bad";
    | TokType.EOF		=> RETURN "EOF";
    | TokType.None		=> RETURN "None";
    END;
  END TokTypeName;

(*****************************************************************************)
(* The "String" type. *)
(*****************************************************************************)

TYPE
  (* Base classes with common portions of the implementation. *)

  String = RCSString.T OBJECT
    ptr: UNTRACED REF CHAR;
    len: CARDINAL;
  OVERRIDES
    numLines := StrNumLines;
    toText := NIL;	(* Must be overridden. *)
    iterate := NIL;	(* Must be overridden. *)
  END;

  StringIter = RCSString.Iterator OBJECT
    ptr: UNTRACED REF CHAR;
    lim: UNTRACED REF CHAR;
  OVERRIDES
    next := NIL;	(* Must be overridden. *)
  END;

PROCEDURE StrNumLines(s: String): CARDINAL =
  VAR
    ptr := s.ptr;
    lim := s.ptr + s.len;
    numLines: CARDINAL := 0;
  BEGIN
    WHILE ptr < lim DO
      INC(numLines);
      WHILE ptr < lim AND ptr^ # '\n' DO  (* Find the next newline. *)
	INC(ptr);
      END;
      IF ptr = lim THEN EXIT END;
      INC(ptr);
    END;
    RETURN numLines;
  END StrNumLines;

(*****************************************************************************)
(* Specializations for simple, unquoted strings. *)
(*****************************************************************************)

TYPE
  SimpleString = String OBJECT OVERRIDES
    toText := SSToText;
    iterate := SSIterate;
  END;

  SimpleStringIter = StringIter OBJECT OVERRIDES
    next := SSNext;
  END;

PROCEDURE SSToText(s: SimpleString): TEXT =
  BEGIN
    RETURN CText.CopyMtoT(s.ptr, s.len);
  END SSToText;

PROCEDURE SSIterate(s: SimpleString): RCSString.Iterator =
  BEGIN
    RETURN NEW(SimpleStringIter, ptr := s.ptr, lim := s.ptr + s.len);
  END SSIterate;

PROCEDURE SSNext(iter: SimpleStringIter; VAR line: RCSString.T): BOOLEAN =
  VAR
    start := iter.ptr;
  BEGIN
    IF iter.ptr >= iter.lim THEN
      RETURN FALSE;
    END;

    WHILE iter.ptr < iter.lim AND iter.ptr^ # '\n' DO
      INC(iter.ptr);
    END;
    IF iter.ptr < iter.lim THEN  (* Include the newline too *)
      INC(iter.ptr);
    END;
    line := NEW(SimpleString, ptr := start, len := iter.ptr-start);
    RETURN TRUE;
  END SSNext;

(*****************************************************************************)
(* Specializations for strings in which "@" characters are doubled. *)
(*****************************************************************************)

TYPE
  QuotedString = String OBJECT OVERRIDES
    toText := QSToText;
    iterate := QSIterate;
  END;

  QuotedStringIter = StringIter OBJECT OVERRIDES
    next := QSNext;
  END;

PROCEDURE QSToText(s: QuotedString): TEXT =
  BEGIN
    RETURN CText.CopyQuotedMtoT(s.ptr, s.len);
  END QSToText;

PROCEDURE QSIterate(s: QuotedString): RCSString.Iterator =
  BEGIN
    RETURN NEW(QuotedStringIter, ptr := s.ptr, lim := s.ptr + s.len);
  END QSIterate;

PROCEDURE QSNext(iter: QuotedStringIter; VAR line: RCSString.T): BOOLEAN =
  VAR
    start := iter.ptr;
  BEGIN
    IF iter.ptr >= iter.lim THEN
      RETURN FALSE;
    END;

    WHILE iter.ptr < iter.lim AND iter.ptr^ # '\n' DO
      INC(iter.ptr);
    END;
    IF iter.ptr < iter.lim THEN  (* Include the newline too *)
      INC(iter.ptr);
    END;
    line := NEW(QuotedString, ptr := start, len := iter.ptr-start);
    RETURN TRUE;
  END QSNext;

(*****************************************************************************)

BEGIN
  FOR key := FIRST(Keyword) TO LAST(Keyword) DO
    IF key # Keyword.None THEN
      EVAL keyTab.put(KeywordName(key), ORD(key));
    END;
  END;
END RCSFile.
