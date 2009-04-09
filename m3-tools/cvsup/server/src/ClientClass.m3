(* Copyright 2000-2001 Olaf Wagner.
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
 *      This product includes software developed by Olaf Wagner.
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
 * $Id: ClientClass.m3,v 1.1.1.1 2009-04-09 17:01:46 jkrell Exp $ *)

MODULE ClientClass;

IMPORT
  ErrMsg, FileRd, GlobTree, Logger, OSError, OSErrorPosix, Rd, RegEx,
  SupMisc, Stdio, Text, TextGlobTreeTbl, TextRefTbl, TextTextTbl,
  Thread, TokScan, Uerror, Wr;

(*---------------------------------------------------------------------------*)
TYPE
  ClassOpt = {MaxConnections, Collections, CollectionDirs, 
              Branches, BranchesOfCollection,
              Tags, TagsOfCollection, Default};

  ClassOpts = SET OF ClassOpt;

(*---------------------------------------------------------------------------*)
REVEAL
  (*=========================================================================*)
  (* ClientClass.T                                                           *)
  (*=========================================================================*)

  T = Public BRANDED "CVSup ClientClass 1.0" OBJECT
    opts: ClassOpts := ClassOpts{};
    collectionDirs: GlobTree.T;
    collections: GlobTree.T;
    partiallyHiddenCollections: GlobTree.T;
    allowedBranches : GlobTree.T;
    allowedCollectionBranches: TextGlobTreeTbl.T;
    allowedTags: GlobTree.T;
    allowedCollectionTags: TextGlobTreeTbl.T;
    default: TEXT;
  OVERRIDES
    init := Init;
    initDefaultFree := InitDefaultFree;
    inAllowedCollectionDirs := InAllowedCollectionDirs;
    inAllowedCollections := InAllowedCollections;
    inAllowedCollectionBranches := InAllowedCollectionBranches;
    inAllowedCollectionTags := InAllowedCollectionTags;
    inPartiallyHiddenCollections := InPartiallyHiddenCollections;
    collectionIsPartiallyHidden := CollectionIsPartiallyHidden;
  END;

(*---------------------------------------------------------------------------*)
CONST
  DefaultDefault = "default";
(*---------------------------------------------------------------------------*)
VAR (* CONST *)
  CommentPattern   := RegEx.Compile("^[ \t]*#.*$"); <* NOWARN *>
  ContLinePattern  := RegEx.Compile(".*\\\\$"); <* NOWARN *>
  EmptyLinePattern := RegEx.Compile("^[ \t]*$"); <* NOWARN *>
  EqualsPattern    := RegEx.Compile("="); <* NOWARN *>
  SlashPattern     := RegEx.Compile("/"); <* NOWARN *>
  AllPattern       := RegEx.Compile(".*"); <* NOWARN *>

  EnoentAtom := OSErrorPosix.ErrnoAtom(Uerror.ENOENT);

(*---------------------------------------------------------------------------*)
PROCEDURE D(msg : TEXT) =
  BEGIN
    TRY
      Wr.PutText(Stdio.stdout, msg & "\n");
    EXCEPT
    ELSE
    END;
  END D;

(*---------------------------------------------------------------------------*)
PROCEDURE IsComment(line : TEXT) : BOOLEAN =
  VAR
  BEGIN
    RETURN RegEx.Execute(CommentPattern, line) > -1;
  END IsComment;

(*---------------------------------------------------------------------------*)
PROCEDURE IsEmptyLine(line : TEXT) : BOOLEAN =
  VAR
  BEGIN
    RETURN RegEx.Execute(EmptyLinePattern, line) > -1;
  END IsEmptyLine; 

(*---------------------------------------------------------------------------*)
PROCEDURE LineContinued(line : TEXT) : BOOLEAN =
  VAR
  BEGIN
    RETURN RegEx.Execute(ContLinePattern, line) > -1;
  END LineContinued;

(*---------------------------------------------------------------------------*)
PROCEDURE IsDefinition(line : TEXT) : BOOLEAN =
  VAR
  BEGIN
    RETURN RegEx.Execute(EqualsPattern, line) > -1;
  END IsDefinition; 

(*---------------------------------------------------------------------------*)
PROCEDURE ContainsAttributes(line : TEXT) : BOOLEAN =
  VAR
  BEGIN
    RETURN RegEx.Execute(SlashPattern, line) > -1;
  END ContainsAttributes; 

(*---------------------------------------------------------------------------*)
PROCEDURE TextUpTo(src: TEXT; pat: RegEx.Pattern) : TEXT =
  VAR i := RegEx.Execute(pat, src);
  BEGIN
    IF i < 0 THEN
      RETURN src;
    ELSIF i = 0 THEN
      RETURN "";
    ELSE
      RETURN Text.Sub(src, 0, i);
    END;
  END TextUpTo;

(*---------------------------------------------------------------------------*)
PROCEDURE TextAfter(src: TEXT; pat: RegEx.Pattern; len : CARDINAL) : TEXT =
  VAR 
    i := RegEx.Execute(pat, src);
  BEGIN
    IF i < 0 THEN
      RETURN "";
    ELSE
      RETURN Text.Sub(src, i + len);
    END;
  END TextAfter; 

(*---------------------------------------------------------------------------*)
PROCEDURE Init(self: T; rd: Rd.T): T
  RAISES {Error, Rd.Failure, Thread.Alerted} =
  VAR
    line, entry: TEXT := NIL;
  BEGIN
    TRY
      line := Rd.GetLine(rd);
      WHILE IsComment(line) OR IsEmptyLine(line) DO
        line := Rd.GetLine(rd);
      END;
      entry := "";
      WHILE LineContinued(line) DO
        entry := entry & Text.Sub(line, 0, Text.Length(line) - 1);
        line := Rd.GetLine(rd);
      END;
      entry := entry & line;
      IF NOT IsEmptyLine(entry) THEN
        ScanEntry(self, entry);
      END;
    EXCEPT
      Rd.EndOfFile => RETURN NIL;
    END;
    RETURN self;
  END Init;

(*---------------------------------------------------------------------------*)
PROCEDURE InitDefaultFree(self: T): T =
  BEGIN
    MakeUnrestricted(self);
    self.name := DefaultDefault;
    self.opts := ClassOpts{ClassOpt.MaxConnections, ClassOpt.Collections,
      ClassOpt.CollectionDirs, ClassOpt.Branches, ClassOpt.Tags};
    RETURN self;
  END InitDefaultFree;

(*---------------------------------------------------------------------------*)
PROCEDURE MakeUnrestricted(self: T) =
  BEGIN
    self.maxConnections := LAST(INTEGER);
    self.collectionDirs := GlobTree.True;
    self.collections := GlobTree.True;
    self.partiallyHiddenCollections := GlobTree.False;
    self.allowedBranches := GlobTree.True;
    self.allowedCollectionBranches := NEW(TextGlobTreeTbl.Default).init();
    self.allowedTags := GlobTree.True;
    self.allowedCollectionTags := NEW(TextGlobTreeTbl.Default).init();
    self.default := NIL;
  END MakeUnrestricted;

(*---------------------------------------------------------------------------*)
PROCEDURE ScanEntry(self: T; entry: TEXT)
  RAISES {Error} =
  CONST myName = "ScanEntry: ";
  VAR 
    ts := TokScan.New(entry, SET OF CHAR{':'});
  BEGIN
    IF debugLevel > 0 THEN
      D(myName & "\"" & entry & "\"");
    END;
    TRY
      MakeUnrestricted(self);
      self.name := ts.getToken("classname");
      ScanElements(self, ts);
      (* If no default entry was given, use "default". *)
      IF NOT Text.Equal(self.name, DefaultDefault)
      AND NOT ClassOpt.Default IN self.opts THEN
        self.default := DefaultDefault;
        self.opts := self.opts + ClassOpts{ClassOpt.Default};
      END;
    EXCEPT
      TokScan.Error(t) => RAISE Error("syntax error in class file: " & t);
    END;
  END ScanEntry;

(*---------------------------------------------------------------------------*)
PROCEDURE ScanElements(self: T; ts: TokScan.T)
  RAISES { Error, TokScan.Error } =
  CONST myName = "ScanElements: ";
  VAR
    elem: TEXT;
    lhs, rhs, name, coll: TEXT := NIL;
    collBranches := NEW(TextTextTbl.Default).init();
    collTags := NEW(TextTextTbl.Default).init();
    iter: TextTextTbl.Iterator;
  BEGIN
    WHILE ts.next(elem) DO
      IF debugLevel > 0 THEN
        D(myName & "\"" & elem & "\"");
      END;
      IF IsDefinition(elem) THEN
        lhs := TextUpTo(elem, EqualsPattern);
        rhs := TextAfter(elem, EqualsPattern, 1);
        IF ContainsAttributes(elem) THEN
          name := TextUpTo(lhs, SlashPattern);
          coll := TextAfter(lhs, SlashPattern, 1);
          IF debugLevel > 0 THEN
            D(myName & name & ", collection " & coll & ": " & rhs);
          END;
        ELSE
          name := lhs;
          IF debugLevel > 0 THEN
            D(myName & name & ": " & rhs);
          END;
        END;
        IF Text.Equal(name, "maxconnections") THEN
          self.maxConnections := TokScan.AtoI(rhs, "maxconnections");
          self.opts := self.opts + ClassOpts{ClassOpt.MaxConnections};
        ELSIF Text.Equal(name, "collection-dirs") THEN
          self.collectionDirs := ScanList(rhs);
          self.opts := self.opts + ClassOpts{ClassOpt.CollectionDirs};
        ELSIF Text.Equal(name, "collections") THEN
          self.collections := ScanList(rhs);
          self.opts := self.opts + ClassOpts{ClassOpt.Collections};
        ELSIF Text.Equal(name, "partially-hidden-collections") OR
          Text.Equal(name, "opaque-collections") THEN
          self.partiallyHiddenCollections := ScanList(rhs);
          self.opts := self.opts + ClassOpts{ClassOpt.Collections};
        ELSIF Text.Equal(name, "tags") THEN
          IF coll = NIL THEN
            self.allowedTags := ScanList(rhs);
            self.opts := self.opts + ClassOpts{ClassOpt.Tags};
          ELSE
            (* Save the right-hand side in text form, because we may
               not have parsed the default allowed tags list yet. *)
            EVAL collTags.put(coll, rhs);
            self.opts := self.opts + ClassOpts{ClassOpt.TagsOfCollection};
          END;
        ELSIF Text.Equal(name, "branches") THEN
          IF coll = NIL THEN
            self.allowedBranches := ScanList(rhs);
            self.opts := self.opts + ClassOpts{ClassOpt.Branches};
          ELSE
            (* Save the right-hand side in text form, because we may
               not have parsed the default allowed branches list yet. *)
            EVAL collBranches.put(coll, rhs);
            self.opts := self.opts + 
                             ClassOpts{ClassOpt.BranchesOfCollection};
          END;
        ELSIF Text.Equal(name, "default") THEN
          self.default := rhs;
          self.opts := self.opts + ClassOpts{ClassOpt.Default};
        ELSE
          RAISE Error("unknown keyword \"" & name & "\" in class file");
        END;
      ELSE
        (* possibly handle boolean/flag parameters, for now just *)
        IF NOT IsEmptyLine(elem) THEN
          RAISE Error("no `=' in class spec: " & elem);
        END;
      END;
    END;
    (* Now that we have all of the elements, parse the per-collection
       branches and tags overrides. *)
    iter := collBranches.iterate();
    WHILE iter.next(coll, rhs) DO
      EVAL self.allowedCollectionBranches.put(coll,
        ScanList(rhs, self.allowedBranches));
    END;
    iter := collTags.iterate();
    WHILE iter.next(coll, rhs) DO
      EVAL self.allowedCollectionTags.put(coll,
        ScanList(rhs, self.allowedTags));
    END;
  END ScanElements;

(*---------------------------------------------------------------------------*)
PROCEDURE ScanList(elem: TEXT; initialList: GlobTree.T := NIL) : GlobTree.T
  RAISES {Error} =
  VAR
    act :  TEXT := NIL;
    res :  GlobTree.T;
    ts  := TokScan.New(elem, SET OF CHAR{','});
  BEGIN
    res := initialList;
    TRY
      WHILE ts.next(act) DO
        IF Text.Length(act) > 0 AND Text.GetChar(act, 0) = '!' THEN
          act := Text.Sub(act, 1);
          IF res = NIL THEN res := GlobTree.True END;
          res := GlobTree.And(res, GlobTree.Not(SupMisc.PatternMatch(act)));
        ELSE
          IF res = NIL THEN res := GlobTree.False END;
          res := GlobTree.Or(res, SupMisc.PatternMatch(act));
        END;
      END;
    EXCEPT
      TokScan.Error(t) => 
      RAISE Error("syntax error in value list in class file: " & t);
    | RegEx.Error(t) =>
      RAISE Error("regex syntax error in value list in class file: " & t);
    END;
    RETURN res;
  END ScanList;

(*---------------------------------------------------------------------------*)
PROCEDURE InAllowedCollectionDirs(self: T; collDir : TEXT): BOOLEAN =
  BEGIN
    <* ASSERT self # NIL AND self.collectionDirs # NIL *>
    RETURN self.collectionDirs.test(collDir);
  END InAllowedCollectionDirs;

(*---------------------------------------------------------------------------*)
PROCEDURE InAllowedCollections(self: T; coll : TEXT): BOOLEAN =
  BEGIN
    <* ASSERT self # NIL AND self.collections # NIL *>
    RETURN self.collections.test(coll);
  END InAllowedCollections;

(*---------------------------------------------------------------------------*)
PROCEDURE GetAllowedCollectionBranches(self: T; coll : TEXT): GlobTree.T =
  VAR patlist: GlobTree.T;
  BEGIN
    <* ASSERT self # NIL AND self.allowedCollectionBranches # NIL *>
    IF NOT self.allowedCollectionBranches.get(coll, patlist) THEN
      patlist := self.allowedBranches;
    END;
    RETURN patlist;
  END GetAllowedCollectionBranches;

(*---------------------------------------------------------------------------*)
PROCEDURE InAllowedCollectionBranches(self: T; coll, branch : TEXT): BOOLEAN =
  BEGIN
    RETURN GetAllowedCollectionBranches(self, coll).test(branch);
  END InAllowedCollectionBranches;

(*---------------------------------------------------------------------------*)
PROCEDURE GetAllowedCollectionTags(self: T; coll : TEXT): GlobTree.T =
  VAR patlist: GlobTree.T;
  BEGIN
    <* ASSERT self # NIL AND self.allowedCollectionTags # NIL *>
    IF NOT self.allowedCollectionTags.get(coll, patlist) THEN
      patlist := self.allowedTags;
    END;
    RETURN patlist;
  END GetAllowedCollectionTags;

(*---------------------------------------------------------------------------*)
PROCEDURE InAllowedCollectionTags(self: T; coll, tag : TEXT): BOOLEAN =
  BEGIN
    RETURN GetAllowedCollectionTags(self,coll).test(tag);
  END InAllowedCollectionTags;

(*---------------------------------------------------------------------------*)
PROCEDURE InPartiallyHiddenCollections(self: T; coll : TEXT): BOOLEAN =
  BEGIN
    <* ASSERT self # NIL AND self.partiallyHiddenCollections # NIL *>
    RETURN self.partiallyHiddenCollections.test(coll);
  END InPartiallyHiddenCollections;

(*---------------------------------------------------------------------------*)
PROCEDURE CollectionIsPartiallyHidden(self: T; coll : TEXT): BOOLEAN =
  BEGIN
    RETURN self.inPartiallyHiddenCollections(coll) OR
      GetAllowedCollectionTags(self, coll) # GlobTree.True OR
      GetAllowedCollectionBranches(self, coll) # GlobTree.True;
  END CollectionIsPartiallyHidden; 


REVEAL
  (*=========================================================================*)
  (* ClientClass.DB                                                          *)
  (*=========================================================================*)

  DB = DBPublic BRANDED "CVSup ClientClassDB 0.0" OBJECT
    tab: TextRefTbl.T := NIL;
  METHODS
  OVERRIDES
    init := DBInit;
    initFromRd := DBInitFromRd;
    getClass := GetClass;
  END;

(*---------------------------------------------------------------------------*)
PROCEDURE DBInit(self : DB; fn : TEXT; logger : Logger.T := NIL): DB
  RAISES {Rd.Failure, Thread.Alerted} =
  VAR 
    rd : Rd.T;
  BEGIN
    TRY
      rd := FileRd.Open(fn);
      TRY
        self := DBInitFromRd(self, rd, logger);
      FINALLY
        Rd.Close(rd);
      END;
    EXCEPT OSError.E(list) =>
      IF list.head # EnoentAtom THEN
        Log(logger, Logger.Priority.Warning,
          "Cannot open \"" & fn & "\": " & ErrMsg.StrError(list));
      END;
      MakeDBFree(self);
    END;
    RETURN self;
  END DBInit;

(*---------------------------------------------------------------------------*)
PROCEDURE DBInitFromRd(self : DB; rd: Rd.T; logger : Logger.T := NIL): DB 
  RAISES {Rd.Failure, Thread.Alerted} =
  VAR
    cl, defCl  : T;
    ref        : REFANY;
    numEntries : CARDINAL;
  BEGIN
    <* ASSERT self # NIL *>
    IF self.tab = NIL THEN
      self.tab := NEW(TextRefTbl.Default).init();
    END;
    (* Create a "default" entry, in case the file doesn't provide one. *)
    cl := NEW(T).initDefaultFree();
    EVAL self.tab.put(cl.name, cl);
    (* Process all of the entries in the file. *)
    numEntries := 0;
    WHILE NOT Rd.EOF(rd) DO
      TRY
        cl := NEW(T).init(rd);
        IF cl # NIL THEN
          (* If there is a "default" entry, it must be first in the file. *)
          IF Text.Equal(cl.name, DefaultDefault) THEN
            IF numEntries # 0 THEN
              RAISE Error("client class " & DefaultDefault & " must be" &
                " first entry in the file");
            END;
            (* Delete the "default" entry that we provided. *)
            EVAL self.tab.delete(DefaultDefault, ref);
          END;
          IF ClassOpt.Default IN cl.opts THEN  (* Fill in defaults *)
            defCl := GetClass(self, cl.default);
            IF defCl = NIL THEN
              RAISE Error("client class " & cl.name & " defaults from unknown"
                & " class " & cl.default);  (* XXX - Make this non-fatal *)
            END;
            ResolveDefaults(cl, defCl);
          END;
          IF self.tab.put(cl.name, cl) THEN
            RAISE Error("Duplicate client class entry for " & cl.name);
          END;
          INC(numEntries);
        END;
      EXCEPT Error(msg) =>
        Log(logger, Logger.Priority.Warning, msg);
      END;
    END;
    RETURN self;
  END DBInitFromRd;

(*---------------------------------------------------------------------------*)
PROCEDURE ResolveDefaults(cl, defCl : T) =
  VAR
  BEGIN
    FOR opt := FIRST(ClassOpt) TO LAST(ClassOpt) DO
      IF opt IN defCl.opts THEN
        CASE opt OF
        | ClassOpt.MaxConnections =>
            IF NOT opt IN cl.opts THEN
              cl.maxConnections := defCl.maxConnections;
            END;
        | ClassOpt.Collections =>
            IF NOT opt IN cl.opts THEN
              cl.collections := defCl.collections;
            END;
        | ClassOpt.CollectionDirs =>
            IF NOT opt IN cl.opts THEN
              cl.collectionDirs := defCl.collectionDirs;
            END;
        | ClassOpt.Branches =>
            IF NOT opt IN cl.opts THEN
              cl.allowedBranches := defCl.allowedBranches;
            END;
        | ClassOpt.Tags =>
            IF NOT opt IN cl.opts THEN
              cl.allowedTags := defCl.allowedTags;
            END;
        | ClassOpt.BranchesOfCollection =>
            ResolveTableDefaults(cl.allowedCollectionBranches,
              defCl.allowedCollectionBranches);
        | ClassOpt.TagsOfCollection =>
            ResolveTableDefaults(cl.allowedCollectionTags,
              defCl.allowedCollectionTags);
        | ClassOpt.Default => (* Ignore *)
        END;
        cl.opts := cl.opts + ClassOpts{opt};
      END;
    END;
  END ResolveDefaults;

(*---------------------------------------------------------------------------*)
PROCEDURE ResolveTableDefaults(target, default: TextGlobTreeTbl.T) =
  VAR
    iter    := default.iterate();
    key     : TEXT;
    defVal  : GlobTree.T;
    targVal : GlobTree.T;
  BEGIN
    (* Iterate over the keys defined in the default.  Copy the ones
       which are not defined in the target. *)
    WHILE iter.next(key, defVal) DO
      IF NOT target.get(key, targVal) THEN
        EVAL target.put(key, defVal);
      END;
    END;
  END ResolveTableDefaults;

(*---------------------------------------------------------------------------*)
PROCEDURE GetClass(self : DB; name: TEXT) : T =
  VAR 
    ref : REFANY;
    cl  : T;
  BEGIN
    <* ASSERT self # NIL AND self.tab # NIL *>
    IF self.tab.get(name, ref) THEN
      cl := NARROW(ref, T);
      RETURN cl;
    ELSE
      RETURN NIL;
    END;
  END GetClass;

(*---------------------------------------------------------------------------*)
PROCEDURE FreeAccessDB() : DB =
  VAR
    db := NEW(DB);
  BEGIN
    MakeDBFree(db);
    RETURN db;
  END FreeAccessDB;

(*---------------------------------------------------------------------------*)
PROCEDURE MakeDBFree(db: DB) =
  VAR
    cl := NEW(T).initDefaultFree();
  BEGIN
    db.tab := NEW(TextRefTbl.Default).init();
    EVAL db.tab.put(DefaultDefault, cl);
  END MakeDBFree;

(*---------------------------------------------------------------------------*)
PROCEDURE Log(logger   : Logger.T;
              priority : Logger.Priority;
              msg      : TEXT) =
  BEGIN
    IF logger # NIL THEN
      Logger.Put(logger, priority, msg);
    END;
  END Log;

BEGIN
END ClientClass.
