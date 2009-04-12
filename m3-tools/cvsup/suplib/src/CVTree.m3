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

MODULE CVTree;

IMPORT
  DirEntry, DirEntryList, DirEntryListSort, ErrMsg, FileAttr, FS,
  GlobTree, OSError, Pathname, RefSeq, Scheduler AS SchedulerPosix, SupMisc, Text;

TYPE
  UniIterator = Iterator OBJECT
    path: TEXT;              (* Current directory path. *)
    follow: GlobTree.T;
    attic: GlobTree.T;
    rootLength: CARDINAL;    (* Length of root path. *)
    relPathStart: CARDINAL;  (* Just after the slash following the root. *)
    dirStack: RefSeq.T;      (* Stack of "Dir". *)
    needToReadDir: BOOLEAN;  (* True if we need to read a directory. *)
    dirAttr: FileAttr.T;     (* Attributes of directory we need to read. *)
    pruning := FALSE;
  OVERRIDES
    next := SingleNext;
    prune := SinglePrune;
    close := SingleClose;
  END;

  Dir = REF RECORD
    mainEntries: DirEntryList.T;   (* Entries in main directory. *)
    atticEntries: DirEntryList.T;  (* Entries in Attic subdirectory. *)
    attr: FileAttr.T;              (* Attributes of the directory. *)
  END;

PROCEDURE Iterate(root: Pathname.T;
                  follow: GlobTree.T := NIL;
		  attic: GlobTree.T := NIL): Iterator
  RAISES {Error} =
  VAR
    rootLength := Text.Length(root);
    relPathStart: CARDINAL;
    rootAttr: FileAttr.T;
  BEGIN
    IF follow = NIL THEN follow := GlobTree.True END;
    IF attic = NIL THEN attic := GlobTree.True END;
    IF rootLength = 0
    OR Text.GetChar(root, rootLength-1) = SupMisc.SlashChar THEN
      relPathStart := rootLength;
    ELSE
      relPathStart := rootLength + 1;
    END;
    (* Get the attributes of the root directory. *)
    TRY
      rootAttr := FileAttr.FromPathname(root, follow := follow.test(""));
    EXCEPT OSError.E(l) =>
      RAISE Error("Cannot get attributes for \"" & root & "\": " &
	ErrMsg.StrError(l));
    END;
    RETURN NEW(UniIterator,
      path := root,
      follow := follow,
      attic := attic,
      dirStack := NEW(RefSeq.T).init(30),
      rootLength := rootLength,
      relPathStart := relPathStart,
      needToReadDir := TRUE,
      dirAttr := rootAttr);
  END Iterate;

PROCEDURE SingleNext(iter: UniIterator;
                     VAR type: FileType;
	             VAR name: Pathname.T;
	             VAR attr: FileAttr.T): BOOLEAN
  RAISES {Error} =
  VAR
    mainEntries, atticEntries: DirEntryList.T := NIL;
    errMsg: TEXT := NIL;
    dir: Dir;
    entry: DirEntry.T;
  BEGIN
    IF iter.needToReadDir THEN
      <* ASSERT iter.dirAttr # NIL *>
      IF iter.pruning THEN
	iter.dirStack.addhi(NEW(Dir,
	  mainEntries := NIL,
	  atticEntries := NIL,
	  attr := iter.dirAttr));
      ELSE
	TRY
	  mainEntries := DirEntryListSort.SortD(ReadDir(iter, attic := FALSE));
	EXCEPT Error(msg) =>
	  errMsg := msg;
	END;

	IF iter.attic.test(iter.path) THEN  (* Do Attic processing. *)
	  VAR
	    cur, prev: DirEntryList.T;
	    c: [-1..1];
	  BEGIN
	    cur := mainEntries;
	    prev := NIL;
	    WHILE cur # NIL DO
	      c := Text.Compare(cur.head.name, SupMisc.CVSAttic);
	      IF c >= 0 THEN EXIT END;
	      prev := cur;
	      cur := cur.tail;
	    END;

	    IF cur # NIL AND c = 0
	    AND cur.head.attr.fileType = FileAttr.FileType.Directory THEN
	      (* There is an attic directory. *)
	      TRY
		atticEntries :=
		  DirEntryListSort.SortD(ReadDir(iter, attic := TRUE));
	      EXCEPT Error(msg) =>
		errMsg := msg;
	      END;
	      IF prev = NIL THEN
		mainEntries := cur.tail;
	      ELSE
		prev.tail := cur.tail;
	      END;
	    END;
	  END;
	END;

	iter.dirStack.addhi(NEW(Dir,
	  mainEntries := mainEntries,
	  atticEntries := atticEntries,
	  attr := iter.dirAttr));
      END;
      iter.needToReadDir := FALSE;
      iter.dirAttr := NIL;
    END;

    IF errMsg # NIL THEN
      (* Either the directory or the Attic was unreadable.  Raise an
         exception to report it.  Subsequent calls will continue on
	 as if the unreadable directory were empty. *)
      RAISE Error(errMsg);
    END;

    dir := NARROW(iter.dirStack.gethi(), Dir);

    IF iter.pruning THEN
      dir.mainEntries := NIL;
      dir.atticEntries := NIL;
      iter.pruning := FALSE;
    END;

    REPEAT
      IF dir.mainEntries # NIL AND dir.atticEntries # NIL THEN
	CASE Text.Compare(dir.mainEntries.head.name, dir.atticEntries.head.name)
	OF
	| -1 =>
	    entry := dir.mainEntries.head;
	    type := FileType.File;
	    dir.mainEntries := dir.mainEntries.tail;
	|  0 =>
	    (* The file exists in the Attic and in the main directory.
	       This should not happen in a valid CVS repository.  We simply
	       ignore the Attic copy of the file. *)
	    entry := dir.mainEntries.head;
	    type := FileType.File;
	    dir.mainEntries := dir.mainEntries.tail;
	    dir.atticEntries := dir.atticEntries.tail;
	|  1 =>
	    entry := dir.atticEntries.head;
	    type := FileType.AtticFile;
	    dir.atticEntries := dir.atticEntries.tail;
	END;
      ELSIF dir.mainEntries # NIL THEN
	entry := dir.mainEntries.head;
	type := FileType.File;
	dir.mainEntries := dir.mainEntries.tail;
      ELSIF dir.atticEntries # NIL THEN
	entry := dir.atticEntries.head;
	type := FileType.AtticFile;
	dir.atticEntries := dir.atticEntries.tail;
      ELSE
	entry := NIL;
      END;
    UNTIL entry = NIL
    OR entry.attr.fileType # FileAttr.FileType.Directory
    OR type # FileType.AtticFile;
    (* In the above loop, we skip over any subdirectories of the Attic.
       There should never be any such subdirectories. *)

    IF entry # NIL THEN  (* We have an entry at the current level. *)
      WITH path = SupMisc.CatPath(iter.path, entry.name) DO
	name := Text.Sub(path, iter.relPathStart);
	attr := entry.attr;

	IF attr.fileType = FileAttr.FileType.Directory THEN
	  type := FileType.DirDown;
	  iter.path := path;
	  iter.needToReadDir := TRUE;
	  iter.dirAttr := attr;
	END;

	RETURN TRUE;
      END;
    END;

    IF Text.Length(iter.path) > iter.rootLength THEN  (* Pop up a level. *)
      type := FileType.DirUp;
      name := Text.Sub(iter.path, iter.relPathStart);
      attr := NARROW(iter.dirStack.remhi(), Dir).attr;
      iter.path := SupMisc.PathPrefix(iter.path);
      RETURN TRUE;
    END;

    RETURN FALSE;
  END SingleNext;

PROCEDURE SinglePrune(iter: UniIterator) =
  BEGIN
    iter.pruning := TRUE;
  END SinglePrune;

PROCEDURE SingleClose(<*UNUSED*> iter: UniIterator) =
  BEGIN
  END SingleClose;

PROCEDURE ReadDir(iter: UniIterator;
                  attic: BOOLEAN): DirEntryList.T
  RAISES {Error} =
  VAR
    entries: DirEntryList.T := NIL;
    e := NEW(DirEntry.T);
    fsIter: FS.Iterator;
    path: Pathname.T;
  BEGIN
    IF attic THEN
      path := SupMisc.CatPath(iter.path, SupMisc.CVSAttic);
    ELSE
      path := iter.path;
    END;
    TRY
      fsIter := SafeIterate(path);
    EXCEPT OSError.E(l) =>
      RAISE Error("Cannot read directory \"" & path & "\": " &
	ErrMsg.StrError(l));
    END;
    TRY
      WHILE SafeNext(fsIter, e.name) DO
	WITH filePath = SupMisc.CatPath(path, e.name),
	  relPath = Text.Sub(filePath, iter.relPathStart),
	  follow = iter.follow.test(relPath)
	DO
	  TRY
	    IF follow THEN
	      TRY
		e.attr := FileAttr.FromPathname(filePath, follow := TRUE);
	      EXCEPT OSError.E =>
		(* This may be a dangling or recursive symbolic link.  Try
		   again with "follow" set to "FALSE".  This will give us
		   the attributes of the link itself, just as "ls" does. *)
		e.attr := FileAttr.FromPathname(filePath, follow := FALSE);
	      END;
	    ELSE
	      e.attr := FileAttr.FromPathname(filePath, follow := FALSE);
	    END;
	    entries := DirEntryList.Cons(e, entries);
	    e := NEW(DirEntry.T);
	  EXCEPT OSError.E =>
	    (* If we can't stat it, then pretend it doesn't exist. *)
	  END;
	END;
      END;
    FINALLY
      SafeIterClose(fsIter);
    END;
    RETURN entries;
  END ReadDir;

(*****************************************************************************)
(* FIXME - The following procedures work around a bug in SRC Modula-3 thru
   version 3.6, at least.  The FS.Iterator operations call the Unix
   opendir, readdir, and closedir functions, which in turn usually call
   malloc.  This is done without any mutual exclusion, and malloc is normally
   not thread-safe.  I have observed memory corruption and have traced it
   to this cause.  As a work-around, these wrappers disable thread switching
   while the offending functions are executing. *)
(*****************************************************************************)

PROCEDURE SafeIterate(path: Pathname.T): FS.Iterator
  RAISES {OSError.E} =
  BEGIN
    SchedulerPosix.DisableSwitching();
    TRY
      RETURN FS.Iterate(path);
    FINALLY
      SchedulerPosix.EnableSwitching();
    END;
  END SafeIterate;

PROCEDURE SafeNext(iter: FS.Iterator; VAR name: TEXT): BOOLEAN =
  BEGIN
    SchedulerPosix.DisableSwitching();
    TRY
      RETURN iter.next(name);
    FINALLY
      SchedulerPosix.EnableSwitching();
    END;
  END SafeNext;

PROCEDURE SafeIterClose(iter: FS.Iterator) =
  BEGIN
    SchedulerPosix.DisableSwitching();
    TRY
      iter.close();
    FINALLY
      SchedulerPosix.EnableSwitching();
    END;
  END SafeIterClose;

BEGIN
END CVTree.
