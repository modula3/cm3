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
 * $Id: RCSFile.i3,v 1.1.1.1 2009-04-09 17:01:57 jkrell Exp $ *)

(* The "RCSFile" interface provides facilities for reading, parsing,
   modifying, and writing RCS files. *)

INTERFACE RCSFile;

IMPORT
  FileAttr, MD5, OSError, Pathname, RCSAccess,
  RCSDate, RCSDelta, RCSDeltaTbl,
  RCSError, RCSKeyword, RCSPhrase, RCSPhrases, RCSRevNum, RCSString,
  RCSTag, Thread, Wr;

TYPE
  T <: Public;

  Public = OBJECT
    branch: TEXT := NIL;
    strictLocking := TRUE;
    comment: TEXT := NIL;
    expand := RCSKeyword.ExpandMode.Default;
    desc: RCSString.T;
    options := Options{};
  METHODS
    init(desc: RCSString.T := NIL): T;
  END;

  Options = SET OF Option;

  Option = {
    ExtraLineAfterDesc,	(* Emit an extra blank line after the "desc" string. *)
    CVSInitialImport    (* Variant whitespace from initial CVS import. *)
  };

  AccessIterator = OBJECT METHODS
    next(VAR access: RCSAccess.T): BOOLEAN;
  END;

  TagIterator = OBJECT METHODS
    next(VAR tag: RCSTag.T): BOOLEAN;
  END;

(* An "RCSFile.T" represents an RCS file.  There are three ways to create
   an "RCSFile.T".

   The "init" method yields an empty RCS file, containing no deltas.
   Deltas and tags can subsequently be added to the file.

   The "OpenReadonly" procedure creates an "RCSFile.T" from an existing
   RCS file.

   The "Import" procedure creates an "RCSFile.T" with a single revision
   in it, representing an existing source file. *)

PROCEDURE OpenReadonly(path: Pathname.T): T
  RAISES {OSError.E, RCSError.E};
(* Open the given file read-only, parse its header, and construct
   an "RCSFile.T" from it. *)

PROCEDURE Import(p: Pathname.T;
		 revNum: RCSRevNum.T;
		 author: TEXT;
		 state: TEXT;
		 logLines := -1): T
  RAISES {OSError.E};
(* Import an existing text file, to create a new "RCSFile.T". *)

PROCEDURE ParseDelta(rf: T; delta: RCSDelta.T) RAISES {RCSError.E};
(* Parse the text associated with the given delta. *)

(* If the delta has already been parsed, this procedure does nothing.
   Normally, the user need not call this procedure.  It is called when
   necessary by "RCSDelta.GetText". *)

(* The procedures below retrieve information from the file, without
   modifying anything. *)

PROCEDURE GetDelta(rf: T; revNum: RCSRevNum.T): RCSDelta.T
  RAISES {RCSError.E};
(* Return the delta associated with the given revision number. *)

PROCEDURE GetHeadDelta(rf: T): RCSDelta.T
  RAISES {RCSError.E};
(* Return the head delta on the main branch.  The exception is raised if
   there are no deltas in the file. *)

PROCEDURE GetBranchTip(rf: T; branch: RCSRevNum.T): RCSDelta.T
  RAISES {RCSError.E};
(* Return the delta at the tip of the given branch. *)

PROCEDURE GetTagDelta(rf: T;
                      tag: TEXT := NIL;
		      date: RCSRevNum.T := NIL): RCSDelta.T
  RAISES {RCSError.E};
(* Return the delta associated with the given tag and date. *)

(* Any combination of "tag" and "date" may be specified.  "tag" defaults
   to the RCS file's default branch, and "date" defaults to as late as
   possible.

   The "tag", if given, may represent a specific revision, an RCS
   branch, or a special CVS branch.  If the tag represents a specific
   revision, then that delta is used.  If the tag represents an
   RCS branch, or is a special CVS branch tag, then the delta at
   the tip of the branch is used.

   If "date" is given, then the selection criterion is altered to choose
   a revision created no later than "date".  If "tag" specified an RCS
   branch (or was defaulted), then the selected revision must be a member
   of that branch.  If "tag" specified a CVS branch, then the selected
   revision must be on the branch, or it must be the branch point revision.
   If "tag" specified a specific revision, then the selected revision
   must be precisely that one.

   The exception is raised if no qualifying revision exists. *)

PROCEDURE IterateByNumber(rf: T; up: BOOLEAN := TRUE): RCSDeltaTbl.Iterator;
(* Iterate over all the deltas in order according to their revision
   numbers. *)

(* The revision numbers are compared by to "RCSRevNum.Compare()"; namely,
   lexicographically.  The optional "up" parameter controls the direction
   of the iteration. *)

PROCEDURE IterateAccess(rf: T): AccessIterator;
(* Iterate over all the access names, in the order of their appearance in
   the file. *)

PROCEDURE IterateTags(rf: T): TagIterator;
(* Iterate over all the symbolic tags, in the order of their appearance in
   the file. *)

PROCEDURE IterateTagsByName(rf: T): TagIterator;
(* Iterate over the tags in alphabetical order by name. *)

(* The procedures below perform modifications on the data structure
   representing the RCS file.  Note, these procedures do not alter the
   underlying file itself. *)

PROCEDURE AddTag(rf: T; name: TEXT; revNum: RCSRevNum.T): RCSTag.T
  RAISES {RCSError.E};
(* Add a tag to an already-parsed RCS file.  An exception is raised if
   there is already a tag with the same name and revision number. *)

PROCEDURE DeleteTag(rf: T; name: TEXT; revNum: RCSRevNum.T)
  RAISES {RCSError.E};
(* Delete the given tag.  An execption is raised if the tag does not
   exist. *)

PROCEDURE IteratePhrases(rf: T): RCSPhrases.Iterator;
(* Iterate over all the "newphrases" in the administrative section of
   the RCS file, in their order of appearance. *)

PROCEDURE AddPhrase(rf: T; phrase: RCSPhrase.T);
(* Append the given phrase to the end of the list in the administrative
   section of the RCS file. *)

PROCEDURE DeletePhrases(rf: T);
(* Delete all of the "newphrases" in the administrative section of the
   RCS file. *)

PROCEDURE AddDelta(rf: T;
                   revNum: RCSRevNum.T;
		   diffBase: RCSDelta.T;
		   date: RCSDate.T;
		   author: TEXT;
		   state: TEXT;
		   log: RCSString.T;
		   text: RCSString.T;
		   treePhrases: RCSPhrases.T := NIL;
		   textPhrases: RCSPhrases.T := NIL): RCSDelta.T
  RAISES {RCSError.E};
(* Add a delta to an already-parsed RCS file. *)

(* A delta may only be added to an RCS file that has already been parsed.
   Also, each new delta must be added at the tip of its branch; it is
   not permitted to add a new delta in the middle of a branch.

   The parameter "revNum" is the revision number of the new delta.

   The "text" parameter is the text associated with the new revision.
   With one exception, the delta text is always given in diff form,
   relative to an existing delta, specified by "diffBase".

   When adding a delta to an empty trunk, the "text" obviously cannot
   be in diff form, since there is nothing in the RCS file to which the
   diff could be relative.  In that particular case, the "text" should be
   full text of the revision, rather than a diff, and "diffBase" should
   be "NIL". *)

PROCEDURE DeleteDelta(rf: T;
                      delta: RCSDelta.T)
  RAISES {RCSError.E};
(* Delete a delta from an already-parsed RCS file. *)

(* The deleted delta must be a tip delta, and it must not have any
   branches.  If the delta is the last on its branch, then the entire
   branch is deleted. *)

(* After optionally performing modifications on the "RCSFile.T", you can
   write it out to a new file using the function below. *)

PROCEDURE ToWr(rf: T; wr: Wr.T)
  RAISES {RCSError.E, Thread.Alerted, Wr.Failure};
(* Write out the possibly-updated RCS file to the given writer. *)

(* The following procedures retrieve auxiliary information associated
   with the RCS file.  It is not necessary to parse any of the file
   before using these procedures. *)

PROCEDURE GetAttr(rf: T): FileAttr.T;
(* Return the file attributes associated with the underlying file.  These
   attributes do not reflect any changes that may have been applied to
   the "RCSFile.T". *)

PROCEDURE CalculateMD5(rf: T; md5: MD5.T);
(* Calculate the MD5 checksum of the original, unedited RCS file.  The
   checksum does not include any edits such as added tags or deltas. *)

(* When you are finished with an "RCSFile.T", be sure to close it.
   Otherwise you will cause resource leakage. *)

PROCEDURE Close(rf: T) RAISES {OSError.E};
(* Close the RCS file, and return all resources associated with it. *)

(* The procedures below are for translating between internal forms and
   printable forms. *)

PROCEDURE EncodeOptions(o: Options): TEXT;
(* Produce a printable representation of the given options. *)

PROCEDURE DecodeOptions(t: TEXT): Options RAISES {RCSError.E};
(* Convert a printable representation into options. *)

END RCSFile.
