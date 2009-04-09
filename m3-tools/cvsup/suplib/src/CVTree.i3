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

(* The "CVTree" interface provides a facility for traversing a CVS
   repository's directory tree.  It incorporates knowledge of the CVS
   Attic into its traversal.  *)

INTERFACE CVTree;

IMPORT FileAttr, GlobTree, Pathname, Thread;

EXCEPTION Error(TEXT);

TYPE
  FileType = {DirDown, File, AtticFile, DirUp};

  Iterator = OBJECT METHODS
    next(VAR type: FileType;
         VAR name: Pathname.T;
         VAR attr: FileAttr.T): BOOLEAN
      RAISES {Error, Thread.Alerted};
    prune();  (* Stop processing entries at this level. *)
    close()
      RAISES {Error, Thread.Alerted};
  END;

(* The "next" method of an "Iterator" returns "FALSE" if the tree
   traversal has completed.  Otherwise, it passes back information about
   the next file in the traversal via the "VAR" parameters, and returns
   "TRUE".  The "type" parameter is set to indicate the nature of the
   file being processed.  For a regular file, "type" is set to
   "FileType.File".  Each directory is processed twice, first when
   descending into it ("FileType.DirDown"), and again when ascending
   back out of it ("FileType.DirUp").

   The "name" parameter is set to the pathname of the file, relative to
   the root that was passed to "Iterate".

   At any point in the traversal, "prune" may be called.  That
   terminates the processing of the current directory; no further files
   will be returned from it.  After a call to "prune", the following
   call to "next" will return a "type" of "FileType.DirUp", ascending
   out of the current directory. *)

PROCEDURE Iterate(root: Pathname.T;
                  follow: GlobTree.T := NIL;
		  attic: GlobTree.T := NIL): Iterator
  RAISES {Error};
(* Return an Iterator that will traverse the tree rooted at the given
   directory. *)

(* The files within each directory are returned in sorted order by name.
   The sort order is defined by "Text.Compare".

   The "follow" argument specifies a pattern expression.  Symbolic links
   whose names match the pattern will be followed rather than treated
   as links.  The default is equivalent to "GlobTree.True", i.e.,
   follow all links.

   The "attic" argument specifies a pattern expression.  Directories
   whose names match the pattern will be checked for subdirectories
   named "Attic" and processed specially.  The default is equivalent
   to "GlobTree.True", i.e., perform Attic processing in all
   directories. *)

END CVTree.
