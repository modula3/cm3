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

(* The "SupMisc" interface gathers together various constants and
   procedures that are somehow associated with CVSup and/or CVS. *)

INTERFACE SupMisc;

IMPORT
  Glob, GlobTree, IP, OSError, Pathname, Rd, RegEx, SupFileRec,
  TextSeq, Thread, Wr;
IMPORT TCP;

TYPE
  ExitCode = { Success, TransientFailure, Failure };

  ThreadStatus = REF RECORD  (* Returned by threads when they exit. *)
    status: ExitCode;
    message: TEXT := NIL;
  END;

  TextPredicateClosure = OBJECT
  METHODS
    matches(t: TEXT): BOOLEAN;
  END;

CONST
  Port: IP.Port = 5999;

  CVSAttic = "Attic";
  CVSAdmin = "CVSROOT";
  CVSOptions = CVSAdmin & SlashText & "options";
  RCSSuffix = ",v";

  DefaultClientBase = "/usr/local/etc/cvsup";
  DefaultClientCollDir = "sup";

  DefaultServerBase = "/usr/local/etc/cvsup";
  DefaultServerCollDir = "sup";

  DefaultCompression = 1;  (* Default compression level. *)

  SlashChar = '/';
  SlashText = "/";

  RootDir = SlashText;
  DevNull = "/dev/null";
  ShellPath = "/bin/sh";

  ListenTimeout = 75.0d0;

EXCEPTION
  BadAddress;
  InvalidEscape;

PROCEDURE AtticName(name: Pathname.T): Pathname.T;
(* Returns where the given file would be if it were in the Attic. *)

PROCEDURE IsBlankLine(t: TEXT): BOOLEAN;
(* Checks for a line containing only white space. *)

PROCEDURE Cat3(a, b, c: TEXT): TEXT;
(* Returns the concatenation of the 3 arguments. *)

PROCEDURE CatN(READONLY a: ARRAY OF TEXT): TEXT;
(* Concatenates all the elements of "a" and returns the result. *)

PROCEDURE CatPath(p1, p2: Pathname.T): Pathname.T;
(* Pastes together two parts to make a pathname. *)

PROCEDURE CheckoutName(name: Pathname.T): Pathname.T;
(* Returns the name of the checked-out file corresponding to an RCS file. *)

PROCEDURE CommonLength(a, b: TEXT): CARDINAL;
(* Returns the length of the maximal common leading portion of two texts. *)

PROCEDURE CommonPathLength(a, b: Pathname.T): CARDINAL;
(* Returns the length of the common prefix of two pathnames, on a
   pathname component boundary. *)

PROCEDURE DecodeWS(t: TEXT): TEXT
  RAISES {InvalidEscape};
(* Returns a text with all the escaped characters of the original decoded.
   See "EncodeWS" for the escape conventions. *)

PROCEDURE EncodeWS(t: TEXT): TEXT;
(* Returns a text with all the white space characters of the original
   escaped as follows:

    \_ = space
    \t = tab
    \n = newline
    \r = carriage return
    \\ = backslash
*)

PROCEDURE ExpandFilenames(prefix: Pathname.T;
                          names: TextSeq.T): TextSeq.T;
(* Returns a new list of filenames, with wildcards expanded, and non-existent
   and overlapping files removed. *)

PROCEDURE FindFile(base: Pathname.T;
                   searchPath: TEXT;
		   file: Pathname.T): Pathname.T;
(* Searches each of the directories in the colon-separated "searchPath"
   for "file".  Non-absolute "searchPath" entries are interpreted relative
   to "base".  Returns the resolved pathname, or "NIL" if the file was
   not found.  Always follows symbolic links. *)

PROCEDURE FilterPathList(searchPath: TEXT;
                         cl: TextPredicateClosure): TEXT;
(* Filters a search path with elements separated by colons and returns
   a similar path list with only those elements selected by the
   given text predicate closure. *)

PROCEDURE GetCmdLine(rd: Rd.T): TEXT
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Like "Rd.GetLine" except that it raises "Rd.EndOfFile" if end of
   file is reached before the terminating newline. *)

PROCEDURE IsDirectory(name: Pathname.T): BOOLEAN;
(* Returns "TRUE" if "name" exists and is a directory. *)

PROCEDURE IsRCS(p: Pathname.T): BOOLEAN;
(* Tests whether the given pathname represents an RCS file. *)

PROCEDURE LiveName(name: Pathname.T): Pathname.T;
(* Returns where the given file would be if it were not in the Attic. *)

PROCEDURE MakeDirectories(path: Pathname.T; umask := -1)
  RAISES {OSError.E};
(* Creates any needed directories leading up to the given filename,
   using the given umask or the system's umask value if unspecified.
   It is not an error if the directories already exist. *)

PROCEDURE NewConnector(addr: IP.Address;
                       loPort, hiPort: IP.Port): TCP.Connector
  RAISES {IP.Error};
(* Creates a new connector, using a port in the given inclusive range. *)

PROCEDURE ParseHost(t: TEXT; VAR (*OUT*) res: IP.Address): BOOLEAN
  RAISES {IP.Error};
(* Parses a host name or dotted-quad.  Returns TRUE on success, or
   FALSE if a name was given but it is unknown.  If a name lookup fails
   for some other reason, raises IP.Error with IP.LookupFailure in
   the error list. *)

PROCEDURE ParseIPAddress(t: TEXT; netOK := FALSE): IP.Address
  RAISES {BadAddress};
(* Parses a dotted-quad.  If "netOK" is "TRUE", missing trailing
   components are treated as if they were 0. *)

PROCEDURE PathCompare(a, b: Pathname.T): [-1..1];
(* Compares two pathnames lexicographically, by applying "Text.Compare"
   successively to corresponding components. *)

PROCEDURE PathLast(p: Pathname.T): Pathname.T;
(* Returns the last component of the given pathname. *)

PROCEDURE PathPrefix(p: Pathname.T): Pathname.T;
(* Returns everything except the last component of the given pathname. *)

PROCEDURE PatternMatch(pattern: TEXT;
                       options := Glob.MatchOptions{}): GlobTree.T
                       RAISES {RegEx.Error};
(* Returns a "GlobTree.T" representing either a shell pattern or a
   regular expression, according to the following rules.  If the first
   character of "pattern" is "+" then that character is removed and
   the remainder of the pattern is treated as a regular expression.
   Otherwise, the pattern is treated as a shell pattern with the
   given "options".  All regular expressions are implicitly anchored
   at the beginning and end, as if they began with "^" and ended
   with "$". The regular expression ".*" and the shell pattern "*"
   are specially recognized and evaluate to "GlobTree.True".  Thus
   the caller can compare the result against that "GlobTree.True" to
   determine whether the pattern is the universal match. *)

PROCEDURE PutCmd(wr: Wr.T;
                 cmd: TEXT;
                 f0, f1, f2, f3, f4, f5, f6, f7, f8, f9: TEXT := NIL;
		 more := FALSE;
		 encode := FALSE)
  RAISES {Thread.Alerted, Wr.Failure};
(* Outputs the given command and argument fields, separated by spaces.
   By default, the command is terminated by a newline, but that can be
   suppressed by setting "more" to TRUE.  Such an unterminated command
   can be continued by another call whose "cmd" argument is "NIL".

   If "encode" is TRUE, then white space within the fields is escaped
   using "EncodeWS". *)

PROCEDURE RCSName(name: Pathname.T): Pathname.T;
(* Returns the name of the RCS file corresponding to a checked-out file. *)

PROCEDURE ResolvePath(p1, p2: Pathname.T): Pathname.T;
(* Resolves pathname "p2" relative to "p1".  If "p2" is absolute, it is
   returned unchanged.  Otherwise, "CatPath(p1, p2)" is returned. *)

PROCEDURE StatusFileName(sfr: SupFileRec.T): Pathname.T;
(* Returns the name of the client's "checkouts" file for the given
   collection.  This is just a simple file name; the leading path is
   not included. *)

PROCEDURE StatusFileSuffix(sfr: SupFileRec.T): TEXT;
(* Returns just the suffix of the client's "checkouts" file for the
   given collection.  The leading ".", if any, is included. *)

PROCEDURE TempName(p: Pathname.T): Pathname.T;
(* Returns a name for a temporary file in the same directory as the given
   file. *)

END SupMisc.
