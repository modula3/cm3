(* Copyright (C) 1992, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(* Last modified on Tue Nov  9 08:53:36 PST 1993 by mcjones *)
(*      modified on Wed Feb  3 15:10:02 PST 1993 by mjordan *)
(*      modified on Tue Feb 11 16:18:58 PST 1992 by muller  *)

(* "Pathname" defines procedures for manipulating pathnames in a
   portable fashion.  *)

INTERFACE Pathname;

IMPORT TextSeq;

TYPE
  T = TEXT;
  Arcs = TextSeq.T;

(* Most operating systems include a file system providing persistent
   storage (files) and naming (directories).  The name space is
   usually a directed, rooted graph in which interior nodes are
   directories and exterior nodes are files and empty directories.
   Each arc is labeled with a character string called an arc name; the
   arc names in any one directory are distinct.  A "Pathname.T" (or
   just a pathname) is a text conforming to the syntax of the
   underlying operating system.  It consists of a sequence of arc
   names specifying a path starting from some distinguished directory
   and ending at the referent of the pathname.

   A pathname may be absolute, in which case it begins with the name
   of a root directory.  If a pathname is not absolute, it is
   interpreted relative to the working directory associated with the
   process (see "GetWorkingDirectory" in the "Process" interface).
   \index{absolute pathname}
   \index{relative pathname}

   Not all operating systems use the same syntax for pathnames, so we
   define the type "Arcs" to represent a pathname in a standard form
   allowing manipulations by portable programs.  Suppose "a" is of
   type "Arcs".  Then "a" is non-"NIL", "a.getlo()" indicates whether
   or not the pathname is absolute, and "TextSeq.Sub(a, 1)" represents
   a sequence (possibly empty) of arc names (all non-"NIL").  If "a"
   represents an absolute pathname, then "a.getlo()" is the root
   directory name and is non-"NIL"; if "a" represents a relative
   pathname, then "a.getlo()" is "NIL".

   It is often useful to view an arc name as having two parts, a base
   and an extension, separated by a period, for example "Pathname.i3".
   \index{extension of pathname}
   \index{base of pathname}

   See the end of this interface for operating-system specific
   details.

*)

EXCEPTION Invalid;

PROCEDURE Valid(pn: T): BOOLEAN;
(* Return "TRUE" iff "pn" conforms to the pathname syntax of this
   operating system. *)

(* When a pathname with invalid syntax is passed to a procedure in
   this interface not declared as raising the exception "Invalid",
   the result is undefined, but safe. *)

PROCEDURE Decompose(pn: T): Arcs RAISES {Invalid};
(* Parse "pn", returning a sequence whose first element is a root
   directory name (possibly "NIL") and whose remaining elements
   consist of zero or more arc names.  Raise "Invalid" if "Valid(pn)"
   is "FALSE".  *)

(* "Decompose" returns exactly the sequence of arc names present in
   "pn"; it doesn't attempt to produce a canonical form. Some
   operating systems allow zero-length arc names (see the discussion
   of specific systems at the end of this section.) *)

PROCEDURE Compose(a: Arcs): T RAISES {Invalid};
(* Combine the elements of "a" to form a pathname corresponding to the
   syntax of this operating system.  Raise "Invalid" if "a" is "NIL",
   if "a.getlo()" is neither "NIL" nor a valid root directory name, or
   if one of the elments of "TextSeq.Sub(a, 1)" is not a valid arc
   name. *)

PROCEDURE Absolute(pn: T): BOOLEAN;
(* Return "TRUE" iff "pn" is an absolute pathname.  Equivalent to
   "Decompose(pn).getlo() # NIL", but faster. *)

PROCEDURE Prefix(pn: T): T;
(* Return a pathname equal to "pn" up to, but not including, the final
   arc name.  If "pn" consists only of a root directory name,
   "Prefix(pn)" returns "pn". *)

PROCEDURE Last(pn: T): T;
(* Return the final arc name in "pn".  If "pn" consists only of a root
   directory name, "Last(pn)" returns the empty string.  *)

PROCEDURE Base(pn: T): T;
(* Return a pathname equal to "pn" except with "Last(pn)" replaced by
   its base. *)

PROCEDURE Join(pn, base: T; ext: TEXT): T;
(* Return a pathname formed by prepending "pn" to "base" (if "pn" is
   not "NIL") and appending "ext" to "base" (if "ext" is not "NIL").
   More precisely, this is equivalent to the following, in which "a"
   is a local variable of type "Arcs": *)
(*
| IF pn = NIL THEN a := NIL
| ELSE
|   IF Absolute(base) THEN `Cause checked runtime error` END;
|   a := Decompose(pn)
| END;
| IF ext # NIL THEN base := base & "." & ext END;
| RETURN Compose(
|   TextSeq.Cat(a, TextSeq.Sub(Decompose(base), 1)))
*)

(* The value returned by "Join" will be a valid pathname only if the
   "base" and "ext" conform to the syntax of the particular operating
   system, as specified at the end of this section. *)

PROCEDURE LastBase(pn: T): T;
(* Return the base of the final arc name of "pn".  It is a checked
   runtime error if "pn" is empty or consists only of a root directory
   name. *)

PROCEDURE LastExt(pn: T): TEXT;
(* Return the extension of the last arc name of "pn".  It is a checked
   runtime error if "pn" is empty or consists only of a root directory
   name. *)

PROCEDURE ReplaceExt(pn: T; ext: TEXT): T;
(* Return a pathname equal to "pn" except with the extension of the
   final arc name replaced with "ext", which must be non-"NIL". *)

VAR (*CONST*)
  Parent: TEXT;
(* A special arc name that, when encountered during a pathname lookup,
   stands for the parent of the directory currently being examined.
   *)
  Current: TEXT;
(* A special arc name that, when encountered during a pathname lookup,
   stands for the directory currently being examined. *)

END Pathname.

(*

\paragraph*{POSIX.} Pathnames have the syntax:

| Pathname = Absolute | Relative.
| Absolute = "/" Relative.
| Relative = [ArcName {"/" ArcName}].

   "Parent" is ``..'' and "Current" is ``.''.

   There is only one root directory and it is named ``/''. A
   POSIX-compliant system must support arc names at least as long as
   fourteen characters.  An arc name longer than the maximum supported
   is either silently truncated by the operating system or is reported
   as an error, depending on a configuration option.  A zero-length
   arc name is treated the same as ``.''.  An arc name may contain any
   character except ``/'' and the null character, but for maximum
   portability the POSIX specification recommends they be restricted
   to upper and lower case letters, digits, and these special
   characters:

|  . `{\tt \char'137}` -

   Furthermore, it is recommended that arc names not start with hyphen
   (-).

   The extension of an arc name is the suffix starting after the last
   ``.''.

   The base of an arc name is the prefix up to, but not including, the
   final ``.'' if the extension is nonempty; it is the entire arc name
   if the extension is empty.

\paragraph*{Win32.} Pathnames have the syntax, where backslash is not
   an escape character but a literal character:

| Pathname = Absolute | Relative.
| Absolute = Volume "`{\tt \char'134}`" Relative.
| Relative = [ArcName {"\" ArcName}].
| ArcName  = Base "." Extension | "." | "..".
| Volume   = Drive ":" | "`{\tt \char'134\char'134}`" Server "`{\tt \char'134}`" Share.
| Server   = ?
| Share    = ?

   "Parent" is ``..'' and "Current" is ``.''.

   The FAT (MS-DOS) file system restricts "Drive" to a single letter,
   and "Base" to between one and eight letters, digits, or these
   special characters:

| $ % ' - _ @ { } `\char'176` `\char'140` ! # ( )

   "Extension" is one to three characters from the same set.  Certain
   "Base"s, including AUX, CLOCK\$, COM1, CON, LPT1, NUL, and PRN are
   reserved---they name devices, regardless of the directory or
   extension.  Embedded (but not trailing) spaces are allowed in the
   "Base" of a file name (but not a directory name).

   The HPFS and NTFS file systems allow arc names up to 254
   characters, and these additional special characters are allowed:

| , + = [ ] ; 

   Additionally, blank is significant anywhere in an arc name except
   at the end.  Win32 allows a programmer to use either ANSI or
   Unicode representation for pathname strings.  The NTFS file system
   stores full Unicode pathnames in the directories.

\paragraph*{Macintosh.} Pathnames have the syntax:

| Pathname   = Absolute | Relative.
| Absolute   = Volume ":" [ArcName {Colons ArcName}].
| Relative   = ArcName
|            | Colons ArcName {Colons ArcName}.
| Colons     = ":" {":"}.

   "Parent" is ``::'' and "Current" is ``:''.

   A "Volume" is one to twenty-seven printing characters excluding
   colon (:).  An arc name is one to thirty-one printing characters
   excluding colon.  A single colon is a separator; "n+1" adjacent
   colons means the "n"th parent.

   The extension of an arc name is the suffix starting after the last
   ``.''; if there is no ``.'', the extension is empty.

   The base of an arc name is the prefix up to, but not including, the
   final ``.'' if the extension is nonempty; it is the entire arc name
   if the extension is empty.

*)
