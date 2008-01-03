(* Copyright 1993 Digital Equipment Corporation.                       *)
(* Distributed only by permission.                                     *)
(* See the file COPYRIGHT for a full description.                      *)
(* Last modified on Fri Dec 10 10:29:13 PST 1993 by mcjones            *)
(*      modified on Wed Apr 28 21:37:34 PDT 1993 by meehan             *)
(*      modified on Wed Feb 17 21:46:45 PST 1993 by mjordan            *)
(*      modified on Tue Jan 26 14:38:00 PST 1993 by gnelson            *)

(* An "Sx.T" is a symbolic expression represented as a recursive
   linked list structure, as in Lisp.  This interface provides
   routines for reading and printing symbolic expressions, as well as
   some convenience procedures for manipulating them.  The syntax of
   an "Sx.T" is as follows:

| Sx = Char | Text | Int | Real | Longreal | Extended
|    | Atom | Boolean | "(" List ")".
|
| List =  {Sx}.

   \index{symbolic expression}

   A "Char" is a Modula-3 character literal; the corresponding "Sx.T"
   is of type "REF CHAR".

   A "Text" is a Modula-3 text literal.  The corresponding "Sx.T" is
   a "TEXT".

   An "Int" is a Modula-3 integer literal, possibly preceded by a plus
   sign ("+") or minus sign ("-").  The corresponding "Sx.T" is of
   type "REF INTEGER".

   A "Real", "Longreal", or "Extended" is a floating-decimal
   number parsed using the grammar for "Float" specified in the "Lex"
   interface.  The corresponding "Sx.T" is of type "REF REAL", "REF
   LONGREAL" or "REF EXTENDED", depending on whether the letter
   introducing the exponent is "'e'", "'d'", or "'x'". If there is
   no exponent, the result will be of type "REF REAL".

   An "Atom" is either (1) a Modula-3 identifier, or (2) a non-empty
   sequence of characters from the set

|  ! # $ % & * + - . / : < = > ? @ [ ] ^ _ { } `{\tt \char'176}`

   or (3) a sequence of characters and escape sequences surrounded by
   vertical bars ("|"s).  The escape sequences are the same as those
   allowed in Modula-3 text literals, with the addition of
   {\def\ttSlashBackslash{{\tt \char'134\char'174}} \ttSlashBackslash}
   to allow an atom to contain "|".  In all three cases, the
   corresponding "Sx.T" is an "Atom.T".

   For example, the following are valid atoms:

| A1
| +=
| |1\||

   A "Boolean" is either "TRUE" or "FALSE"; the corresponding "Sx.T"
   is of type "Atom.T"; in other words, this is not a distinct type.

   The "Sx.T" corresponding to a "List" is a "RefList.T" containing
   the items of the list in order.

   The tokens of an "Sx.T" can be separated by arbitrary sequences of
   blanks, tabs, newlines, carriage returns, form feeds, and vertical
   tabs, which are ignored.  (These are the same whitespace characters
   that are ignored between tokens of a Modula-3 program.) They can
   also be separated by comments, which begin with a semicolon and end
   with newline.

   The syntax of tokens can be extended with the "SetReadMacro" procedure.

*)

INTERFACE Sx;

IMPORT Atom, Rd, RefList, Thread, Wr;

TYPE T = REFANY;

EXCEPTION
  ReadError(TEXT);
  PrintError(TEXT);

PROCEDURE FromChar(c: CHAR): REF CHAR;
(* Return a "Char" with value "c". *)

PROCEDURE FromInt(i: INTEGER): REF INTEGER;
(* Return an "Int" with value "i". *)

PROCEDURE FromReal(r: REAL): REF REAL;
(* Return a "Real" with value "r". *)

PROCEDURE FromLongReal(r: LONGREAL): REF LONGREAL;
(* Return a "Longreal" with value "r". *)

PROCEDURE FromExtended(r: EXTENDED): REF EXTENDED;
(* Return an "Extended" with value "r". *)

PROCEDURE FromBool(b: BOOLEAN): Atom.T;
(* Return a "Boolean". If "b" is "TRUE", return "Sx.True".
   Otherwise, return "Sx.False". *)

(* The "From..." procedures do not necessarily perform an allocation:
   if the same value is passed to two calls, the same reference may be
   returned.  As a consequence, clients should not modify the referent
   of a reference returned by any of these procedures.

   Each "REF CHAR", "REF INTEGER", "REF REAL", "REF LONGREAL", "REF
   EXTENDED", "TEXT", or "Atom.T", no matter how constructed, is an
   "Sx.T". *)

VAR (*CONST*) True, False: Atom.T;
(* {\tt True = Atom.FromText(\char'42TRUE\char'42)},
    {\tt False = Atom.FromText(\char'42FALSE\char'42)}. *)

PROCEDURE Read(rd: Rd.T; syntax: Syntax := NIL): T
  RAISES {ReadError, Rd.EndOfFile, Thread.Alerted};
(* Read and return a symbolic expression from "rd", ignoring
   whitespace and comments. If "syntax" is "NIL", use the syntax
   described above; otherwise use any read macros that have been
   registered in "syntax". *)

PROCEDURE ReadDelimitedList(
    rd: Rd.T; delim : CHAR; syntax: Syntax := NIL): RefList.T
  RAISES {ReadError, Thread.Alerted};
(* Repeatedly read symbolic expressions from "rd", ignoring whitespace
   and comments, until the next character is "delim"; consume the
   delimiter and return the list of symbolic expressions that were
   read.  Raise "ReadError" if there is a syntax error, including
   unexpected end of file. *)

PROCEDURE Print(
    wr: Wr.T;
    sx: T;
    maxDepth: CARDINAL := LAST(CARDINAL);
    maxLength: CARDINAL := LAST(CARDINAL))
  RAISES {PrintError, Wr.Failure, Thread.Alerted};
(* Print the symbolic expression "sx" on the writer "wr", assuming the
   standard syntax. *)

(* Each sublist will contain no more than "maxLength" elements; extra
   elements are replaced by an ellipsis (three dots).  Any sublist
   nested at a depth greater than "maxDepth" is also replaced by an
   ellipsis.  "Print" inserts "|" around atoms if necessary to ensure
   that they are readable.  "Print" does not insert line-breaks or
   indentation to produce a human-readable (``pretty-printed'') format
   for large symbolic expressions.

   "Print" will raise "PrintError" if it tries to print something that
   is not ``printable'' (as defined below). If a list contains an
   unprintable element that is beyond the limits established by
   "maxDepth" and "maxLength", "PrintError" may or may not be raised.

   An object is said to be ``printable'' if it satisfies the following
   hypothetical predicate:
|
|  PROCEDURE Printable(x: REFANY): BOOLEAN =
|    BEGIN
|      TYPECASE x OF
|      | NULL, REF CHAR, TEXT, REF INTEGER, REF REAL,
|        REF LONGREAL, REF EXTENDED, Atom.T => 
|          RETURN TRUE
|      | RefList.T (list) => RETURN Printable(list.head) AND 
|                                   Printable(list.tail)
|      ELSE
|          RETURN FALSE
|      END
|   END Printable;
|
   "Read(rd,NIL)" is guaranteed to return a printable value unless it
   raises an exception. Assuming the defaults for "syntax",
   "maxDepth", and "maxLength", and assuming no exceptions are raised,
   "Read" and "Print" are ``inverses''.
*)

TYPE Syntax <: REFANY;

(* A "Syntax" is a partial map from characters to read macros. *)

PROCEDURE CopySyntax(s: Syntax := NIL): Syntax;
(* Allocate and return a new syntax table whose contents are the same
   as "s" or, if "s = NIL", the same as the standard syntax table. The
   standard syntax table has no read macros. *)

PROCEDURE SetReadMacro(s: Syntax; ch: CHAR; m: ReadMacro);
(* Set "s[ch] := m". It is a checked runtime error if "s = NIL", if
   "ch" is a whitespace character, or if "ch = ';'". It is allowed for
   "m" to be "NIL"; this has the effect of removing the mapping, if
   any, from "ch" to a readmacro. *)

TYPE ReadMacro = OBJECT METHODS
    read(rd: Rd.T; s: Syntax): RefList.T
      RAISES {ReadError, Thread.Alerted}
  END;

(* If you pass a "Syntax" "s" to "Read" or "ReadDelimitedList", then
   the reading algorithm is modified as follows.  After skipping
   whitespace and comments, and before reading a token, the next
   character in the input stream is consumed and examined.  If "s"
   defines a read macro for this character, then this read macro is
   called with the same arguments that were passed to "Read" or
   "ReadDelimitedList".  The resulting list is spliced into the
   current list being built.  In particular, if the macro returns
   "NIL", then everything it read is ignored; if the macro returns a
   single-element list, then that single element is inserted into the
   list being built.  "ReadError" is raised if the macro returns a
   non-list or if it returns a multi-element list in a context where
   no list is being built, such as at the top level of "Read".

   For example, the following program fragment constructs a syntax
   table that extends the standard syntax in two ways.  First,
   additional comments are supported by ignoring all characters
   between "{" and "}".  Second, an expression of the form 
   "[e1~...~en]" is turned into the list "(ARRAY e1~...~en)":

| VAR syn := CopySyntax(); BEGIN
|   SetReadMacro(syn, '{',
|     NEW(ReadMacro, read := ReadComment));
|   SetReadMacro(syn, '[',
|     NEW(ReadMacro, read := ReadArray));
|   ...
|
| PROCEDURE ReadComment(
|     self: ReadMacro; rd: Rd.T; <* UNUSED *> s: Syntax)
|   : RefList.T =
|   BEGIN
|     WHILE NOT Rd.EOF() AND Rd.GetChar(rd) # '}' DO
|       (* SKIP *)
|     END;
|     RETURN NIL
|   END ReadComment;
|
| VAR (*CONST*) arrayAtm := Atom.FromText("ARRAY");
|
| PROCEDURE ReadArray(self: ReadMacro; rd: Rd.T; s: Syntax)
|   : RefList.T =
|   VAR elements := ReadDelimitedList(rd, ']', s);
|   BEGIN
|     RETURN RefList.List1(RefList.Cons(arrayAtm, elements))
|   END ReadArray;

   The call to "RefList.List1" in "ReadArray" is important.  If it were
   omitted, then the text

| (a b [c d])

   would be read as

| (a b ARRAY c d)

   instead of the intended

| (a b (ARRAY c d)).

*)

END Sx.
