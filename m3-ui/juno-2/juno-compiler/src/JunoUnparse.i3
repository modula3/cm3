(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Mar 28 14:47:43 PST 1996 by heydon                   *)
(*      modified on Tue Aug 31 21:59:50 PST 1993 by gnelson                  *)

INTERFACE JunoUnparse;

(* Provides procedures for unparsing Juno modules, top-level module blocks,
   commands, and expressions. A number of valid tokens may be specified so
   that incorrect programs may be unparsed up to the point of failure.

   Each of these procedures unparses some subtype of "JunoAST.T" to a writer
   "wr" at an initial indentation level of "indent" to a maximum width of
   "width" characters. Those procedures taking a "tokens" argument unparse at
   most the specified number of tokens. In all cases, the writer is left
   positioned just after the last character of the last token written; no
   new-line is appended. 

   Those procedures that take a "private" argument will supress the unparsing
   of public predicate, function, and procedure bodies when "private = FALSE".

   For each of the unparsing procedures, a TRUE value for "debug" will cause
   AST's to be printed out with debugging information. In particular, variable
   indices will be added. *)

IMPORT JunoAST, Wr, Formatter, Real AS R;

CONST Prec: CARDINAL = R.MaxSignifDigits - 1;

PROCEDURE Block(wr: Wr.T; ast: JunoAST.Block; tokens: CARDINAL;
  indent: CARDINAL := 0; width: CARDINAL := 75; prec := Prec;
  debug := FALSE; private := TRUE) RAISES {Wr.Failure};

PROCEDURE Cmd(wr: Wr.T; ast: JunoAST.Cmd; tokens: CARDINAL;
  indent: CARDINAL := 0; width: CARDINAL := 75; prec := Prec; debug := FALSE)
  RAISES {Wr.Failure};

PROCEDURE Expr(wr: Wr.T; ast: JunoAST.Expr; tokens: CARDINAL;
  indent: CARDINAL := 0; width: CARDINAL := 75; prec := Prec; debug := FALSE)
  RAISES {Wr.Failure};

PROCEDURE P(wr: Wr.T; ast: JunoAST.T; indent: CARDINAL := 0;
  width: CARDINAL := 75; prec := Prec; debug := FALSE; private := TRUE;
  errast: JunoAST.T := NIL) RAISES {Wr.Failure};
(* Procedure "P" will unparse any kind of AST, so long as it does not
   contain a parse error. If an ``error AST'' is supplied, the characters
   "\001" and "\002" will be placed in the output stream to delimit
   the extent of the error AST. *)

PROCEDURE ToFmt(fmt: Formatter.T; ast: JunoAST.T; indent: CARDINAL := 0;
  prec := Prec; debug := FALSE; private := TRUE; errast: JunoAST.T := NIL)
  RAISES {Wr.Failure};
(* Like "P" above, but the output is written to the formatter "f". The
   formatter is flushed but not closed at the end of this procedure. *)

PROCEDURE Debug(r: REFANY);
(* Unparse the "JunoAST.T" or "JunoValue.T" "r" to stderr. Real numbers are
   unparsed to "Prec" precision. *)

END JunoUnparse.
