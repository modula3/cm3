(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3Scanner.i3                                          *)
(* Last modified on Thu Dec  8 10:42:16 PST 1994 by kalsow     *)

INTERFACE M3Scanner;

IMPORT Rd;

TYPE
  T = OBJECT
    token        : CARDINAL  := TK_Comment;
    start        : CARDINAL  := 0;
    length       : CARDINAL  := 0;
    offset       : CARDINAL  := 0;
    line         : CARDINAL  := 0;
    column       : CARDINAL  := 0;
    msg          : TEXT      := NIL;
    buffer       : Buf       := NIL;
  METHODS
    next ();
    toText (): TEXT;
    className (tk: INTEGER): TEXT;
  END;

  Buf = BRANDED "M3Scanner.Buf" REF ARRAY OF CHAR;

TYPE
  Default <: T OBJECT METHODS
    initFromRd  (source        : Rd.T;
                 skip_comments := TRUE;
                 split_pragmas := TRUE): T;
    initFromBuf (buf           : Buf;
                 skip_comments := TRUE;
                 split_pragmas := TRUE): T;
  END;

TYPE
  TK = [TK_Comment .. TK_With];

CONST (* Token classes returned by a "Default" scanner. *)
  TK_Comment = 0;          TK_EOF = 1;              TK_Error = 2;

  (* lexical classes with variable literals *)
  TK_Ident = 3;            TK_Card_const = 4;       TK_Real_const = 5;
  TK_Longreal_const = 6;   TK_Extended_const = 7;   TK_Char_const = 8;
  TK_Text_const = 9;

  (* operators *)
  TK_Plus = 10;            TK_Minus = 11;           TK_Asterisk = 12;
  TK_Slash = 13;           TK_Assign = 14;          TK_Ampersand = 15;
  TK_Dot = 16;             TK_Comma = 17;           TK_Semi = 18;
  TK_L_paren = 19;         TK_L_bracket = 20;       TK_L_brace = 21;
  TK_Arrow = 22;           TK_Equal = 23;           TK_Sharp = 24;
  TK_Less = 25;            TK_Greater = 26;         TK_Ls_equal = 27;
  TK_Gr_equal = 28;        TK_Dot_dot = 29;         TK_Colon = 30;
  TK_R_paren = 31;         TK_R_bracket = 32;       TK_R_brace = 33;
  TK_Bar = 34;             TK_Subtype = 35;         TK_Implies = 36;
  TK_Begin_pragma = 37;    TK_End_pragma = 38;

  (* reserved words *)
  TK_And = 39;             TK_Any = 40;             TK_Array = 41;
  TK_As = 42;              TK_Begin = 43;           TK_Bits = 44;
  TK_Branded = 45;         TK_By = 46;              TK_Case = 47;
  TK_Const = 48;           TK_Div = 49;             TK_Do = 50;
  TK_Else = 51;            TK_Elsif = 52;           TK_End = 53;
  TK_Eval = 54;            TK_Except = 55;          TK_Exception = 56;
  TK_Exit = 57;            TK_Exports = 58;         TK_Finally = 59;
  TK_For = 60;             TK_From = 61;            TK_Generic = 62;
  TK_If = 63;              TK_Import = 64;          TK_In = 65;
  TK_Interface = 66;       TK_Lock = 67;            TK_Loop = 68;
  TK_Methods = 69;         TK_Mod = 70;             TK_Module = 71;
  TK_Not = 72;             TK_Object = 73;          TK_Of = 74;
  TK_Or = 75;              TK_Overrides = 76;       TK_Procedure = 77;
  TK_Raise = 78;           TK_Raises = 79;          TK_Readonly = 80;
  TK_Record = 81;          TK_Ref = 82;             TK_Repeat = 83;
  TK_Return = 84;          TK_Reveal = 85;          TK_Set = 86;
  TK_Then = 87;            TK_To = 88;              TK_Try = 89;
  TK_Type = 90;            TK_Typecase = 91;        TK_Unsafe = 92;
  TK_Until = 93;           TK_Untraced = 94;        TK_Value = 95;
  TK_Var = 96;             TK_While = 97;           TK_With = 98;

CONST
  First_Literal  = TK_Ident;
  Last_Literal   = TK_Text_const;
  First_Operator = TK_Plus;
  Last_Operator  = TK_End_pragma;
  First_Keyword  = TK_And;
  Last_Keyword   = TK_With;

CONST
  TokenName = ARRAY TK OF TEXT {
    "**COMMENT**", "**EOF**", "**ERROR**",

    "<id>", "<cardinal>", "<real>", "<longreal>", "<extended>",
    "<char>", "<text>",

    "+", "-", "*", "/", ":=", "&", ".", ",", ";", "(", "[", "{", "^",
    "=", "#", "<", ">", "<=", ">=", "..", ":", ")", "]", "}", "|",
    "<:", "=>", "<*", "*>",

    "AND", "ANY", "ARRAY", "AS", "BEGIN", "BITS", "BRANDED", "BY",
    "CASE", "CONST", "DIV", "DO", "ELSE", "ELSIF", "END", "EVAL",
    "EXCEPT", "EXCEPTION", "EXIT", "EXPORTS", "FINALLY", "FOR",
    "FROM", "GENERIC", "IF", "IMPORT", "IN", "INTERFACE", "LOCK",
    "LOOP", "METHODS", "MOD", "MODULE", "NOT", "OBJECT", "OF", "OR",
    "OVERRIDES", "PROCEDURE", "RAISE", "RAISES", "READONLY", "RECORD",
    "REF", "REPEAT", "RETURN", "REVEAL", "SET", "THEN", "TO", "TRY",
    "TYPE", "TYPECASE", "UNSAFE", "UNTIL", "UNTRACED", "VALUE", "VAR",
    "WHILE", "WITH" };

END M3Scanner.

(*
An "M3Scanner.T", or scanner, parses a stream of characters and
returns a stream of Modula-3 tokens. If "s" is a scanner, each
call "s.next()" sets the values of "s"'s fields to correspond to
the next token in the stream.
   
The fields of a scanner are not to be modified by its client.

"s.token" is the class of the token.  It is one of the
"TK_" values defined above.  Subtypes of "M3Scanner.T" may
define additional values.

In "s.buffer[s.start .. s.start+s.length-1]" are the characters
that comprise the token.

"s.offset" is the character offset of the token relative to the
beginning of the stream.  The first character of the stream is
at offset zero.

"s.line" is the line where the token occured relative
to the beginning of the stream.   The first line is one.

"s.column" is the character offset of the beginning of the token
within the line that contains it.  The first column is zero.

"s.buffer" contains the source being scanned.  Modifying its contents
may perturb the token stream or cause a checked runtime error.

"s.msg" describes the error that caused the "Error" token to be
returned.

"s.toText()" returns the TEXT value of current token.

If "tk" is a token class generated by "s", "s.className(tk)" returns
string identifing that class.  Otherwise, "NIL" is returned.

The scanner returned by "NEW(Default).initFromRd(rd)" will read the
entire contents of "rd" into its buffer and initialize
the scanner as a zero-length comment at offset zero.

The scanner returned by "NEW(Default).initFromBuf(buf)" will use
"buf" as its buffer and initialize the scanner as a zero-length
comment at offset zero.

If "skip_comments" is "TRUE", outer-level comments will be returned
as tokens.  Otherwise, comments are ignored.

If "split_pragmas" is "TRUE", the contents of pragmas will be scanned
and returned as a stream of tokens between "Begin_pragma" and
"End_pragma" tokens.  If "split_pragmas" is false, the entire pragma
is returned in a single "Begin_pragma" token.
*)
