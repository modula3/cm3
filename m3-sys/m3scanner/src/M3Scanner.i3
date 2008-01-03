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
  TK_Ident = 3;            TK_Card_const = 4;       TK_Long_const = 5;
  TK_Real_const = 6;       TK_Longreal_const = 7;   TK_Extended_const = 8;
  TK_Char_const = 9;       TK_Text_const = 10;

  (* operators *)
  TK_Plus = 11;            TK_Minus = 12;           TK_Asterisk = 13;
  TK_Slash = 14;           TK_Assign = 15;          TK_Ampersand = 16;
  TK_Dot = 17;             TK_Comma = 18;           TK_Semi = 19;
  TK_L_paren = 20;         TK_L_bracket = 21;       TK_L_brace = 22;
  TK_Arrow = 23;           TK_Equal = 24;           TK_Sharp = 25;
  TK_Less = 26;            TK_Greater = 27;         TK_Ls_equal = 28;
  TK_Gr_equal = 29;        TK_Dot_dot = 30;         TK_Colon = 31;
  TK_R_paren = 32;         TK_R_bracket = 33;       TK_R_brace = 34;
  TK_Bar = 35;             TK_Subtype = 36;         TK_Implies = 37;
  TK_Begin_pragma = 38;    TK_End_pragma = 39;

  (* reserved words *)
  TK_And = 40;             TK_Any = 41;             TK_Array = 42;
  TK_As = 43;              TK_Begin = 44;           TK_Bits = 45;
  TK_Branded = 46;         TK_By = 47;              TK_Case = 48;
  TK_Const = 49;           TK_Div = 50;             TK_Do = 51;
  TK_Else = 52;            TK_Elsif = 53;           TK_End = 54;
  TK_Eval = 55;            TK_Except = 56;          TK_Exception = 57;
  TK_Exit = 58;            TK_Exports = 59;         TK_Finally = 60;
  TK_For = 61;             TK_From = 62;            TK_Generic = 63;
  TK_If = 64;              TK_Import = 65;          TK_In = 66;
  TK_Interface = 67;       TK_Lock = 68;            TK_Loop = 69;
  TK_Methods = 70;         TK_Mod = 71;             TK_Module = 72;
  TK_Not = 73;             TK_Object = 74;          TK_Of = 75;
  TK_Or = 76;              TK_Overrides = 77;       TK_Procedure = 78;
  TK_Raise = 79;           TK_Raises = 80;          TK_Readonly = 81;
  TK_Record = 82;          TK_Ref = 83;             TK_Repeat = 84;
  TK_Return = 85;          TK_Reveal = 86;          TK_Set = 87;
  TK_Then = 88;            TK_To = 89;              TK_Try = 90;
  TK_Type = 91;            TK_Typecase = 92;        TK_Unsafe = 93;
  TK_Until = 94;           TK_Untraced = 95;        TK_Value = 96;
  TK_Var = 97;             TK_While = 98;           TK_With = 99;

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

    "<id>", "<cardinal>", "<long>", "<real>", "<longreal>", "<extended>",
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
