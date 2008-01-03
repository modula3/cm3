(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jun  8 17:23:14 PDT 1994 by heydon                   *)
(*      modified on Fri Aug  7 21:53:59 PDT 1992 by myers                    *)

INTERFACE JunoToken;

(* A JunoToken.T represents one of the possible input tokens of the Juno
   language. Juno has 8 token "classes": literal real numbers, literal texts,
   identifiers, operators, keywords, reserved identifiers, comments, and an
   end-of-stream marker. This procedure also provides the procedure ToText()
   for converting a token to a textual form. *)

IMPORT JunoValue;

TYPE
  Kind =
    { LitReal, LitText, Id,
      Semi, Dot, Comma, Colon, LPren, RPren, LBrace, RBrace, LBracket,
        RBracket, Assign, SuchThat, Else, Guard, Near, Equals, Differs, Less,
        Greater, AtMost, AtLeast, Plus, Minus, Times, Divide, Concat,
      Module, Private, End, Import, Const, Var, Pred, Func, Proc, UI, Is,
        Skip, Abort, If, Fi, Do, Od, Save, In, Nil, True, False, Or, And, Not,
        Exists, Cong, Para, Hor, Ver, Rel, Div, Mod,
      Real, Text, Pair, Int, Floor, Ceiling, Round, Abs, Sin, Cos, Ln,
        Exp, Car, Cdr, Max, Min, Atan,
      Comment, EndMarker, Unknown };
  Op = [Kind.Semi..Kind.Concat];	 (* operators *)
  KeyWd = [Kind.Module..Kind.Mod];	 (* keywords *)
  ResvdId = [Kind.Real..Kind.Atan];	 (* reserved identifiers *)

CONST
  AllOps = SET OF Kind{FIRST(Op)..LAST(Op)};
  AllKeyWds = SET OF Kind{FIRST(KeyWd)..LAST(KeyWd)};
  AllResvdIds = SET OF Kind{FIRST(ResvdId)..LAST(ResvdId)};

TYPE
  T = REF RECORD
    kind: Kind;				 (* kind of token *)
    text: TEXT;				 (* text of the token *)
    val: REFANY;			 (* TEXT or Atom.T *)
    num: JunoValue.Real;                 (* for real values *)
  END;

(* A token "t" has a kind and an optional value. The value "t.val" is
   meaningful if and only if "t.kind" is "Kind.LitText", "Kind.Id", or
   "Kind.Comment". The value "t.num" is meaningful if and only if "t.kind"
   is "Kind.LitReal". There are 8 token classes: literal reals, 
   literal texts, identifiers, operators, keywords, reserved identifiers,
   comments, and end-of-stream. A token "t" is classified according to the
   following table:
|
|    Class           Condition               Value Type
|    literal real    t.kind = Kind.LitReal   t.num is a JunoValue.Real
|    literal text    t.kind = Kind.LitText   t.val is a TEXT
|    identifier      t.kind = Kind.Id        t.val is an Atom.T
|    operator        t.kind IN AllOps
|    keyword         t.kind IN AllKeyWds
|    reserved id     t.kind IN AllResvdIds
|    comment         t.kind = Kind.Comment   t.val is a TEXT
|    end-of-stream   t.kind = Kind.EndMarker
|
   The TEXT associated with a text literal does not include the start and end
   double-quote characters, and escape sequences in the input literal have
   been converted to ASCII characters in the result text. The TEXT associated
   with a comment *does* include the begin- and end-of-comment characters, as
   well as those of any nested comments. *)

PROCEDURE Copy(t: T): T;
(* Returns a new token with the same field values as "t". *)

PROCEDURE ToName(t: T): TEXT;
(* Produces a human-readable representation of the token "t" as a TEXT. This
   representation has the form: "kind(value)", where "kind" is one of the
   strings "Real", "Text", "Id", "Op", "KeyWd", "ResvdId", or "EndMarker", and
   "value" is the token value. When "kind" is "Op", "KeyWd", or "ResvdId", the
   "value" printed is the implicit value of the operator, keyword, or reserved
   identifier, respectively, such as ":=", "IMPORT", or "FLOOR". When "kind"
   is "EndMarker", no value is printed. *)

PROCEDURE ToText(t: T): TEXT;
(* Produces a textual representation of the token "t". If substituted for the
   text of the token actually found in the program, this representation will
   not change the program's meaning. *)

CONST
  KindName = ARRAY Kind OF TEXT {
    "numeric literal", "text literal", "identifier",
    "semicolon", "period", "comma", "colon", "left parenthesis",
      "right parenthesis", "left brace", "right brace",	"left bracket",
      "right bracket", ":=", "::", "|", "->", "~", "=", "#", "<", ">",
      "<=", ">=", "+", "-", "*", "/", "&",
    "MODULE", "PRIVATE", "END", "IMPORT", "CONST", "VAR",
      "PRED", "FUNC", "PROC", "UI", "IS", "SKIP", "ABORT", "IF", "FI",
      "DO", "OD", "SAVE", "IN", "NIL", "TRUE", "FALSE", "OR", "AND", "NOT",
      "EXISTS", "CONG", "PARA", "HOR", "VER", "REL", "DIV", "MOD",
    "REAL", "TEXT", "PAIR", "INT", "FLOOR", "CEILING", "ROUND", "ABS", "SIN",
      "COS", "LN", "EXP", "CAR", "CDR", "MAX", "MIN", "ATAN",
    "comment", "end of file", "unknown token" };

END JunoToken.
