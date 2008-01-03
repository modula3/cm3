(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE M3Lexer;

IMPORT M3ID, M3Scanner, Rd, Target;

TYPE
  T <: T_;  T_ = M3Scanner.Default OBJECT
    char  : INTEGER;
    text  : TEXT;
    int   : Target.Int;
    float : Target.Float;
    id    : M3ID.T;
  METHODS
    initFromRd  (rd  : Rd.T): T;
    initFromBuf (buf : Buf): T;
  END;

  Buf = M3Scanner.Buf;

TYPE
  TK = [FIRST (M3Scanner.TK) .. TK_Fatal];

CONST (* additional Token classes returned by a "Lexer.T". *)
  TK_Inline   = LAST (M3Scanner.TK) + 1;
  TK_External = LAST (M3Scanner.TK) + 2;
  TK_Assert   = LAST (M3Scanner.TK) + 3;
  TK_Unused   = LAST (M3Scanner.TK) + 4;
  TK_Obsolete = LAST (M3Scanner.TK) + 5;
  TK_Trace    = LAST (M3Scanner.TK) + 6;
  TK_CallConv = LAST (M3Scanner.TK) + 7;
  TK_Fatal    = LAST (M3Scanner.TK) + 8;

END M3Lexer.

(*
An "M3Lexer.T", or lexer, parses a stream of characters and
returns a stream of Modula-3 tokens.  It is an enhancement
of a standard "M3Scanner.Default" scanner with the following
additional properties:

  Aside from the initial value, "TK_Comment" tokens are discarded.

  If "s.token" is "TK_Ident", "s.id" is set to the corresponding M3ID.T;

  If "s.token" is "TK_Card_const", "s.int" is set to the corresponding value.

  If "s.token" is "TK_Real_const", "TK_Longreal_const", or "TK_Extended_const",
  "s.float" is set to the corresponding value.

  If "s.token" is "TK_Char_const", "s.char" is set to the "ORD" of the
  corresponding character value.

  If "s.token" is "TK_Text_const", "s.text" is set to the corresponding
  TEXT value.  Escaped characters in the input are converted to their
  corresponding values in "s.text".

  The "<*INLINE*>", "<*EXTERNAL*>", "<*ASSERT*>", "<*UNUSED*>",
  "<*OBSOLETE*>", "<*TRACE*>", "<*FATAL*>", and calling convention
  pragmas are recognized and returned as new token classes.  The
  preceding "TK_Begin_pragma" token is discarded.  The portion
  of the pragma following the initial identifer is returned as separate
  tokens ending with a "TK_End_pragma" token.

  The "<*NOWARN*>", "<*PRAGMA*>", and "<*LINE*>" pragmas are recognized
  and discarded.

  All other pragmas are returned as streams of tokens beginning with
  "TK_Begin_pragma" and ending with "TK_End_pragma".

It is the responsibility of the "M3Lexer" client to initialize
the "Target" module.
*)
