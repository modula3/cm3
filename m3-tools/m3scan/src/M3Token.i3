(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3Token.i3                                            *)
(* Last modified on Fri May 28 10:03:16 PDT 1993 by kalsow     *)

INTERFACE M3Token;

TYPE
  T = {
    EOF, Comment, Error,

    (* lexical classes with variable literals *)
    Ident, Card_const, Real_const, Longreal_const,
    Extended_const, Char_const, Text_const,

    (* operators *)
    Plus, Minus, Asterisk, Slash, Assign, Ampersand, Dot,
    Comma, Semi, L_paren, L_bracket, L_brace, Arrow, Equal,
    Sharp, Less, Greater, Ls_equal, Gr_equal, Dot_dot,
    Colon, R_paren, R_bracket, R_brace, Bar, Subtype,
    Implies, Begin_pragma, End_pragma,

    (* reserved words *)
    And, Any, Array, As, Begin, Bits, Branded, By, Case,
    Const, Div, Do, Else, Elsif, End, Eval, Except,
    Exception, Exit, Exports, Finally, For, From, Generic,
    If, Import, In, Interface, Lock, Loop, Methods, Mod,
    Module, Not, Object, Of, Or, Overrides, Procedure,
    Raise, Raises, Readonly, Record, Ref, Repeat, Return,
    Reveal, Set, Then, To, Try, Type, Typecase, Unsafe,
    Until, Untraced, Value, Var, While, With
  };

CONST
  First_Literal  = T.Ident;
  Last_Literal   = T.Text_const;
  First_Operator = T.Plus;
  Last_Operator  = T.End_pragma;
  First_Keyword  = T.And;
  Last_Keyword   = T.With;

VAR (*CONST*)
  name: ARRAY T OF TEXT;

END M3Token.
