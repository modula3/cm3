(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)
(**)
(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3CToken;

IMPORT M3AST_LX;

(* This interface defines types for the Modula-3 tokens. The basic
type is an enumeration type "E", with a member for each reserved word
in the language, identifiers, literals and the special symbols.
Subranges of this type, "ReservedWord" and "Literal" delimit the
members in those categories.

In order to support {\extension} of the basic token set, for example,
that might be used in an extension of the language via pragmas, the
type "T" is defined as "[0..255]", and constants with the same names
as the enumeration members are defined. The standard Modula-3 lexer
object deals with values of type "T", thus allowing a subtype to
extend the set with new values. This is necessary because Modula-3
does not allow subtyping of enumeration types. *) 

TYPE 
  T = [0..255];

  E = {ADDRESS_, AND_, ANY_, ARRAY_, AS_,
       BEGIN_, BITS_, BRANDED_, BY_,
       CASE_, CONST_,
       DIV_, DO_,
       ELSE_, ELSIF_, END_, EVAL_, EXCEPT_, EXCEPTION_, EXIT_, EXPORTS_,
           EXTENDED_,
       FINALLY_, FOR_, FROM_,
       GENERIC_,
       IF_, IMPORT_, IN_, INTEGER_, INTERFACE_,
       LOCK_, LONGREAL_, LOOP_,
       METHODS_, MOD_, MODULE_,
       NIL_, NOT_, NULL_,
       OBJECT_, OF_, OR_, OVERRIDES_,
       PROCEDURE_,
       RAISE_, RAISES_, READONLY_, REAL_,RECORD_, REF_, REFANY_, REPEAT_,
           RETURN_, REVEAL_, ROOT_,
       SET_,
       THEN_, TO_, TRY_, TYPE_, TYPECASE_,
       UNSAFE_, UNTIL_, UNTRACED_,
       VALUE_, VAR_,
       WHILE_, WITH_,
       Identifier,
       CharLiteral, TextLiteral, IntegerLiteral, RealLiteral, LongRealLiteral,
           ExtendedLiteral,
       Plus, Minus, Times, Divide, Equal, NotEqual, LessThan, GreaterThan,
           LessThanOrEqual, GreaterThanOrEqual,
       Ampersand, Dereference, Dot,
       Bra, Ket, CurlyBra, CurlyKet, SquareBra, SquareKet,
       Becomes, Semicolon, Comma, Colon, Bar, Range, Subtype, Implies,
       Void};

  Set = SET OF T; ESet = SET OF E;

  ReservedWord = [E.ADDRESS_ .. E.WITH_];
  Literal = [E.CharLiteral .. E.ExtendedLiteral];
  ReservedToken = [E.Plus .. E.Implies];


CONST
  ReservedWords = Set{ORD(FIRST(ReservedWord))..ORD(LAST(ReservedWord))};
  Literals = Set{ORD(FIRST(Literal))..ORD(LAST(Literal))};
  ReservedTokens = Set{ORD(FIRST(ReservedToken))..ORD(LAST(ReservedToken))};

CONST
  ADDRESS_: T = ORD(E.ADDRESS_);
  AND_: T = ORD(E.AND_);
  ANY_: T = ORD(E.ANY_);
  ARRAY_: T = ORD(E.ARRAY_);
  AS_: T = ORD(E.AS_);
  BEGIN_: T = ORD(E.BEGIN_);
  BITS_: T = ORD(E.BITS_);
  BRANDED_: T = ORD(E.BRANDED_);
  BY_: T = ORD(E.BY_);
  CASE_: T = ORD(E.CASE_);
  CONST_: T = ORD(E.CONST_);
  DIV_: T = ORD(E.DIV_);
  DO_: T = ORD(E.DO_);
  ELSE_: T = ORD(E.ELSE_);
  ELSIF_: T = ORD(E.ELSIF_);
  END_: T = ORD(E.END_);
  EVAL_: T = ORD(E.EVAL_);
  EXCEPT_: T = ORD(E.EXCEPT_);
  EXCEPTION_: T = ORD(E.EXCEPTION_);
  EXIT_: T = ORD(E.EXIT_);
  EXPORTS_: T = ORD(E.EXPORTS_);
  EXTENDED_: T = ORD(E.EXTENDED_);
  FINALLY_: T = ORD(E.FINALLY_);
  FOR_: T = ORD(E.FOR_);
  FROM_: T = ORD(E.FROM_);
  GENERIC_: T = ORD(E.GENERIC_);
  IF_: T = ORD(E.IF_);
  IMPORT_: T = ORD(E.IMPORT_);
  IN_: T = ORD(E.IN_);
  INTEGER_: T = ORD(E.INTEGER_);
  INTERFACE_: T = ORD(E.INTERFACE_);
  LOCK_: T = ORD(E.LOCK_);
  LONGREAL_: T = ORD(E.LONGREAL_);
  LOOP_: T = ORD(E.LOOP_);
  METHODS_: T = ORD(E.METHODS_);
  MOD_: T = ORD(E.MOD_);
  MODULE_: T = ORD(E.MODULE_);
  NIL_: T = ORD(E.NIL_);
  NOT_: T = ORD(E.NOT_);
  NULL_: T = ORD(E.NULL_);
  OBJECT_: T = ORD(E.OBJECT_);
  OF_: T = ORD(E.OF_);
  OR_: T = ORD(E.OR_);
  OVERRIDES_: T = ORD(E.OVERRIDES_);
  PROCEDURE_: T = ORD(E.PROCEDURE_);
  RAISE_: T = ORD(E.RAISE_);
  RAISES_: T = ORD(E.RAISES_);
  READONLY_: T = ORD(E.READONLY_);
  REAL_: T = ORD(E.REAL_);
  RECORD_: T = ORD(E.RECORD_);
  REF_: T = ORD(E.REF_);
  REFANY_: T = ORD(E.REFANY_);
  REPEAT_: T = ORD(E.REPEAT_);
  RETURN_: T = ORD(E.RETURN_);
  REVEAL_: T = ORD(E.REVEAL_);
  ROOT_: T = ORD(E.ROOT_);
  SET_: T = ORD(E.SET_);
  THEN_: T = ORD(E.THEN_);
  TO_: T = ORD(E.TO_);
  TRY_: T = ORD(E.TRY_);
  TYPE_: T = ORD(E.TYPE_);
  TYPECASE_: T = ORD(E.TYPECASE_);
  UNSAFE_: T = ORD(E.UNSAFE_);
  UNTIL_: T = ORD(E.UNTIL_);
  UNTRACED_: T = ORD(E.UNTRACED_);
  VALUE_: T = ORD(E.VALUE_);
  VAR_: T = ORD(E.VAR_);
  WHILE_: T = ORD(E.WHILE_);
  WITH_: T = ORD(E.WITH_);
  Identifier: T = ORD(E.Identifier);
  CharLiteral: T = ORD(E.CharLiteral);
  TextLiteral: T = ORD(E.TextLiteral);
  IntegerLiteral: T = ORD(E.IntegerLiteral);
  RealLiteral: T = ORD(E.RealLiteral);
  LongRealLiteral: T = ORD(E.LongRealLiteral);
  ExtendedLiteral: T = ORD(E.ExtendedLiteral);
  Plus: T = ORD(E.Plus);
  Minus: T = ORD(E.Minus);
  Times: T = ORD(E.Times);
  Divide: T = ORD(E.Divide);
  Equal: T = ORD(E.Equal);
  NotEqual: T = ORD(E.NotEqual);
  LessThan: T = ORD(E.LessThan);
  GreaterThan: T = ORD(E.GreaterThan);
  LessThanOrEqual: T = ORD(E.LessThanOrEqual);
  GreaterThanOrEqual: T = ORD(E.GreaterThanOrEqual);
  Ampersand: T = ORD(E.Ampersand);
  Dereference: T = ORD(E.Dereference);
  Dot: T = ORD(E.Dot);
  Bra: T = ORD(E.Bra);
  Ket: T = ORD(E.Ket);
  CurlyBra: T = ORD(E.CurlyBra);
  CurlyKet: T = ORD(E.CurlyKet);
  SquareBra: T = ORD(E.SquareBra);
  SquareKet: T = ORD(E.SquareKet);
  Becomes: T = ORD(E.Becomes);
  Semicolon: T = ORD(E.Semicolon);
  Comma: T = ORD(E.Comma);
  Colon: T = ORD(E.Colon);
  Bar: T = ORD(E.Bar);
  Range: T = ORD(E.Range);
  Subtype: T = ORD(E.Subtype);
  Implies: T = ORD(E.Implies);
  Void: T = ORD(E.Void);

CONST
  ReservedWordTexts = ARRAY ReservedWord OF TEXT{
      "ADDRESS", "AND", "ANY", "ARRAY", "AS", "BEGIN", "BITS", "BRANDED", 
      "BY", "CASE",  "CONST", "DIV", "DO", "ELSE", "ELSIF", "END", "EVAL", 
      "EXCEPT", "EXCEPTION", "EXIT", "EXPORTS", "EXTENDED", "FINALLY", "FOR", 
      "FROM", "GENERIC", "IF", "IMPORT",
      "IN", "INTEGER", "INTERFACE", "LOCK", "LONGREAL", "LOOP", "METHODS",
      "MOD", "MODULE", "NIL", "NOT", "NULL", "OBJECT", "OF", "OR", 
      "OVERRIDES", "PROCEDURE",
      "RAISE", "RAISES", "READONLY", "REAL", "RECORD", "REF", "REFANY",
      "REPEAT", "RETURN", "REVEAL", "ROOT", "SET", "THEN", "TO", "TRY", "TYPE",
      "TYPECASE", "UNSAFE", "UNTIL", "UNTRACED", "VALUE", "VAR", "WHILE",
      "WITH"};
  Texts = ReservedWordTexts;

  ReservedTokenTexts = ARRAY ReservedToken OF TEXT{
      "+", "-", "*", "/", "=", "#", "<", ">", "<=", ">=", "&", "^", ".",
      "(", ")", "{", "}", "[", "]", ":=", ";", ",", ":", "|", "..", "<:",
      "=>"};

PROCEDURE Token_rep(t: T): M3AST_LX.Token_rep;
(* Return a "Token_rep" corresponding to "t". *)

PROCEDURE Token_repToText(tr: M3AST_LX.Token_rep): TEXT;
(* Returns the corresponding text for "tr" *)

END M3CToken.
