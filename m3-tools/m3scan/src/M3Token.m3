(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: M3Token.m3                                            *)
(* Last modified on Mon Apr 25 15:56:37 PDT 1994 by kalsow     *)
(*      modified on Sat Mar 16 00:31:12 1991 by muller         *)

MODULE M3Token;

BEGIN
  name [T.EOF]            := "**EOF**";
  name [T.Comment]        := "**COMMENT**";
  name [T.Error]          := "**ERROR**";
  name [T.Ident]          := "<id>";
  name [T.Card_const]     := "<cardinal>";
  name [T.Real_const]     := "<real>";
  name [T.Longreal_const] := "<longreal>";
  name [T.Extended_const] := "<extended>";
  name [T.Char_const]     := "<char>";
  name [T.Text_const]     := "<text>";
  name [T.Plus]           := "+";
  name [T.Minus]          := "-";
  name [T.Asterisk]       := "*";
  name [T.Slash]          := "/";
  name [T.Assign]         := ":=";
  name [T.Ampersand]      := "&";
  name [T.Dot]            := ".";
  name [T.Comma]          := ",";
  name [T.Semi]           := ";";
  name [T.L_paren]        := "(";
  name [T.L_bracket]      := "[";
  name [T.L_brace]        := "{";
  name [T.Arrow]          := "^";
  name [T.Equal]          := "=";
  name [T.Sharp]          := "#";
  name [T.Less]           := "<";
  name [T.Greater]        := ">";
  name [T.Ls_equal]       := "<=";
  name [T.Gr_equal]       := ">=";
  name [T.Dot_dot]        := "..";
  name [T.Colon]          := ":";
  name [T.R_paren]        := ")";
  name [T.R_bracket]      := "]";
  name [T.R_brace]        := "}";
  name [T.Bar]            := "|";
  name [T.Subtype]        := "<:";
  name [T.Implies]        := "=>";
  name [T.Begin_pragma]   := "<*";
  name [T.End_pragma]     := "*>";
 
  name [T.And]            := "AND";
  name [T.Any]            := "ANY";
  name [T.Array]          := "ARRAY";
  name [T.As]             := "AS";
  name [T.Begin]          := "BEGIN";
  name [T.Bits]           := "BITS";
  name [T.Branded]        := "BRANDED";
  name [T.By]             := "BY";
  name [T.Case]           := "CASE";
  name [T.Const]          := "CONST";
  name [T.Div]            := "DIV";
  name [T.Do]             := "DO";
  name [T.Else]           := "ELSE";
  name [T.Elsif]          := "ELSIF";
  name [T.End]            := "END";
  name [T.Except]         := "EXCEPT";
  name [T.Exception]      := "EXCEPTION";
  name [T.Exit]           := "EXIT";
  name [T.Exports]        := "EXPORTS";
  name [T.Eval]           := "EVAL";
  name [T.Finally]        := "FINALLY";
  name [T.For]            := "FOR";
  name [T.From]           := "FROM";
  name [T.Generic]        := "GENERIC";
  name [T.If]             := "IF";
  name [T.Import]         := "IMPORT";
  name [T.In]             := "IN";
  name [T.Interface]      := "INTERFACE";
  name [T.Lock]           := "LOCK";
  name [T.Loop]           := "LOOP";
  name [T.Methods]        := "METHODS";
  name [T.Mod]            := "MOD";
  name [T.Module]         := "MODULE";
  name [T.Not]            := "NOT";
  name [T.Object]         := "OBJECT";
  name [T.Of]             := "OF";
  name [T.Or]             := "OR";
  name [T.Overrides]      := "OVERRIDES";
  name [T.Procedure]      := "PROCEDURE";
  name [T.Raise]          := "RAISE";
  name [T.Raises]         := "RAISES";
  name [T.Readonly]       := "READONLY";
  name [T.Record]         := "RECORD";
  name [T.Ref]            := "REF";
  name [T.Repeat]         := "REPEAT";
  name [T.Return]         := "RETURN";
  name [T.Reveal]         := "REVEAL";
  name [T.Set]            := "SET";
  name [T.Then]           := "THEN";
  name [T.To]             := "TO";
  name [T.Try]            := "TRY";
  name [T.Type]           := "TYPE";
  name [T.Typecase]       := "TYPECASE";
  name [T.Unsafe]         := "UNSAFE";
  name [T.Until]          := "UNTIL";
  name [T.Untraced]       := "UNTRACED";
  name [T.Value]          := "VALUE";
  name [T.Var]            := "VAR";
  name [T.While]          := "WHILE";
  name [T.With]           := "WITH";
END M3Token.
