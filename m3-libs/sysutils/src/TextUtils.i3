(* Copyright 1999-2002 elego Software Solutions GmbH, Berlin, Germany.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $Id: TextUtils.i3,v 1.5 2009-07-24 05:45:31 jkrell Exp $ *)

(*---------------------------------------------------------------------------*)
INTERFACE TextUtils;

IMPORT ASCII, TextSeq, TextTextTbl;

(*---------------------------------------------------------------------------*)
EXCEPTION Error(TEXT);

(*---------------------------------------------------------------------------*)
PROCEDURE SkipLeft(t : TEXT; s : ASCII.Set := ASCII.Spaces) : TEXT;
  (* return a text from which all leading elements of `s' 
     have been stripped *)

(*---------------------------------------------------------------------------*)
PROCEDURE SkipRight(t : TEXT; s : ASCII.Set := ASCII.Spaces) : TEXT;
  (* return a text from which all trailing elements of `s' 
     have been stripped *)

(*---------------------------------------------------------------------------*)
PROCEDURE Compress(t : TEXT; s : ASCII.Set := ASCII.Spaces) : TEXT;
  (* return a text from which all leading and trailing elements of `s' 
     have been stripped *)

(*---------------------------------------------------------------------------*)
PROCEDURE SubstChar(t : TEXT; a, b : CHAR) : TEXT;
  (* return a text in which each occurrence of `a' has been 
     replaced by `b' *)

(*---------------------------------------------------------------------------*)
PROCEDURE SubstChars(t : TEXT; READONLY a, b : ARRAY OF CHAR) : TEXT;
  (* return a text in which each occurrence of an element of `a' has been 
     replaced by the corresponding element of `b' *)

(*---------------------------------------------------------------------------*)
PROCEDURE Substitute(READONLY t, a, b : TEXT; times := 0) : TEXT;
  (* return a text in which `times' occurences of `a' have been
     substituted by `b'. `times' = 0 means all occurences. *)

(*---------------------------------------------------------------------------*)
PROCEDURE RemoveChars(t : TEXT; s : ASCII.Set := ASCII.Spaces) : TEXT;
  (* return a text from which all characters of `s' have been removed *)

(*---------------------------------------------------------------------------*)
PROCEDURE Squeeze(READONLY t : TEXT) : TEXT;
  (* return a text without multiple blank lines *)

(*---------------------------------------------------------------------------*)
PROCEDURE MemberOfTextSeq(tl : TextSeq.T; elem : TEXT) : BOOLEAN;
  (* <=> `elem' is member of `tl'. *)

(*--------------------------------------------------------------------------*)
PROCEDURE TextSeqToText(seq : TextSeq.T; sep := " "; maxCol := 0;
                        contToken := "\\\n") : TEXT;
  (* Return a text containing the concatenation of all elements of
     the text sequence `seq'. Break lines before `maxCol' if it is
     > 0, use continuation sequence `contToken' at break. *)

(*--------------------------------------------------------------------------*)
PROCEDURE TextSeqToArray(seq : TextSeq.T) : REF ARRAY OF TEXT;

(*--------------------------------------------------------------------------*)
PROCEDURE SubstEnvVars(READONLY t : TEXT; 
                       env : TextTextTbl.T := NIL) : TEXT;
  (* Return the text `t' in which all environment variables of the
     form ${name} or $name have been substituted by their values
     in the current environment. 
  *)

(*--------------------------------------------------------------------------*)
PROCEDURE AddPrefix(seq : TextSeq.T; prefix : TEXT) : TextSeq.T;
  (* Add `prefix' to every element of the sequence. The returned 
     sequence is a new object. *)

(*--------------------------------------------------------------------------*)
PROCEDURE AddSuffix(seq : TextSeq.T; suffix : TEXT) : TextSeq.T;
  (* Add `suffix' to every element of the sequence. The returned 
     sequence is a new object.*)

(*--------------------------------------------------------------------------*)
PROCEDURE Split(text : TEXT; sep : TEXT) : TextSeq.T;
  (* Split `text' into elements at each occurence of `sep'. Discard
     `sep' in the returned sequence. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Tokenize(text : TEXT; sepchars := ASCII.Spaces;
                   squeeze := TRUE) : TextSeq.T;
  (* Split `text' into elements at each occurence of a separating
     character from `sepchars'. Discard all `sepchars' in the returned
     sequence. Do not return empty elements if `sqeueeze' is TRUE. *)  

(*--------------------------------------------------------------------------*)
PROCEDURE Lower(text : TEXT) : TEXT;
  (* Return a text where all alphas are in lower case. *)

(*--------------------------------------------------------------------------*)
PROCEDURE Upper(text : TEXT) : TEXT;
  (* Return a text where all alphas are in upper case. *)

(*---------------------------------------------------------------------------*)
PROCEDURE SubstituteVariables(t : TEXT; parameters : TextTextTbl.T) : TEXT
  RAISES {Error};
  (* Substitute all variable of the form {:name}, {?name}, {!name}
     within t an d return the result. '?' denotes an optional variable,
     ':' denotes a mandatory variable, and '!' denotes a mandatory
     variable whose value must not be empty.
  *)

(*---------------------------------------------------------------------------*)
PROCEDURE Pos(READONLY s, t : TEXT; caseSensitive := TRUE) : INTEGER;
  (* <=> Position of Text `t' in text `s'. Returns -1 if not found. *)

(*---------------------------------------------------------------------------*)
PROCEDURE Contains(READONLY s, t : TEXT; caseSensitive := TRUE) : BOOLEAN;
  (* <=> Text `s' contains text `t'. *)

(*---------------------------------------------------------------------------*)
PROCEDURE CountChar(s: TEXT; ch: CHAR; caseSensitive := TRUE) : INTEGER;
  (* the number of occurences of ch in s *)

(*---------------------------------------------------------------------------*)
PROCEDURE StartsWith(s, t: TEXT; caseSensitive := TRUE) : BOOLEAN;
  (* does s start with t *)

(*---------------------------------------------------------------------------*)
PROCEDURE EndsWith(s, t: TEXT; caseSensitive := TRUE) : BOOLEAN;
  (* does s end with t *)

(*---------------------------------------------------------------------------*)
PROCEDURE BoolVal(READONLY t : TEXT; default := FALSE) : BOOLEAN;
  (* Evaluate the text t to a boolean value. For "yes", "true", "1", and
     "on", TRUE is returned, for "no", "false", "0", and "off", FALSE
     is returned, otherwise the default value. *)

(*---------------------------------------------------------------------------*)
PROCEDURE Elem_Compare (a, b: TEXT): [-1..1];

(*---------------------------------------------------------------------------*)
PROCEDURE Sort (VAR a: ARRAY OF TEXT;  cmp := Elem_Compare);
(* quick sort from libm3 for convenience *)

END TextUtils.
