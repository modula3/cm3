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
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)


INTERFACE TextExtras;

IMPORT ASCII;

TYPE T = TEXT;

PROCEDURE CIEqual(t, u: T): BOOLEAN RAISES {};
(* Return TRUE if t and u have the same length and the same
   (case-insensitive) contents. *)

PROCEDURE Compare(t, u: T): INTEGER RAISES {};
(* Result is <, =, > 0 accordingly as t is <, =, > u (ascii sort order) *)

PROCEDURE CICompare(t, u: T): INTEGER RAISES {};
(* Case-insensitive version of Compare. *)

PROCEDURE FindChar(
    t: T;
    ch: CHAR;
    VAR (*inout*) index: CARDINAL)
    : BOOLEAN
    RAISES {};
(*
  Search 't' for 'ch', starting from the 'index'th position.
  If 'index' is initially equal to the length of 't' FALSE is returned and
  'index' is left unchanged.
  It is a checked runtime error if 'index' is initially greater than the
  length of 't'.
  If 'ch' is found then 'index' is set to the corresponding position and
  TRUE is returned. Otherwise, 'index' is set to the length of 't' and FALSE
  is returned.
  Any character---including Nul---may be searched for.
*)

PROCEDURE FindCharSet(
    t: T;
    READONLY charSet: ASCII.Set;
    VAR (* inout *) index: CARDINAL)
    : BOOLEAN
    RAISES {};
(* As FindChar but matches any of the characters in 'charSet'. *)

PROCEDURE FindSub(
  t, sub: T;
  VAR (* inout *) index: CARDINAL): BOOLEAN RAISES {};
(* As FindChar, but matching substring 'sub'. *)

PROCEDURE Extract(
  t: T; fx, tx: CARDINAL): T RAISES {};
(* Equivalent to, but more convenient than, Sub(t, fx, tx-fx), especially
when used with FindXXX; *)

PROCEDURE Join(t1, t2, t3, t4, t5: T := NIL): T RAISES {};
(* Returns the concatenation of all its non NIL arguments. Non NIL arguments
must precede NIL arguments; it is a checked runtime error if they do not. e.g.
if 't4' is non NIL 't1' to 't4' must be non NIL as well.
  It is a checked runtime error if all the arguments are NIL *)

PROCEDURE JoinN(READONLY texts: ARRAY OF TEXT): T RAISES {};
(* Returns the concatenation of all the elements of 'texts'. It is a checked
runtime error if any element of 'texts' is NIL or if 'NUMBER(texts) = 0'  *)

PROCEDURE CIHash(t: T): INTEGER;
(* Case insensitive version of "Text.Hash", for case-insensitive tables. *)


END TextExtras.
