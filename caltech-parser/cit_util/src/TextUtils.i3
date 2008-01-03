(*                                                                           *)
(*  TextUtils.i3                                                             *)
(*                                                                           *)
(*  Some useful text processing routines for the PL1 compiler.               *)
(*                                                                           *)
(*  Copyright (c) 2000 California Institute of Technology                    *)
(*  All rights reserved.                                                     *)
(*  Department of Computer Science                                           *)
(*  Pasadena, CA 91125.                                                      *)
(*                                                                           *)
(*  Author: Mika Nystrom <mika@cs.caltech.edu>                               *)
(*                                                                           *)
(*  Permission to use, copy, modify, and distribute this software            *)
(*  and its documentation for any purpose and without fee is hereby          *)
(*  granted, provided that the above copyright notice appear in all          *)
(*  copies. The California Institute of Technology makes no representations  *)
(*  about the suitability of this software for any purpose. It is            *)
(*  provided "as is" without express or implied warranty. Export of this     *)
(*  software outside of the United States of America may require an          *)
(*  export license.                                                          *)
(*                                                                           *)
(* $Id: TextUtils.i3,v 1.2 2001-09-19 14:07:43 wagner Exp $ *)
INTERFACE TextUtils;
IMPORT TextList, TextSet;

(* replace every occurrence of "old" by "new" in "in" *)
PROCEDURE Replace(in, old, new : TEXT) : TEXT;
PROCEDURE ReplaceChar(in : TEXT; old, new : CHAR) : TEXT;
PROCEDURE CountCharOccurences(in: TEXT; c: CHAR): CARDINAL;

(* find first occurrence of sub in in *)
PROCEDURE FindSub(in, sub : TEXT; VAR pos : CARDINAL; start := 0) : BOOLEAN;

(* have substr? *)
PROCEDURE HaveSub(in, sub : TEXT) : BOOLEAN;
PROCEDURE HavePrefix(in, prefix: TEXT): BOOLEAN;
PROCEDURE HaveSuffix(in, suffix: TEXT): BOOLEAN;
PROCEDURE RemoveSuffix(in, suffix: TEXT): TEXT;

PROCEDURE InfixFormat(sep : TEXT; list : TextList.T; ignoreNulls := FALSE) : TEXT;

PROCEDURE Pluralize(noun : TEXT; count : INTEGER; 
                    ending := "s"; printNum := TRUE) : TEXT ;

PROCEDURE ListToSet(list : TextList.T) : TextSet.T;
PROCEDURE SetToList(set : TextSet.T) : TextList.T;


END TextUtils.
