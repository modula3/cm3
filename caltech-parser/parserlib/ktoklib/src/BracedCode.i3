(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE BracedCode;
IMPORT Rd, CharRange;

PROCEDURE FindChar(rd: Rd.T; which: CharRange.T) RAISES {Rd.EndOfFile};
(* advance rd past a character specified in "which", skipping
   nested comments and braces, and quoted chars and strings. *)

PROCEDURE Match(rd: Rd.T): TEXT;
(* on entry: rd positionned following '{' in src.
   on exit: rd positionned following matching '}' in src, or EOF if none.
   return value: the text in between the braces *)

PROCEDURE GetAhead(rd: Rd.T): TEXT;
(* on entry: rd positionned before either "{...}" or other stuff.
   if other stuff, rewind rd to position on entry, and return "".
   if braces, return code within braces. *)

END BracedCode.
