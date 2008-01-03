(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jan  7 10:54:49 PST 1993 by muller                   *)
(*                                                                           *)
(* The above lines, and this line, and all lines up 
   to the first blank line, will be skipped by M3ToTex *)

(* The "Example" interface shows how to use "m3totex". *)

INTERFACE Example;

TYPE T <: ROOT;

(* An "Example.T" is an opaque object type. *)

PROCEDURE P(t: T; VAR (*OUT*) n: INTEGER);
(* Replace "t" by its hyperbolic completion and
   store its specular index in "n". *)

(* Comments that do not start in the first column are
   typeset like program text (for example, the comment
   "OUT" in the signature of "P").

   Comments that start in the first column and are not
   preceded by a blank line are indented and slanted.
   They are intended to be used for short summary procedure
   specifications, like the one following the declaration
   of "P".

   Comments that start in the first column and are
   preceded by a blank line are typeset in ordinary
   roman text, except that quoted words are typeset
   like program text (for example, "Example.T"). 

   If you want to include some verbatim material in a comment, precede
   each verbatim line by a vertical-bar and a space.  For example,

| VAR t := NEW(t); n: INTEGER; BEGIN	
|   WHILE n # 0 DO 
|     P(t, n);
|     `reduce the specular index of "t"`.
|   END
| END

Notice that you can escape into roman type by using 
back-quotes. *)

END Example.
