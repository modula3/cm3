(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Tue May 11 17:05:11 PDT 1993 by swart      *)
(*      modified on Mon Nov 18 21:32:21 PST 1991 by gnelson    *)
(*      modified on Thu Nov  2 18:28:31 1989 by muller         *)
(*      modified on Mon Oct  2 09:13:50 1989 by kalsow         *)
(*      modified on Fri Jun  3 16:12:45 PDT 1988 by glassman   *)
(*      modified on Tue Mar  3 18:14:23 PST 1987 by luca       *)

INTERFACE Axis;

IMPORT Word;

(* "Axis.T.Hor" and "Axis.T.Ver" are Trestle's names for the horizontal
   and vertical axes.  "Axis.Other" exchanges "Hor" and "Ver".

   Index: screen, coordinate system *)

TYPE T = {Hor, Ver};

CONST Other = ARRAY T OF T {T.Ver, T.Hor};

PROCEDURE Compare (a, b: T): [-1 .. 1];
(* == RETURN (a - b) *)

PROCEDURE Equal (a, b: T): BOOLEAN;
(* == RETURN (a = b) *)

PROCEDURE Hash (a: T): Word.T;
(* == RETURN ORD (a) *)

END Axis.
