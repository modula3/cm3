(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* PathPrivate.def, by Mark Kent Fri Feb 20 22:10:03 1987 *)
(* Last modified on Fri Apr  1 15:09:15 PST 1994 by heydon  *)
(*      modified on Tue Feb 11 16:22:28 PST 1992 by muller  *)
(*      modified on Wed Sep  4 19:22:22 PDT 1991 by gnelson *)
(*      modified on Mon Aug  3 13:06:30 1987 by mkent *)
<*PRAGMA LL*>

(* This interface defines the representation of a Path.T. *)

INTERFACE PathPrivate;

IMPORT Point, Path, Word;

TYPE
  ArrayRef = REF ARRAY OF Word.T;

REVEAL Path.T = BRANDED OBJECT
    points: ArrayRef := NIL; (* data for the path *)
    start, next, current, end: ADDRESS := NIL;
    curveCount: CARDINAL := 0
  END;

(* The current data for the path is packed in [start^..next^),
   and the space [next^..end^) is available for additional segments.

   If "points # NIL", then the space between start and end is 
   contained in the array points^.  If "points = NIL", then
   the path is read-only.
   
   The value of "current" is the record for the "MoveTo" that
   started the last subpath, if it is open, and equal to "next" 
   otherwise.

   The value "curveCount" is the number of Bezier curves on the
   path. *)

TYPE Lock = UNTRACED REF Word.T;

PROCEDURE Freeze(path: Path.T): Lock;
PROCEDURE Thaw(l: Lock);

(* To read the address fields of a path, you must first call "Freeze", 
   preventing the allocator from moving the data in points.  You must
   then call "Thaw", passing the result of the call to "Freeze" when you
   no longer need the pointers to be maintained correctly. *)

TYPE
  Type = {Curve, Line, Move, Close};
  Ttype = BITS Word.Size FOR Type;
  PCurve = UNTRACED REF CurveRec;
  CurveRec = RECORD ct: Ttype; p, q, r: Point.T; END;
  PLine = UNTRACED REF LineRec;
  LineRec = RECORD ct: Ttype; p: Point.T END;

  (* in a "PCurve", the "ct" field is "Curve".  in a "PLIne", the "ct"
     field is either Line, Move, or Close.   If "ct" is "Close",
     then "p" is the startpoint of the subpath that it closes.  *)

  
END PathPrivate.
