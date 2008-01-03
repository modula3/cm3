(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Figure.i3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

INTERFACE Figure;
IMPORT VBT;
IMPORT Rd;
IMPORT Rect;
IMPORT Axis;
IMPORT DrawContext;
IMPORT CacheDrawContext;
IMPORT TextSubs;
IMPORT Point;
IMPORT Region;

TYPE
  T <: ROOT;


(* constructing "Figure.T"s *)

PROCEDURE FromPS(rd: Rd.T; captureResDPI := 720): T;
(* construct a "Figure.T" from an "eps" file *)

PROCEDURE FromCache(cache: CacheDrawContext.T; static := TRUE): T;
(* construct a "Figure.T" from cached drawing commands.
   if static is TRUE, assumes that cache will never change. *)

PROCEDURE FromFigure(fig: T; where := Point.Origin;
                     subs: TextSubs.T := NIL): T;
(* clone a "Figure.T". Optionally translating to "where",
   and replacing some text according to "subs". *)

PROCEDURE FromText(text: TEXT := NIL; size := 12): T;
(* construct a "Figure.T" containing "TEXT" "text", of size "size",
   attached left-justified at the origin.
   If text is NIL, return an empty figure. *)
   


(* using "Figure.T"s *)

PROCEDURE Plot(fig: T; in: DrawContext.T;
               where := Point.Origin; subs: TextSubs.T := NIL);
(* plot "fig" into "in", possibly tanslating its origin to "where",
   and optionally replacing some text. *)

PROCEDURE ToCache(fig: T; static := TRUE): CacheDrawContext.T;
(* if static is TRUE, assumes that returned cache will never change. *)

PROCEDURE Size(fig: T; in: DrawContext.T;
               axis := Axis.T.Hor;
               subs: TextSubs.T := NIL): CARDINAL;
(* return size along axis "axis" of figure "fig", were it to be plotted
   in "in" with substitutions "subs". *)

<*OBSOLETE*>PROCEDURE VBTSize(fig: T; vbt: VBT.T;
                  axis := Axis.T.Hor;
                  subs: TextSubs.T := NIL): CARDINAL;
(* return size along axis "axis" of figure "fig", were it to be plotted
   in a new VBTDrawContext for "vbt" with substitutions "subs". *)

PROCEDURE BoundRect(fig: T; in: DrawContext.T;
                    where := Point.Origin;
                    subs: TextSubs.T := NIL): Rect.T;
(* like "Size", but return the whole rectangle *)

PROCEDURE BoundRegion(fig: T; in: DrawContext.T;
                      where := Point.Origin;
                      subs: TextSubs.T := NIL): Region.T;
(* like "BoundRect", but return a possibly more accurate region. *)

END Figure.

