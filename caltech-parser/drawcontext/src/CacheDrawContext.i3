(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: CacheDrawContext.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE CacheDrawContext;
IMPORT DrawContext;
IMPORT VBTDrawContext;
IMPORT TextSubs;
IMPORT Point;
TYPE
  T <: Public;
  Public = DrawContext.T OBJECT METHODS
    recall(to: DrawContext.T;
           where := Point.Origin;
           subs: TextSubs.T := NIL);
    (* recall any lines and texts that were drawn into self,
       scaled by ratio res(self)/res(to),
       translated to "where",
       and with text processed according to subs. *)

    diffRecall(to: VBTDrawContext.T;
               where := Point.Origin;
               oldSubs, newSubs: TextSubs.T := NIL);
    (* erase and repaint any text that changed. *)
  END;
END CacheDrawContext.
