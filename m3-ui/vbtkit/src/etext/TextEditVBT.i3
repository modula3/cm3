(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri May 17 11:55:48 PDT 1996 by mhb         *)
(*      modified on Tue Mar 23 15:47:58 PST 1993 by meehan      *)
(*      modified on Tue Jun 16 13:08:10 PDT 1992 by muller      *)
(*      modified on Thu Jan 31 10:34:05 PST 1991 by brooks      *)
(*      modified on Tue Dec 4 7:01:44 PST 1990 by steveg        *)
(*      modified on Wed May 17 17:06:31 PDT 1989 by gidi        *)
<* PRAGMA LL *>

(* A "TextEditVBT" combines a textport with a scrollbar. *)

INTERFACE TextEditVBT;

IMPORT TextPort, TextPortClass, VBT;

TYPE
  T <: Public;
  Public = Private BRANDED OBJECT
             (* READONLY after init *)
             tp: TextPort.T := NIL;
             sb: Scrollbar  := NIL;
           METHODS
             <* LL.sup = VBT.mu *>
             init (scrollable := TRUE): T
           END;
  Private <: VBT.T;
  Scrollbar <: TextPortClass.Scrollbar;

(* The call "v.init()" initializes "v" as a "TextEditVBT" and returns
   "v". If the textport, "v.tp", is "NIL", then a new textport will be
   allocated, initialized (with default parameters), and assigned to
   "v.tp". If "scrollable" is "FALSE", then there will be no
   scrollbar.  If "scrollable" is "TRUE" but "v.sb" is "NIL", then a
   new scrollbar will be allocated, initialized as a vertical
   scrollbar with the textport's color scheme, and assigned to "v.sb".

   If "v" is scrollable, then the default layout will contain a
   scrollbar, either on the leftside or rightside of the textport,
   depending on the value of the "SCROLLBARLOC" environment variable
   (see the "VBTKitEnv" interface for details). *)

END TextEditVBT.



